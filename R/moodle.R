#' @title Extract cookies from a Chrome browser tab
#'
#' @param tab A [chromote::ChromoteSession()] object
#'
#' @importFrom stats setNames
#' @importFrom httr set_cookies
#'
#' @return an [httr::httr] requests object with all fields NULL except the options$cookie field
#' @export
extract_cookies <- function(tab) {
  # Copy the cookies associated from the web browser associated with moodle.smith.edu
  # Which we can use to authenticate HTTP requests we're going to make in a moment
  cookies <- tab$Network$getCookies()
  httr_cookies <- stats::setNames(
    object = lapply(cookies[[1]], `[[`, "value"),
    nm = lapply(cookies[[1]], `[[`, "name")
    ) |>
    unlist() |>
    httr::set_cookies(.cookies = _)

  return(httr_cookies)
}

#' @title Extract the Moodle 'sesskey' string
#' @description
#' Whenever you log into Moodle, Moodle sets a "session key" that is expected to be included with POST and GET requests to the server. Given a Chromote session object, This function retrieves the current session key from the browser's JavaScript environment.
#'
#' @param tab A [chromote::ChromoteSession()] object obtained via the [open_moodle()] function
#'
#' @return A scalar character vector representing the Moodle session key
#' @export
extract_moodle_session_key <- function(tab) {
  tab$Runtime$evaluate("M.cfg.sesskey")$result$value
}

#' @title Retrieve the browser's user agent string
#'
#' @inheritParams extract_moodle_session_key
#'
#' @return A scalar character vector holding the browser's user agent string
#' @export
get_user_agent <- function(tab) {
  tab$Runtime$evaluate(expression = "navigator.userAgent")$result$value
}

get_page_body <- function(tab) {
  dom_nodeID <- tab$DOM$getDocument()$root$nodeId
  html_body <- tab$DOM$getOuterHTML(tab$DOM$querySelector(dom_nodeID, "body")$nodeId)$outerHTML
  rvest::read_html(html_body)
}

wait_for_element <- function(tab, elem, timeout = 10) {

  js <- paste("document.querySelector('", elem, "');")
  x <- tab$Runtime$evaluate(js)

  deadline <- Sys.time() + timeout
  while(is.null(x$result$className) && Sys.time() < deadline)  {
    x <- tab$Runtime$evaluate(js)
  }

  if (is.null(x$result$className)) {
    x <- NULL
  }
  return(invisible(x))
}

wait_for_port <- function(host, port, timeout = 10) {
  deadline <- Sys.time() + timeout

  while (Sys.time() < deadline) {
    con <- suppressWarnings(
      try(
        socketConnection(host, port, open = "r+", timeout = 1),
        silent = TRUE
      )
    )

    if (!inherits(con, "try-error")) {
      close(con)
      return(invisible(TRUE))
    }

    Sys.sleep(0.05)
  }

  stop("Timed out waiting for port ", port)
}

is_duo_prompt_url <- function(url) {
  grepl("^https://.*\\.duosecurity\\.com/.*prompt.*$", url)
}


#' @title Open Moodle in Chrome/Chromium
#' @description
#' This function launches a (possibly headless) Chrome/Chromium instance, and opens the dashboard
#' page at the given Moodle instance.
#'
#' @param site_url A scalar character vector holding the root URL for your Moodle instance
#' @param graphical A scalar logical vector. If `TRUE` (the default), a graphical Chrome/Chromium window is opened for the user to log in to Moodle. If `FALSE`, a headless Chrome/Chromium instance is launched. The `username` and `password` arguments must be supplied if `graphical = FALSE`.
#' @param username A scalar character vector holding the username you log in to Moodle with
#' @param password A scalar character vector holding the password you log in to Moodle with
#' @param org  A scalar character vector holding the name of the organization you belong to. Used when logging in to a Moodle page the has federated SSO.
#' @param twoFA A list of named character vectors. The name of each element should describe a 2FA provider and each element should describe a 2FA method. See the Details section for  more information
#' @param timeout Scalar numeric vector. Describes the number of second to wait for network events (e.g., for a web page to load).
#'
#' @details
#' Currently moodleR only supports Duo as a two-factor authentication provider, and only supports
#' authentication via OTP codes, hardware security key, or phone call. Thus the `twoFA` argument
#' must be one of:
#'
#' - `list(duo = "otp")`
#' - `list(duo = "key")`
#' - `list(duo = "call")`
#'
#' @return A [MoodlePage] object
#'
#' @importFrom chromote ChromoteSession
#' @importFrom later run_now
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' tab <- open_moodle(
#'   site_url = "https://moodle.smith.edu",
#'   graphical = FALSE,
#'   username = "whopper",
#'   password = "CorrectHorseBatteryStaple",
#'   org = "smith",
#'   twoFA = list(duo = "push")
#' )
#' tab$view() # opens a debugging window showing the "headless" tab
#' }
#'
#' @export
open_moodle <- function(
    site_url,
    graphical = TRUE,
    username = NULL,
    password = NULL,
    org = NULL,
    twoFA = list(),
    timeout = 10
  ) {

  #### Check 2FA provider/method validity ####

  # Map argument names to "official" Duo names
  duo_method_map <- c(
    "push" = "Duo push",
    "key" = "Security key",
    "call" = "Phone call"
    )

  names(twoFA) <- tolower(names(twoFA))

  if ("duo" %in% names(twoFA)) {
    if (!twoFA$duo %in% names(duo_method_map)) {
      cli::cli_abort("{.str twoFA$duo} is not a supported 2FA method.")
    }
    duo <- duo_method_map[twoFA$duo]
  }

  # the page we're trying to end up at
  dashboard_url <- file.path(site_url, "my/")

  tab <- MoodlePage$new(site_url = "https://moodle.smith.edu")

  # Moodle won't respond to bots/headless browsers, so edit the user agent string
  # in order to larp as regular chrome
  user_agent_result <- get_user_agent(tab)
  tab$Network$setUserAgentOverride(userAgent = sub("Headless", "", user_agent_result))

  if (graphical) {
    login_cookies <- graphical_moodle_login(site_url)
    tab$Network$setCookies(cookies = login_cookies)
  }

  tab$go_to("https://moodle.smith.edu/my/")
  dest_url <- tab$Page$getFrameTree()$frameTree$frame$url

  # If we get the dashboard, we're logged in and ready to go
  if (dest_url == dashboard_url) {
    return(tab)
  }

  # Otherwise, prepare for log in
  message("Preparing to log in")

  if (!grepl("login", dest_url)) {
    stop("Reached an uncogonized endpoint (", url, "), login attempted cancelled.")
  }

  login_links <- get_page_body(tab) |>
    rvest::html_elements(".mx-auto a") |>
    rvest::html_attr("href") |>
    unlist()

  org_login_link <- login_links |>
    purrr::map(\(x) httr::parse_url(x)$query$idpentityid) |>
    purrr::map_if(is.null, \(x) "") |>
    unlist() |>
    grepl(org, x = _)

  if (!any(org_login_link)) {
    stop("No login link for ", org, found)
  }

  message(paste0("Logging in at ", login_links[org_login_link]))
  tab$go_to(login_links[org_login_link])
  dest_url <- tab$Page$getFrameTree()$frameTree$frame$url

  if (dest_url == dashboard_url) {
    return(tab)
  }

  if (!grepl("login", dest_url)) {
    stop("Reached an uncogonized endpoint (", dest_url, "), login attempted cancelled.")
  }

  creds_accepted <- NULL
  check_login_response <- function(value) {

    is_login_page <- startsWith(
      x = value$response$url,
      "https://login.smith.edu/idp/profile/SAML2/Redirect/SSO"
    )

    if (is_login_page && value$response$status == 200 && value$type == "Document") {
      creds_accepted <<- FALSE
    }

    is_duo_page <- grepl("^https://.*\\.duosecurity.com.*", x = value$response$url)
    if (is_duo_page && value$response$status == 200 && value$type == "Document") {
      creds_accepted <<- TRUE
    }

    if (value$response$url == dashboard_url) {
      creds_accepted <<- TRUE
    }

  }

  cancel_cb <- tab$Network$responseReceived(wait_ = FALSE, callback_ = check_login_response)

  wait_for_element(tab, "#username")
  tab$Runtime$evaluate(paste0(
    "var input = document.getElementById('username');",
    "input.value = '", username, "';"
  ))
  wait_for_element(tab, "#password")
  tab$Runtime$evaluate(paste0(
    "var input = document.getElementById('password');",
    "input.value = '", password, "';"
  ))

  wait_for_element(tab, 'button[name="_eventId_proceed"]')
  tab$Runtime$evaluate("document.getElementsByName('_eventId_proceed')[0].click();")

  wait_until <- Sys.time() + timeout
  while(is.null(creds_accepted) && Sys.time() <= wait_until) {
    later::run_now(loop = tab$get_child_loop())
  }
  # dereference callback
  cancel_cb()

  if (is.null(creds_accepted)) {
    tab$close()
    stop("Timed out waiting for login response.")
  }

  if (!creds_accepted) {
    tab$close()
    stop("Username/password combination rejected.")
  }

  message("Credentials accepted")

  page_url <- tab$DOM$getDocument()$root$documentURL
  wait_until <- Sys.time() + timeout
  while (!(page_url == dashboard_url || is_duo_prompt_url(page_url) ) && Sys.time() < wait_until) {
    Sys.sleep(.25)
    page_url <- tab$DOM$getDocument()$root$documentURL
    is_duo_prompt <- is_duo_prompt_url(page_url)
  }

  if (!(page_url == dashboard_url || is_duo_prompt_url(page_url) ) ) {
    browser()
    stop("Timed out waiting for Duo Prompt Page")
  }

  if (page_url == dashboard_url) {
    return(tab)
  }

  message("Waiting for 2FA options link")
  btn_query <- '[...document.querySelectorAll("button")].find(b => b.textContent.trim() === "Other options")'
  btn <- tab$Runtime$evaluate(btn_query)
  wait_until <- Sys.time() + timeout
  while (Sys.time() < wait_until && btn$result$type == "undefined") {
    btn <- tab$Runtime$evaluate(btn_query)
  }

  if (btn$result$type == "undefined") {
    stop("Could not find link to 2FA options page")
  }

  message("Found for 2FA options page link")
  tab$Runtime$evaluate(paste0(btn_query, ".click();"))

  x <- wait_for_element(tab, ".all-auth-methods-list .method-label")
  labels <- get_page_body(tab) |>
    rvest::html_elements(".all-auth-methods-list .method-label") |>
    rvest::html_text() |>
    tolower()

  duo_method_index <- which(labels == tolower(duo)) - 1 # JS starts at 0
  wait_for_element(tab, ".auth-method-link")
  tab$Runtime$evaluate(
    paste0(
      "document.getElementsByClassName('auth-method-link')[",
      duo_method_index,
      "].click();"
    )
  )

  if (duo == "Duo push") {

    wait_for_element(tab, "span.code-text")
    code <- get_page_body(tab) |>
      rvest::html_elements("span.code-text") |>
      rvest::html_text()

    twoFA_option <- paste("Enter", code, "in the Duo mobile app")

  } else {

    twoFA_option <- get_page_body(tab) |>
      rvest::html_elements("h1") |>
      rvest::html_text()
  }

  if (tolower(twoFA_option) == "something went wrong") {
    stop("Something went wrong with 2FA, try again")
  }

  cli::cli_alert("{twoFA_option} to continue authentication")

  page_url <- tab$DOM$getDocument()$root$documentURL

  btn_query <- 'Array.from(document.querySelectorAll("button")).find(b => b.textContent.trim() === "No, other people use this device")'

  while (page_url != dashboard_url) {

    # We don't want to use wait_for_element() here because if the browser is trusted, the
    # "do you want to trust?" menu won't ever show up. But the page URL doesn't change when it
    # shows up, so we can't tell apart the "still waiting for course page to load" situation from
    # the "do you want to trust?" situation based on URL.
    # So we'll just poll the page for it as long as we're not yet on the course page.

    btn <- tab$Runtime$evaluate(btn_query)

    if (btn$result$type != "undefined") {
      tab$Runtime$evaluate(paste0(btn_query, ".click();"))
    }

    Sys.sleep(.2)
    page_url <- tab$DOM$getDocument()$root$documentURL
  }

  return(tab)
}

#' @importFrom processx process
#' @importFrom chromote find_chrome ChromeRemote Chromote
graphical_moodle_login <- function(site_url) {

  # Open Chrome
  p <- processx::process$new(
    command = chromote::find_chrome(),
    args = c("--remote-debugging-address=127.0.0.1", "--remote-debugging-port=9222")
  )

    # Wait for debugging port to be ready
    deadline <- Sys.time() + 10
    ready <- FALSE

    while (!ready && Sys.time() < deadline) {
      con <- suppressWarnings(
        try(
          socketConnection("127.0.0.1", 9222, open = "r+", timeout = 1),
          silent = TRUE
        )
      )

      if (!inherits(con, "try-error")) {
        close(con)
        ready <- TRUE
      }

      Sys.sleep(0.05)
    }

  if (!ready) {
    stop("Timed out waiting for port ", port)
  }

  r <- Chromote$new(browser = ChromeRemote$new(host = "127.0.0.1", port = 9222))

  first_id <- function(browser) {
    ts <- browser$Target$getTargets()$targetInfos
    stopifnot(length(ts) > 0)
    target_types <- vapply(ts, FUN = `[[`, FUN.VALUE = character(1), "type")
    first_tab <- which(target_types == "page")[1]
    ts[[first_tab]]$targetId
  }

  tab <- ChromoteSession$new(parent = r, targetId = first_id(r))

  dashboard_url <- file.path(site_url, "my/")
  tab$Page$navigate(dashboard_url)

  # Since the user may need to log in, wait until we actually get to the Moodle
  # page to continue
  while (tab$DOM$getDocument()$root$documentURL != dashboard_url) {
    Sys.sleep(.25)
  }

  # return login cookies
  login_cookies <- tab$Network$getCookies()$cookies

  tab$close()

  return(login_cookies)
}


#' @title Create a new section on a Moodle page
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET POST content content_type_json
#' @importFrom rvest html_text2 html_attr html_elements
#'
#' @param tab A [MoodlePage()] object obtained via the [open_moodle()] function
#' @param section_name Scalar character vector giving the name of the new section
#'
#' @return An (invisible) [httr::response] object containing the response from Moodle's core_courseformat_update_course endpoint
#' @export
create_new_section <- function(tab, section_name) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  # Get the HTML for the main Moodle course page
  URL <- paste0(tab$site_url, "/course/view.php?id=", course_id)
  main_page <- httr::GET(URL, cookies, httr::user_agent(UA))

  # Scrape the HTML to find the number and ID value for each section on the page
  # We need to find the number and ID for the *last* section, so we can add one below
  sections <- httr::content(main_page) |>
    rvest::html_elements("h3")

  section_names <- sections |>
    rvest::html_text2()

  section_numbers <- sections |>
    rvest::html_attr("data-number")

  section_moodle_ids <- sections |>
    rvest::html_attr("data-id")

  # Adding a new section is a two-step process. First, tell Moodle "Give me a new section"
  course_info <- httr::POST(
    paste0(
      tab$site_url,
      "/lib/ajax/service.php?sesskey=",
      sessionkey,
      "&info=core_courseformat_update_course"
    ),
    encode = "raw",
    content_type_json(),
    body = paste0(
      '[{"index":0,"methodname":"core_courseformat_update_course","args":{"action":"section_add","courseid":"',
      course_id,
      '","ids":[],"targetsectionid":',
      section_moodle_ids[length(section_moodle_ids)],
      "}}]"
    ),
    cookies,
    httr::user_agent(UA)
  )

  # After we tell Moodle "Give me a new section", it tells us the ID of this new section
  # and we update all the page information using it
  course_info_json <- httr::content(course_info)[[1]][["data"]] |>
    jsonlite::fromJSON(simplifyVector = FALSE)

  new_section_index <- course_info_json[[1]]$fields$numsections+1
  new_section_id <- course_info_json[[1]]$fields$sectionlist[new_section_index]

  updated_course_info <- httr::POST(
    paste0(
      tab$site_url,
      "/lib/ajax/service.php?sesskey=",
      sessionkey,
      "&info=core_course_edit_section"
    ),
    encode = "raw",
    content_type_json(),
    body = paste0(
      '[{"index":0,"methodname":"core_course_edit_section","args":{"id":"',
      new_section_id,
      '","action":"refresh","sectionreturn":0}}]'
    ),
    cookies,
    httr::user_agent(UA)
  )

  # Sections are just called "Topic 1", "Topic 2"... by default
  # So, we need to edit our new section title to have an informative name.
  # Which is again a two step process
  updated_course_info <- httr::POST(
    paste0(
      tab$site_url,
      "/lib/ajax/service.php?sesskey=",
      sessionkey,
      "&info=core_update_inplace_editable"
    ),
    encode = "raw",
    content_type_json(),
    body = paste0(
      '[{"index":0,"methodname":"core_update_inplace_editable","args":{"itemid":"',
      new_section_id,
      '","component":"format_topics","itemtype":"sectionnamenl","value":"',
      section_name,
      '"}}]'
    ),
    cookies,
    httr::user_agent(UA)
  )

  project_section_info <- httr::POST(
    paste0(
      tab$site_url,
      "/lib/ajax/service.php?sesskey=",
      sessionkey,
      "&info=core_courseformat_update_course"
    ),
    encode = "raw",
    content_type_json(),
    body = paste0(
      '[{"index":0,"methodname":"core_courseformat_update_course","args":{"action":"section_state","courseid":"',
      course_id,
      '","ids":[',
      new_section_id,
      "]}}]"
    ),
    cookies,
    httr::user_agent(UA)
  )
  return(invisible(project_section_info))
}

#' @title Retrieve Moodle student IDs
#' @description
#' Retrieve the internal ID number for each student in a Moodle course.
#'
#' Note that the Moodle "secret ID" is *not* the same as the student ID number you might see in the gradebook when you download their grades, or on their ID card, etc.
#'
#' @inheritParams create_new_section
#'
#' @importFrom xml2 as_list
#' @importFrom rvest read_html
#'
#' @return A data.frame object with the following variables:
#' \describe{
#'   \item{student_id}{The student's internal Moodle ID number (i.e., their "secret" ID)}
#'   \item{student_email}{The student's email address}
#' }
#' @export
get_student_secret_ids <- function(tab) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  participants_page <- httr::GET(
    paste0(tab$site_url, "/user/index.php?id=", course_id),
     cookies,
    httr::user_agent(UA)
   )

  post_body <- paste0(
  '[{"index":0,
     "methodname":"core_table_get_dynamic_table_content",
     "args":{"component":"core_user",
             "handler":"participants",
             "uniqueid":"user-index-participants-', course_id, '",
             "sortdata":[{"sortby":"lastname","sortorder":4}],
             "jointype":2,
             "filters":{"courseid":{"name":"courseid",
                                    "jointype":1,
                                    "values":[', course_id, ']
                                    }
                        },
             "firstinitial":"",
             "lastinitial":"",
             "pagenumber":"1",
             "pagesize":"500",
             "hiddencolumns":[],
             "resetpreferences":false
            }
   }]'
  )

  resp <- httr::POST(
    paste0(tab$site_url, "/lib/ajax/service.php?",
          "sesskey=", sessionkey,
          "&info=core_table_get_dynamic_table_content"
          ),
    encode = "raw",
    content_type_json(),
    body = post_body,
    cookies,
    httr::user_agent(UA)
  )

  student_rows <- httr::content(resp) |>
    {\(x){x[[1]]$data$html}}(x = _) |>
    rvest::read_html() |>
    rvest::html_elements("table#participants tr:not(.emptyrow)")

  student_ids <- student_rows |>
    rvest::html_elements("input") |>
    rvest::html_attr("id") |>
    sub("user", "", x = _)

  student_emails <- student_rows |>
    xml2::as_list() |>
    {\(x) {x[-1]}}() |>
    sapply(\(x) unlist(x[3], recursive = TRUE, use.names = FALSE))

  data.frame(
    student_id = student_ids[-1],
    student_email = student_emails
  )

}

#' @title Retrieve Moodle group names and corresponding IDs
#' @description
#' Retrieve name and corresponding internal ID number for each group in a Moodle course.
#' Note that this function does retrieve the names of members in each group.
#'
#' @inheritParams create_new_section
#'
#' @importFrom dplyr bind_rows mutate rename
#'
#' @return A data.frame object with the following variables:
#' \describe{
#'   \item{group_idnumber}{The groups's internal Moodle ID number}
#'   \item{group_name}{The group's user-facing name in Moodle}
#' }
#'
#' @export
get_group_ids <- function(tab) {

  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  group_main_page <- httr::GET(
    paste0(tab$site_url, "/group/index.php?id=", course_id),
    cookies,
    httr::user_agent(UA)
  )

  group_ids <- httr::content(group_main_page) |>
    rvest::html_element("#groups") |>
    rvest::html_elements("option") |>
    rvest::html_attrs() |>
    dplyr::bind_rows() |>
    dplyr::mutate(title = sub(" \\([0-9]\\)$", "", .data$title)) |>
    dplyr::rename(group_idnumber = .data[["value"]], group_name = .data[["title"]])

  return(group_ids)
}

#' @title Add members to existing Moodle groups
#' @description
#' Populates Moodle groups with members based on correspondence between Moodle group name, and a
#' "tidy" data frame describing the group each student is in.
#'
#' @inheritParams create_new_section
#' @param groups A data frame containing the group names and emails of students in each group. The name of the column holding the emails must be `email_address`, and the name of the column holding the group names must be `group_name`.
#'
#' The `group_name` column must hold entries corresponding to existing Moodle group names. The list of existing groups in a Moodle course can be retrieved using [get_group_ids()]
#' @importFrom dplyr left_join semi_join
#'
#' @return An (invisible) list of [httr::response] objects containing the response from Moodle's group/members.php endpoint for each added group
#'
#' @export
populate_groups <- function(tab, groups) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  group_ids <- get_group_ids(course_id, tab)
  student_ids <- get_student_secret_ids(course_id, tab)

  groups <- dplyr::left_join(
    groups,
    student_ids,
    by = c("email_address" = "student_email")
  )

  resp_list <- vector(mode = "list", legnth = length(group_ids$group_idnumber))
  names(resp_list) <- group_ids$group_idnumber

  for (id in group_ids$group_idnumber) {

    x <- dplyr::semi_join(
      groups,
      dplyr::filter(group_ids, .data$group_idnumber == id),
      by = "group_name"
    )

    student_id_component <- paste("addselect%5B%5D",  x$student_ids,  sep = "=", collapse = "&")

    generic_body <- list(
      "sesskey" = sessionkey,
      "removeselect_searchtext" = "",
      "userselector_preserveselected" = "0",
      "userselector_autoselectunique" = "0",
      "userselector_searchanywhere" = "0",
      "add" = "%E2%97%84%C2%A0Add",
      "addselect_searchtext" = ""
    )

    students <- as.list(x$student_id)
    names(students) <- rep("addselect[]", length(students))

    resp_list[as.character(id)] <- httr::POST(
      paste0(tab$site_url, "/group/members.php?group=", id),
      encode = "form",
      body = c(generic_body, students),
      cookies,
      httr::user_agent(UA)
    )

  }

  return(invisible(resp_list))

}

#' @title Retrieve course roster from Moodle
#' @description
#' Retrieve the course roster from Moodle. This roster includes all students, but not all course
#' participants (i.e., does not include TA's, instructors, administrators, etc.).
#'
#' @inheritParams create_new_section
#'
#' @importFrom xml2 url_escape
#' @importFrom httr content
#'
#' @return A data frame with 4 columns (`first_name`, `last_name`, `id_number`, and `email_address`).
#' Note that the ID number corresponds to the "school ID" number, not their internal Moodle "secret"
#' ID number.
#' @export
get_course_roster <- function(tab) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  export_page <- httr::GET(
    url = paste0(tab$site_url, "/grade/export/txt/index.php?id=", course_id),
    cookies,
    httr::user_agent(UA)
  )

  grade_items <- httr::content(export_page) |>
    html_elements("#id_gradeitemscontainer .form-check-input") |>
    html_attr("name")

  body <- list(
    "mform_isexpanded_id_gradeitems" = "1",
    "checkbox_controller1" = "0",
    "mform_isexpanded_id_options" = "1",
    "id" = as.character(course_id),
    "sesskey" = sessionkey,
    "_qf__grade_export_form" = "1"
  )

  body <- c(
    body,
    setNames(object = rep(list("0"), length(grade_items)),
             nm = grade_items
             ),
    list(
      "export_feedback" = "0",
      "export_onlyactive" = "0",
      "export_onlyactive" = "1",
      "display[real]" = "0",
      "display[real]" = "1",
      "display[percentage]" = "0",
      "display[letter]" = "0",
      "decimals" = "2",
      "separator" = "comma",
      "submitbutton" = "Download"
    )
  )

  raw_csv <- httr::POST(
    url = paste0(tab$site_url, "/grade/export/txt/export.php"),
    httr::add_headers(Accept = "text/csv"),
    encode = "form",
    cookies,
    body = body,
    httr::user_agent(UA)
  )

  roster <- httr::content(
    raw_csv,
    type = "text/csv",
    show_col_types = FALSE,
    encoding = "UTF-8"
  )

  roster <- roster[c("First name", "Last name", "ID number", "Email address")]
  names(roster) <- c("first_name", "last_name", "id_number", "email_address")

  return(roster)
}


get_quiz_responses <- function(item_id) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)

  responses <- httr::POST(
    url = paste0(tab$site_url, "/mod/quiz/report.php"),
    body = list(
      sesskey = sessionkey,
      download = "csv",
      id = item_id,
      mode = "overview",
      attempts = "enrolled_any",
      slotmarks = 1
    ),
    cookies,
    httr::user_agent(UA)
  ) |>
    httr::content(
      type = "text/csv",
      show_col_types = FALSE,
      encoding = "UTF-8",
      na = c("", "NA", "-")
    )

  responses <- responses[-nrow(responses), ]
  return(responses)

}

#' @title Download quiz responses and attachments
#' @description Downloads all complete responses to a Moodle quiz, along with any file attachments.
#'
#' @inheritParams create_new_section
#' @param item_id The ID number of the Moodle assignment/quiz. Can be found in the URL shown in the
#'   address bar when the assignment/quiz is opened in the web browser.
#' @param questions Vector of question numbers that are expected to have attachments
#' @param include_attachments Logical scalar indicating whether attachments should be downloaded
#' @param output_dir Path to the folder where responses/attachments retrieved from Moodle will be
#'   output. A folder matching the name of the quiz/assignment will be created in this folder, and
#'   all responses attachments will be placed within. Has no effect when `include_attachments` is
#'   `FALSE`.
#' @param overwrite Logical scalar. Controls whether existing files are overwritten when downloading
#'  attachments
#'
#' @details Text responses for each finished attempted are stored in the responses.csv file Within
#'   `output_dir` a folder is created for each student, where the name of the folder corresponds to
#'   the student's email address. Each student's file attachments from the Moodle quiz are placed
#'   in the their respective folder.
#'
#' @importFrom tidyselect everything
#' @importFrom rvest html_text html_element
#' @importFrom httr write_disk
#' @importFrom purrr walk2
#' @importFrom dplyr group_by n
#'
#' @return A data frame beginning with the columns `Name`, `Email address`, `Attempt`, `Status`, and
#' `Grade/100.00`, followed by a column for each quiz question response. The columns are named
#' `Response 1`, `Response 2`, `Response 3`, etc.
#'
#' Note that if a quiz allows a student multiple attempts, then all attempts will be included in
#' this data frame.
#'
#' @export
download_quiz_responses <- function(
  tab,
  item_id,
  include_attachments = FALSE,
  output_dir,
  overwrite = FALSE
) {

  if (length(include_attachments) != 1L || !is.logical(include_attachments)) {
    stop("`include_attachments` must be `TRUE` or `FALSE`")
  }

  if (include_attachments && missing(output_dir)) {
    stop("If `include_attachments = TRUE, then the `output_dir` arugment must be specified")
  }

  p <- tab$Page$loadEventFired(wait_ = FALSE)
  tab$Page$navigate(
    paste0(tab$site_url, "/mod/quiz/report.php?id=", item_id, "&mode=overview&onlygraded=1"),
    wait_ = FALSE
  )
  tab$wait_for(p)

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)

  request_body <- list(
    "id" = item_id,
    "mode" = "responses",
    "sesskey" = sessionkey,
    "_qf__quiz_responses_settings_form" = 1,
    "mform_isexpanded_id_preferencespage" = 1,
    "mform_isexpanded_id_preferencespage" = 1,
    "attempts" = "enrolled_with",
    "stateinprogress" = 0,
    "stateoverdue" = 0,
    "statefinished" = 0,
    "statefinished" = 1,
    "stateabandoned" = 0,
    "pagesize" = 500,
    "qtext" = 0,
    "resp" = 0,
    "resp" = 1,
    "right" = 0,
    "submitbutton" = "Show+report"
  )

  quiz_response_page <- httr::POST(
    "https://moodle.smith.edu/mod/quiz/report.php",
    encode = "form",
    body = request_body,
    cookies,
    httr::user_agent(UA)
  ) |>
    httr::content()

  quiz_title <- quiz_response_page |>
    html_element("#page-header h1") |>
    rvest::html_text()

  responses_table <- quiz_response_page |>
    rvest::html_element(css = "table#responses")

  responses <- responses_table |>
    rvest::html_table(na.strings = c("", "-")) |>
    dplyr::filter(!is.na(.data$`Select all`)) |>
    dplyr::select(-.data$`Select all`)

  names(responses) <- sub("Sort by.*", "", names(responses))

  incomplete_quiz_index <- responses |>
    dplyr::transmute(dplyr::across(tidyr::everything(), is.na)) |>
    rowSums() |>
    as.logical()

  # incomplete_quizzes <- responses[incomplete_quiz_index, ]

  responses <- responses |>
    dplyr::rename(Name = .data$`First name  / Last name`) |>
    dplyr::mutate(
      Name = sub("Review attempt", "", .data$Name, fixed = TRUE)
    )

  if (include_attachments) {
    root <- file.path(output_dir, quiz_title)

    if (!dir.exists(root)) {
      dir.create(root, recursive = TRUE)
    }

    questions <- responses |>
      select(starts_with("Response")) |>
      purrr::map_lgl(\(x) any(grepl("Attachments: ", x, fixed = TRUE))) |>
      which()

    rows <- responses_table |>
      rvest::html_elements("tr:not(.emptyrow)")

    rows <- rows[-1] # drop header row
    # rows <- rows[!incomplete_quiz_index] # discard incomplete quizzes

    # Map question numbers to column indices in the Moodle table
    # -1 because Moodle starts the columns number index at 0
    column_indexes <- paste0(".c", questions - 1 + 5)

    for (c in seq_along(column_indexes)) {
      links_to_questions <- rows |>
        html_element(css = column_indexes[c]) |>
        html_element("a") |>
        html_attr("href")

      for (i in seq_along(links_to_questions)) {
        student_dir <- file.path(root, responses$`Email address`[i])
        if (!dir.exists(student_dir)) {
          dir.create(student_dir)
        }

        q <- httr::GET(url = links_to_questions[[i]], cookies) |>
          httr::content()

        question_state <- q |>
          rvest::html_elements("div.state") |>
          rvest::html_text()

        if (question_state == "Not answered") {
          message("Skipping ", i, " Question ", questions[c] - 1 + 5, " (Question not answered)")
          next
        }

        attachment_anchors <- links <- q |>
          html_elements("div.attachments a")

        attachment_names <- html_text2(attachment_anchors)
        attachment_links <- html_attr(attachment_anchors, "href")

        if (length(attachment_links) == 0) {
          message("Skipping ", i, " Question ", questions[c] - 1 + 5, " (No Attachment Found)")
          next
        }

        cli::cli_alert_info("Downloading responses for {responses$`Email address`[i]}")

        question_dir <- file.path(student_dir, paste0("Question_", questions[c]))
        if (!dir.exists(question_dir)) {
          dir.create(question_dir)
        }

        purrr::walk2(attachment_links, attachment_names, \(x, y) {
          filepath <- file.path(question_dir, y)
          if (!file.exists(filepath) | overwrite) {
            httr::GET(x, cookies, httr::write_disk(filepath, overwrite = TRUE))
          }
        })

        attachment_paths <- paste(paste0("Question_", questions[c]), attachment_names, sep = "/")
        responses[[paste("Response", questions[c])]][i] <- list(attachment_paths)
      }
    }
  }

  # The Moodle response table only associates the students first quiz attempt with their email
  # all subsequent attempts in the rows below have blanks for the email that propagate as NA
  # values in R

  # This overwrites the NA values in rows 2,3,4, etc. with the email address in row 1 for each
  # student
  responses <- responses |>
    dplyr::group_by(.data$`Name`) |>
    dplyr::mutate(
      `Email address` = .data$`Email address`[1],
      `Attempt` = 1:dplyr::n(),
      ) |>
    dplyr::select(
      c("Name", "Email address", "Attempt", "Status", "Grade/100.00"),
      starts_with("Response")
      )

  return(responses)
}

#' @title Get gradebook items
#' @description
#' Retrieve the names and internal ID numbers for all items in the gradebook.
#'
#' @inheritParams create_new_section
#' @return A named character vector with one element per item in the course gradebook. The elements of the vector correspond to the internal item ID numbers used in the Moodle database. The names of each element correspond to the user-facing names assigned to each item.
#' @export
get_gradebook_items <- function(tab) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  export_page <- httr::GET(
    url = paste0(tab$site_url, "/grade/export/txt/index.php?id=", course_id),
    cookies,
    httr::user_agent(UA)
  ) |>
    httr::content()

  grade_item_names <- export_page |>
    html_elements("#id_gradeitemscontainer .form-check label") |>
    html_text2()

  grade_item_ids <- export_page |>
    html_elements("#id_gradeitemscontainer .form-check-input") |>
    html_attr("name")

  names(grade_item_ids) <- grade_item_names

  return(grade_item_ids)

}

#' @title Retrieve course roster from Moodle
#' @description
#' Retrieve the course roster from Moodle. This roster includes all students, but not all course
#' participants (i.e., does not include TA's, instructors, administrators, etc.).
#'
#' @inheritParams create_new_section
#' @param grade_items Character vector holding the precise names of items in the Moodle gradebook; these names can be obtained from [get_gradebook_items()]. When this argument is omitted, no items are retrieved and only the ID variables (name, email, etc.) are retrieved.
#'
#' @importFrom xml2 url_escape
#' @importFrom httr content
#'
#' @return A data frame beginning with 4 columns (`first_name`, `last_name`, `id_number`, and `email_address`), and additional columns for each requested item.
#'
#' @examples
#' \dontrun{
#' tab <- open_moodle(
#'   site_url = "https://moodle.smith.edu",
#'   graphical = TRUE
#' )
#' # Export the full gradebook
#' grade_items <- get_gradebook_items(tab)
#' grades <- export_gradebook(tab, names(grade_items))
#'
#' # Just the items starting with prefix "Reading Quiz"
#' is_quiz_item <- startsWith(names(grade_items), "Reading Quiz")
#' grades <- export_gradebook(tab2, names(grade_items)[is_quiz_item])
#' }
#' @export
export_gradebook <- function(tab, grade_items) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  export_page <- httr::GET(
    url = paste0(tab$site_url, "/grade/export/txt/index.php?id=", course_id),
    cookies,
    httr::user_agent(UA)
  )

  item_ids <- get_gradebook_items(tab)
  x <- setNames(object = rep(list("0"), length(item_ids)), nm = item_ids)

  if (!missing(grade_items)) {
    requested_ids <- item_ids[grade_items]
    x <- c(x, setNames(object = rep(list("1"), length(requested_ids)), nm = requested_ids))
  }

  body <- c(
    list(
      "mform_isexpanded_id_gradeitems" = "1",
      "checkbox_controller1" = "0",
      "mform_isexpanded_id_options" = "1",
      "id" = as.character(course_id),
      "sesskey" = sessionkey,
      "_qf__grade_export_form" = "1"
    ),
    x,
    list(
      "export_feedback" = "0",
      "export_onlyactive" = "0",
      "export_onlyactive" = "1",
      "display[real]" = "0",
      "display[real]" = "1",
      "display[percentage]" = "0",
      "display[letter]" = "0",
      "decimals" = "2",
      "separator" = "comma",
      "submitbutton" = "Download"
    )
  )

  raw_csv <- httr::POST(
    url = paste0(tab$site_url, "/grade/export/txt/export.php"),
    httr::add_headers(Accept = "text/csv"),
    encode = "form",
    cookies,
    body = body,
    httr::user_agent(UA)
  )

  gb <- httr::content(
    raw_csv,
    type = "text/csv",
    show_col_types = FALSE,
    encoding = "UTF-8",
    as = "parsed",
    na = c("", "NA", "-") # passed to readr::read_csv()
  )

  return(gb)
}

