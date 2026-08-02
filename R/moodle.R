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
