#' @title Assign grades to assignments that use scale grading
#'
#' @inheritParams extract_cookies tab
#' @inheritParams download_quiz_responses item_id
#' @param grade Character vector holding grades to assign. Values must
#' correspond to the name of a grade from the grading scale. If length equals 1,
#' then the grade is applied to all submissions.
#' @param status Scalar character vector. Identifies the subset of submissions
#' for which the grades are to be set. Valid values are `"all"`,
#' `"requiregrading"`, `"notsubmitted"`, `"submitted"`, and `"graded"`.
#'
#' @export
receive_assignment <- function(tab, item_id, grade, status = "requiregrading") {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)

  valid_statuses <- c("all", "requiregrading", "notsubmitted", "submitted", "graded")
  if (! status %in% valid_statuses) {
    stop("`status` must be one of ", cat(paste0('"', valid_statuses, '"'), sep = ", "))
  }

  if (status == "all") {
    # Moodle uses a blank parameter status to indicate all students
    status <- character(0)
  }

  quickgrading_URL <- paste0(
      tab$site_url,
      "/mod/assign/view.php?id=",
      item_id,
      "&action=grading&",
      "status=", status
    )

  grading_page <- httr::GET(quickgrading_URL, cookies)

  rows <- grading_page |>
    httr::content() |>
    html_elements("tr:not(.emptyrow)")

  columns <- rows[1] |>
    html_elements("th") |>
    html_text2()

  hidden_inputs <- rows[-1] |>
    html_elements("input") |>
    rvest::html_attrs() |>
    purrr::keep(\(x) x["type"] == "hidden")

  grademodified <- hidden_inputs |>
    purrr::keep(\(x) startsWith(x["name"], "grademodified") ) |>
    purrr::map(\(x) setNames(list(unname(x["value"])), x["name"])) |>
    unlist(recursive = FALSE)

  gradeattempt <- hidden_inputs |>
    purrr::keep(\(x) startsWith(x["name"], "gradeattempt") ) |>
    purrr::map(\(x) setNames(list(unname(x["value"])), x["name"])) |>
    unlist(recursive = FALSE)

  grade_column <- which(columns == "Grade") - 1

  grade_inputs <- rows[-1] |>
    html_elements(paste0("td.c", grade_column, " select.quickgrade"))

  grade_input_names <- grade_inputs |>
    html_attr("name")

  grade_names <- grade_inputs[[1]] |>
    html_elements("option") |>
    html_text() |>
    tolower()

  grade_input_values <- grade_inputs[[1]] |>
    html_elements("option") |>
    html_attr("value")

  selected_value <- grade_input_values[grade_names == tolower(grade)]

  if (length(selected_value) != length(grade_input_names)) {
    grades <- as.list(rep(selected_value, length(grade_input_names)))
  } else {
    grades <- as.list(selected_value)
  }

  grades <- setNames(grades, nm = grade_input_names)

  .x <- httr::POST(
    url = paste0(tab$site_url, "/mod/assign/view.php"),
    body = c(
      list(
        id = item_id,
        action = "quickgrade",
        sesskey = sessionkey,
        `_qf__mod_assign_quick_grading_form` = 1
      ),
      grades,
      grademodified,
      gradeattempt,
      list(
        sendstudentnotifications = 0
      )
    ),
    encode = "form",
    cookies,
    httr::user_agent(UA)
  )

  msg <- httr::content(.x) |>
    html_element("div#region-main div.alert") |>
    html_text2()

  if (startsWith(msg, "The grade changes were saved")) {
    return(invisible(.x))
  }
  else {
    stop("Error updating grades: ", msg)
  }
}

#' @title Retrieve data about submissions to a Moodle assignment
#' @description
#' Retrieves the table of information about submissions found on the assignments
#' "Submissions" tab. Can also optionally download all submissions in a zip file.
#'
#' @inheritParams extract_cookies tab
#' @inheritParams download_quiz_responses item_id include_attachments output_dir overwrite
#' @inheritParams receive_assignment status
#'
#' @importFrom rvest html_table
#' @importFrom dplyr across starts_with
#' @export
get_assignment_submissions <- function(
    tab,
    item_id,
    status = "requiregrading",
    include_attachments = FALSE,
    output_dir,
    overwrite = FALSE
) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)

  if (status == "all") {
    # Moodle uses a blank parameter status to indicate all students
    status <- character(0)
  }

  quickgrading_URL <- paste0(
    tab$site_url,
    "/mod/assign/view.php?id=",
    item_id,
    "&action=grading&",
    "status=", status
  )

  grading_page <- httr::GET(quickgrading_URL, cookies) |>
    httr::content()

  submissions_table <- grading_page  |>
    rvest::html_element("table#submissions")

  submissions <- submissions_table |>
    rvest::html_table(na.strings = c("", "-")) |>
    dplyr::select(
      Name = tidyselect::starts_with("Select"),
      "Email address",
      "Status",
      "Grade",
      tidyselect::starts_with("Last modified")) |>
    dplyr::mutate(
      Name = sub("Select ", "", .data$Name, fixed = TRUE),
      dplyr::across(
        dplyr::starts_with("Last modified"),
        \(x) as.POSIXct(x, format = "%A, %B %d, %Y, %H:%M %p")
      )
    )

  return(submissions)
}
