receive_assignment <- function(tab, assignment_id) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  quickgrading_URL <- paste0(
      tab$site_url,
      "/mod/assign/view.php?id=",
      assignment_id,
      "&action=grading&status=requiregrading"
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

  grade_input_values <- grade_inputs[[1]] |>
    html_elements("option") |>
    html_attr("value")

  max_value <- grade_input_values |>
    as.numeric() |>
    max()

  grades <- as.list(rep(max_value, length(grade_input_names))) |>
    setNames(nm = grade_input_names)

  .x <- httr::POST(
    url = paste0(tab$site_url, "/mod/assign/view.php"),
    body = c(
      list(
        id = assignment_id,
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
