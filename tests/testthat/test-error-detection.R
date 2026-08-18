test_that("Error detection works", {

  pw <- Sys.getenv("moodle_pw")

  if (nchar(pw) == 0) {
    skip("moodle_pw env variable is empty")
  }

  tab <- open_moodle(
    site_url = "https://moodle.smith.edu",
    graphical = FALSE,
    username = "whopper",
    password = pw,
    org = "smith",
    twoFA = list(duo = "push")
  )

  tab$course <- .Machine$integer.max

  error_content <- get_page_body(tab) |>
    extract_moodle_error()

  expect_equal(error_content$message, "Can't find data record in database.")
  expect_true(is_moodle_error(error_content))
  expect_error(throw_moodle_error(error_content))
})
