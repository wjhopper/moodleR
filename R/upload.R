#' @title Upload file to Moodle
#'
#' @inheritParams create_new_section
#' @param section Scalar character vector giving the name of the section where the file should be added
#' @param title Scalar character vector giving the title displayed for the file on the main page
#' @param path Scalar character vector giving the path to the file to be uploaded
#' @param visible Scalar logical vector indicating whether the file should be visible or hidden upon upload
#' @export
upload_file <- function(tab, section, title, path, visible = TRUE) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  section_info <- get_section_info(tab, section)

  if (length(section_info) > 1) {
    warning("Multiple sections named ", section, " found, using the first one")
    section_info <- section_info[[1]]
  }

  newfile_page <- httr::GET(
    url = paste0(tab$site_url, "/course/modedit.php"),
    query = list(
      add = "resource",
      type = "",
      course = course_id,
      sectionid = section_info[[1]]$id,
      return = 0,
      beforemod = 0
    ),
    cookies,
    httr::user_agent(UA)
  ) |>
    httr::content()

  files_input_id <- newfile_page |>
    rvest::html_element("input#id_files") |>
    rvest::html_attr("value")

  edit_context <- extract_moodle_context_string(newfile_page)

  description_input_id <- newfile_page |>
    rvest::html_elements("div#fitem_id_introeditor input") |>
    purrr::keep(\(x) html_attr(x, "name") == "introeditor[itemid]") |>
    html_attr("value")

  file_info <- httr::POST(
    url = paste0(tab$site_url, "/repository/repository_ajax.php?action=upload"),
    body = list(
      repo_upload_file = httr::upload_file(path),
      sesskey = sessionkey,
      repo_id = 4,
      itemid = files_input_id,
      author = "",
      savepath = "/",
      title = title,
      ctx_id = edit_context
    ),
    encode = "multipart",
    cookies,
    httr::user_agent(UA)
  )

  upload_info <- httr::POST(
    url = paste0(tab$site_url, "/course/modedit.php"),
    body = list(
      completionunlocked = 1,
      course = course_id,
      section = section_info[[1]]$number,
      module = 19,
      modulename = "resource",
      add = "resource",
      update = 0,
      revision = 1,
      sesskey = sessionkey,
      `_qf__mod_resource_mod_form` = 1,
      name = title,
      "introeditor[text]" = "",
      "introeditor[format]" = 1,
      "introeditor[itemid]" = description_input_id,
      files = files_input_id,
      display = 0,
      filterfiles = 0,
      visible = as.logical(visible),
      availabilityconditionsjson =
        '{"op":"&","c":[],"showc":[]}',
      completion = 0,
      competencies = "_qf__force_multiselect_submission",
      competency_rule = 0,
      submitbutton2 = "Save and return to course"
    ),
    encode = "form",
    cookies,
    httr::user_agent(UA)
  )

  return(upload_info)
}
