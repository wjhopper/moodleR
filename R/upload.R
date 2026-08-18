#' @title Upload file to Moodle
#'
#' @inheritParams create_section
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
      visible = as.numeric(visible),
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

  error_contents <- httr::content(upload_info) |>
    extract_moodle_error()

  if (is_moodle_error(error_contents)) {
    throw_moodle_error(error_contents)
  }

  return(invisible(upload_info))
}

#' @title Create Moodle groups
#'
#' @inheritParams create_section
#' @param groups A character vector holding the names of each group to be created
create_groups <- function(tab, groups) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  import_groups_page <- httr::GET(
    url = paste0(tab$site_url, "/group/import.php?", course_id),
    cookies,
    httr::user_agent(UA)
  ) |>
    httr::content()

  files_input_id <- import_groups_page |>
    rvest::html_element("input#id_files") |>
    rvest::html_attr("value")

  edit_context <- extract_moodle_context_string(import_groups_page)

  tmp_csv <- tempfile()

  data.frame(groupname = groups) |>
    write.csv(file = tmp_csv, row.names = FALSE)

  file_info <- httr::POST(
    url = paste0(tab$site_url, "/repository/repository_ajax.php?action=upload"),
    body = list(
      repo_upload_file = httr::upload_file(tmp_csv),
      sesskey = sessionkey,
      repo_id = 4,
      itemid = files_input_id,
      author = "",
      savepath = "/",
      title = "groupnames.csv",
      ctx_id = edit_context
    ),
    encode = "multipart",
    cookies,
    httr::user_agent(UA)
  )

  response <- httr::POST(
    url = paste0(tab$site_url, "/group/import.php"),
    body = list(
      id = course_id,
      sesskey = sessionkey,
      `_qf__groups_import_form` = 1,
      mform_isexpanded_id_general = 1,
      userfile = files_input_id,
      delimiter_name = "comma",
      encoding = "UTF-8",
      submitbutton = "Import groups"
    ),
    encode = "form",
    cookies,
    httr::user_agent(UA)
  )

  error_contents <- httr::content(response) |>
    extract_moodle_error()

  if (is_moodle_error(error_contents)) {
    throw_moodle_error(error_contents)
  }

  return(invisible(response))

}

#' @title Add text area to Moodle page
#'
#' @inheritParams upload_file
#' @param text A character vector holding text string you want to upload. Can contain raw HTML. If lenth > 1, the elements are concatenated together using a line break.
#' @export
add_text <- function(tab, section, title, text) {

  sessionkey <- extract_moodle_session_key(tab)
  cookies <- extract_cookies(tab)
  UA <- get_user_agent(tab)
  course_id <- tab$course

  section_info <- get_section_info(tab, section)

  if (length(section_info) > 1) {
    warning("Multiple sections named ", section, " found, using the first one")
    section_info <- section_info[[1]]
  }

  response <- httr::POST(
    url = paste0(tab$site_url, '/course/modedit.php'),
     body = list(
      	"showdescription" = "1",
      	"completionunlocked" = "1",
      	"course" = course_id,
      	"coursemodule" = " ",
      	"section" = section_info[[1]]$number,
      	"module" = "13",
      	"modulename" = "label",
      	"instance" = "",
      	"add" = "label",
      	"update" = "0",
      	"return" = "0",
      	"sr" = "0",
      	"sesskey" = sessionkey,
      	"_qf__mod_label_mod_form" = "1",
      	"mform_isexpanded_id_generalhdr" = "1",
      	"mform_isexpanded_id_modstandardelshdr" = "1",
      	"mform_isexpanded_id_availabilityconditionsheader" = "0",
      	"mform_isexpanded_id_activitycompletionheader" = "1",
      	"introeditor[text]" = text,
      	"name" = title,
      	"introeditor[format]" = "1",
      	"introeditor[itemid]" = "997065256",
      	"visible" = "1",
      	"cmidnumber" = "",
      	"lang" = "",
      	"availabilityconditionsjson" = "{\"op\":\"&\",\"c\":[],\"showc\":[]}",
      	"completion" = "0",
      	"submitbutton2" = "Save and return to course"
    	),
     cookies,
     httr::user_agent(UA)
   )

  error_contents <- httr::content(response) |>
    extract_moodle_error()

  if (is_moodle_error(error_contents)) {
    throw_moodle_error(error_contents)
  }

  return(invisible(response))
}
