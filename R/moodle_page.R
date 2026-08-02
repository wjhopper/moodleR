#' @title MoodlePage class
#' @description
#' MoodlePage objects represents a Chrome browser tab tone that is logged in to a Moodle instance.
#' A _session_ in a Chromote object. Note that in the Chrome
#' DevTools Protocol a session is a debugging session connected to a _target_,
#' which is a browser window/tab or an iframe.
#' @importFrom R6 R6Class
#' @importFrom chromote ChromoteSession
#' @export
MoodlePage <- R6::R6Class(
  "MoodlePage",
  inherit = chromote::ChromoteSession,
  lock_objects = FALSE,
  cloneable = FALSE,
  parent_env = asNamespace("chromote"),
  public = list(
    #' @field site_url root URL for the Moodle instance your are connected to
    site_url = NULL,

    #' @field timeout Default timeout in seconds for \pkg{chromote} to wait for a response.
    timeout = NULL,

    #' @description Determine whether an object represents a valid Moodle internal course id number
    #' @param course_id A scalar vector holding a positive integer value
    is_valid_course_id = function(course_id) {

      len <- length(course_id)

      if (len == 1 && is.numeric(course_id) && !is.integer(course_id)) {
        int_course_id <- as.integer(course_id)
        is_integer <- !is.na(int_course_id) || int_course_id == course_id
      }

      if (len == 1 && is.character(course_id)) {
        int_course_id <- suppressWarnings(as.integer(course_id))
        is_integer <- !is.na(int_course_id) && int_course_id == suppressWarnings(as.numeric(course_id))
      }

      if (!is_integer || len != 1) {
        stop("Course ID must be a single positive integer, but a ", class(course_id), " of length ", len, " was supplied")
      }
      if (course_id <= 0) {
        stop("Course ID must be a single positive integer, but a non-positive value was supplied")
      }

      return(TRUE)
    },

    #' @description Create a new `MoodlePage` object
    #' @param site_url A scalar character vector holding the root URL for your Moodle instance
    #' @param timeout Scalar numeric vector. Describes the number of second to wait for network events (e.g., for a web page to load).
    #' @param ... Arguments passed to [chromote::ChromoteSession] constructor
    initialize = function(site_url, timeout = 15, ...) {
      super$initialize(...)
      self$site_url = site_url
      self$timeout = timeout
    }
  ),
  active = list(
    #' @field course Returns or sets the course
    course = function(course_id) {
      if (missing(course_id)) {
        return(private$course_id)
      }
      else {
        if (self$is_valid_course_id(course_id)) {
          self$go_to(paste0(self$site_url, "/course/view.php?id=", course_id))
          private$course_id <- course_id
        }
      }
    }
  ),
  private = list(
    course_id = NULL
  )
)
