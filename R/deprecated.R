.deprecate_function <- function(what, details) {
  lifecycle::deprecate_stop(
    when = "0.4.0",
    what = what,
    details = details
  )
}

#' @rdname col_labels
#' @include formatters_var_labels.R
#' @details
#' `r lifecycle::badge("deprecated")`
#'
#' In previous versions of `teal.data` labels were managed with `get_labels()`.
#' This function is deprecated as of `0.4.0`, use `col_labels` instead.
#' @export
get_labels <- function(...) {
  .deprecate_function("get_labels()", "Use col_labels(data)")
}

#' Names of data sets in `teal_data` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use `names()` instead of `datanames()`.
#'
#' `datanames()` is deprecated. If object should be hidden, then use a `.` (dot)
#' prefix for the object's name.
#'
#' @param x (`teal_data` or `qenv_error`) object to access or modify
#' @param value (`character`) new value for `@datanames`; all elements must be names of variables existing in `@.xData`
#'
#' @return The contents of `@datanames` or `teal_data` object with updated `@datanames`.
#' @aliases `datanames<-.teal_data`
#'
#' @name datanames

#' @rdname datanames
#' @export
datanames <- function(x) {
  lifecycle::deprecate_soft("0.6.1", "datanames()", details = "names()")
  names(x)
}

#' @rdname datanames
#' @export
`datanames<-` <- function(x, value) {
  lifecycle::deprecate_soft(
    "0.6.1",
    "`datanames<-`()",
    details = "invalid to use `datanames()<-` or `names()<-` on an object of class `teal_data`. See ?names.teal_data"
  )
  names(x)
}

#' @rdname datanames
#' @export
#' @keywords internal
`names<-.teal_data` <- function(x, value) {
  lifecycle::deprecate_warn(
    "0.6.1",
    "`names<-.teal_data`()",
    details = "invalid to use `datanames()<-` or `names()<-` on an object of class `teal_data`. See ?names.teal_data"
  )
  x
}
