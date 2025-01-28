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
  lifecycle::deprecate_soft(
    when = "0.7.0",
    what = "datanames()",
    with = "names()"
  )
  names(x)
}

#' @rdname datanames
#' @export
`datanames<-` <- function(x, value) {
  lifecycle::deprecate_soft(
    when = "0.7.0",
    what = "`datanames<-`()",
    details = "invalid to use `datanames()<-` or `names()<-` on an object of class `teal_data`. See ?names.teal_data"
  )
  x
}

#' @rdname datanames
#' @export
#' @keywords internal
`names<-.teal_data` <- function(x, value) {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "`names<-.teal_data`()",
    details = "invalid to use `datanames()<-` or `names()<-` on an object of class `teal_data`. See ?names.teal_data"
  )
  x
}
