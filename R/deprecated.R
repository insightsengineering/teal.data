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
