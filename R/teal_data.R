#' Comprehensive data integration function for `teal` applications
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Universal function to pass data to teal application.
#'
#' @param ... any number of objects (presumably data objects) provided as `name = value` pairs.
#'
#' @param join_keys (`join_keys` or single `join_key_set`)
#' optional object with datasets column names used for joining.
#' If empty then no joins between pairs of objects.
#'
#' @param code (`character`, `language`) optional code to reproduce the datasets provided in `...`.
#'  Note this code is not executed and the `teal_data` may not be reproducible
#'
#' Use [verify()] to verify code reproducibility .
#'
#' @return A `teal_data` object.
#'
#' @export
#'
#' @examples
#' teal_data(x1 = iris, x2 = mtcars)
#'
teal_data <- function(...,
                      join_keys = teal.data::join_keys(),
                      code = character(0)) {
  data_objects <- rlang::list2(...)
  if (inherits(join_keys, "join_key_set")) {
    join_keys <- teal.data::join_keys(join_keys)
  }

  if (length(data_objects) > 0 && !checkmate::test_names(names(data_objects), type = "named")) {
    stop("Dot (`...`) arguments on `teal_data()` must be named.")
  }
  new_teal_data(
    data = data_objects,
    code = code,
    join_keys = join_keys
  )
}
