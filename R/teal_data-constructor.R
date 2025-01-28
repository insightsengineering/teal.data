#' Comprehensive data integration function for `teal` applications
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Initializes a data for `teal` application.
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
#'  Use [verify()] to verify code reproducibility.
#'
#' @details
#'
#' A `teal_data` is meant to be used for reproducibility purposes. The class inherits from
#' [`teal.code::qenv`] and we encourage to get familiar with \CRANpkg{teal.code} first.
#' `teal_data` has following characteristics:
#'
#' - It inherits from the environment and methods such as [`$`], [get()], [ls()], [as.list()],
#' [parent.env()] work out of the box.
#' - `teal_data` is a locked environment, and data modification is only possible through the
#' [teal.code::eval_code()] and [`within.qenv()`][teal.code::within.qenv()] functions.
#' - It stores metadata about the code used to create the data (see [`get_code()`][get_code,teal_data-method]).
#' - It supports slicing (see [`teal.code::subset-qenv`])
#' - Is immutable which means that each code evaluation does not modify the original `teal_data`
#'   environment directly.
#' - It maintains information about relationships between datasets (see [join_keys()]).
#'
#' @return A `teal_data` object.
#'
#' @seealso [`teal.code::eval_code`], [`get_code()`][get_code,teal_data-method], [join_keys()], [names.teal_data()]
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
  methods::new(
    "teal_data",
    .xData = data_objects,
    code = code,
    join_keys = join_keys
  )
}
