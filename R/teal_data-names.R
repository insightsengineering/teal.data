#' Names of data sets in `teal_data` object
#'
#' Functions to get the names of a `teal_data` object.
#' The names are extrapolated from the objects in the `qenv` environment and
#' are not stored statically, unlike the normal behavior of `names()` function.
#'
#' Objects named with a `.` (dot) prefix will be ignored and not returned,
#' unless `all.names` parameter is set to `TRUE`.
#'
#' @param x A (`teal_data`) object to access or modify.
#'
#' @return A character vector of names.
#'
#' @examples
#' td <- teal_data(iris = iris)
#' td <- within(td, mtcars <- mtcars)
#' names(td)
#'
#' td <- within(td, .CO2 <- CO2)
#' names(td)
#'
#' @export
names.teal_data <- function(x) {
  # Sorting can be safely done as environments don't have any order
  # nor support numeric-index subsetting
  names_x <- sort(names(as.environment(x)))
  .get_sorted_names(names_x, join_keys(x), as.environment(x))
}

#' @keywords internal
.get_sorted_names <- function(datanames, join_keys, env) {
  child_parent <- sapply(
    datanames,
    function(name) parent(join_keys, name),
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  union(
    intersect(unlist(topological_sort(child_parent)), ls(env, all.names = TRUE)),
    datanames
  )
}
