#' Names of data sets in `teal_data` object
#'
#' Functions to get the names of a `teal_data` object.
#' The names are obtained from the objects listed in the `qenv` environment.
#'
#' Objects named with a `.` (dot) prefix will be ignored and not returned.
#' To get the names of all objects, use `ls(x, all.names = TRUE)`, however, it
#' will not group the names by the join_keys topological structure.
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
#' names(td) # '.CO2' will not be returned
#'
#' @export
names.teal_data <- function(x) {
  # Sorting from `ls` can be safely done as environments don't have any order
  # nor support numeric-index subsetting
  envir <- as.environment(x)
  .get_sorted_names(ls(envir = envir), join_keys(x), envir)
}

#' @keywords internal
.get_sorted_names <- function(datanames, join_keys, env) {
  child_parent <- sapply(datanames, parent, x = join_keys, USE.NAMES = TRUE, simplify = FALSE)

  union(
    intersect(unlist(topological_sort(child_parent)), ls(env, all.names = TRUE)),
    datanames
  )
}
