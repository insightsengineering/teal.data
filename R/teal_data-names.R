#' Names of data sets in `teal_data` object
#'
#' Functions to get the names of a `teal_data` object.
#' The names are obtained from the objects listed in the `qenv` environment.
#'
#' Objects named with a `.` (dot) prefix will be ignored and not returned.
#' To get the names of all objects, use `ls(x, all.names = TRUE)`, however, it
#' will not group the names by the join_keys topological structure.
#'
#' In order to rename objects in the `teal_data` object, use base R functions (see examples).
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
#' # hidden objects with dot-prefix
#' td <- within(td, .CO2 <- CO2)
#' names(td) # '.CO2' will not be returned
#'
#' # rename objects
#' td <- teal_data(iris = iris)
#' td <- within(td, {
#'   new_iris <- iris
#'   rm(iris)
#' })
#' names(td) # only 'new_iris' will be returned
#'
#' @export
names.teal_data <- function(x) {
  # Sorting from `ls` can be safely done as environments don't have any order
  # nor support numeric-index subsetting
  envir <- as.environment(x)
  .get_sorted_names(names = ls(envir = envir), join_keys = join_keys(x), env = envir)
}

#' @export
length.teal.data <- function(x) length(ls(x))

#' @keywords internal
.get_sorted_names <- function(names, join_keys, env) {
  child_parent <- sapply(names, parent, x = join_keys, USE.NAMES = TRUE, simplify = FALSE)

  union(
    intersect(unlist(topological_sort(child_parent)), ls(env, all.names = TRUE)),
    names
  )
}
