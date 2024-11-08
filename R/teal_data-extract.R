#'
#' @section Subsetting:
#' `x[names]` subsets objects in `teal_data` environment and limit the code to the necessary needed to build limited
#' objects.
#'
#' @param names (`character`) names of objects included in `teal_subset` to subset
#' @param x (`teal_data`)
#'
#' @examples
#'
#' # Subsetting
#' data <- teal_data()
#' data <- eval_code(data, "a <- 1;b<-2")
#' data["a"]
#' data[c("a", "b")]
#'
#' join_keys(data) <- join_keys(join_key("a", "b", "x"))
#' join_keys(data["a"]) # should show empty keys
#' join_keys(data["b"])
#' join_keys(data)["a"] # should show empty keys
#' join_keys(data)["b"]
#'
#' @rdname teal_data
#'
#' @export
`[.teal_data` <- function(x, names) {
  x <- NextMethod("`[`", x, check_names = FALSE) # takes 'names' from function's environment
  if (inherits(x, "qenv")) return(teal_data()) # all 'names' not in object
  x@join_keys <- x@join_keys[names]
  x
}
