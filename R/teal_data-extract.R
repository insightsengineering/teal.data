#'
#' @describeIn teal_data Subsetting
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
#' @export
`[.teal_data` <- function(x, names) {
  checkmate::assert_class(names, "character")
  names_in_env <- intersect(names, ls(get_env(x)))
  if (!length(names_in_env)) {
    return(teal_data())
  }

  subset_qenv <- utils::getFromNamespace("[.qenv", "teal.code")
  x <- subset_qenv(x, names_in_env)
  x@join_keys <- x@join_keys[names_in_env]

  x
}
