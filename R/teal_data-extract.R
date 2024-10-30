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
  checkmate::assert_character(names)
  possible_names <- ls(get_env(x))
  names_warn <- setdiff(names, possible_names)
  names <- intersect(names, possible_names)
  if (!length(names)) {
    warning("None of `names` elements exist in `teal_data`. Returning empty `teal_data`.")
    return(teal_data())
  }

  if (length(names_warn)) {
    warning(
      sprintf(
        "Some elements of `names` do not exist in `teal_data`. Skipping those: %s.",
        paste(names_warn, collapse = ", ")
      )
    )
  }

  x <- NextMethod("`[`", x) # takes 'names' from function's environment
  x@join_keys <- x@join_keys[names]

  x
}
