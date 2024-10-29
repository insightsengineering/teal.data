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
  checkmate::assert_class(names, "character")
  names_in_env <- intersect(names, ls(get_env(x)))
  if (!length(names_in_env)) {
    return(teal_data())
  }
  # From ?NextMethod
  # To pass `names` to `NextMethod` - it looks like it needs to be called `names`
  # and created in the environment of the function that calls `NextMehod`.

  # NextMethod works by creating a special call frame for the next method. If no new arguments are supplied, the
  # arguments will be the same in number, order and name as those to the current method but their values will be
  # promises to evaluate their name in the current method and environment. Any named arguments matched to ... are
  # handled specially: they either replace existing arguments of the same name or are appended to the argument list.
  # They are passed on as the promise that was supplied as an argument to the current environment.
  # (S does this differently!) If they have been evaluated in the current (or a previous environment) they remain
  # evaluated. (This is a complex area, and subject to change: see the draft ‘R Language Definition’.)

  names <- names_in_env
  x <- NextMethod("`[`", x)
  x@join_keys <- x@join_keys[names_in_env]

  x
}
