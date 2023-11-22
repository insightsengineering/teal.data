#' Get Code from `teal_data`
#'
#' Retrieve code from `teal_data` object.
#'
#' Retrieve code stored in `@code`, which (in principle) can be used to recreate all objects found in `@env`.
#' Use `names` to limit the code to one or more of the data sets enumerated in `@datanames`.
#' If the code has not passed verification, a warning will be prepended.
#'
#' @param object `teal_data`
#' @param names (`character`) vector of data set names to return the code for.
#' @param deparse (`logical`) whether return deparsed form of a call.
#' @return
#' Either a character vector or expression representing code used to create the requested data sets.
#' @examples
#'
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' get_code(tdata1)
#' datanames(tdata1) <- c("a", "b", "c")
#' get_code(tdata1, names = "a")
#' get_code(tdata1, names = "b")
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
#' get_code(tdata2)
#' get_code(verify(tdata2))
#' @aliases get_code,teal_data-method
#' @export
setMethod("get_code", "teal_data", definition = function(object, deparse = TRUE, names = NULL) {
  checkmate::assert_character(names, min.len = 1L, null.ok = TRUE)
  checkmate::assert_subset(names, datanames(object))
  checkmate::assert_flag(deparse)

  code <- if (!is.null(names)) {
    get_code_dependency(object@code, names)
  } else {
    object@code
  }

  if (!object@verified) {
    code <- c("warning('Code was not verified for reproducibility.')", code)
  }

  if (deparse) {
    code
  } else {
    parse(text = code, keep.source = TRUE)
  }
})
