#' Get Code from `teal_data`
#'
#' Get code from [`teal_data`]. The extraction of the code limited to specific objects can be performed with the usage
#' of `names` parameter. If input `teal_data@verified` is `FALSE` the returned code is pre-pended with a warning
#' stating that the object was not verified.
#'
#' @param x [`teal_data`] object.
#' @param names (`character`) vector of object names to return the code for.
#' @param deparse (`logical`) whether return deparsed form of a call.
#' @return (`character`) code of import and preparation of data for `teal` application.
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
#' get_code(tdata1, "a")
#' get_code(tdata1, "b")
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 = iris")
#' get_code(tdata2)
#' get_code(verify(tdata2))
#' @aliases get_code,teal_data-method
#' @export
setGeneric("get_code", function(x, ...) standardGeneric("get_code"))
setMethod("get_code", "teal_data", definition = function(x, names = NULL, deparse = TRUE) {

  checkmate::assert_character(names, min.len = 1L, null.ok = TRUE)
  checkmate::assert_subset(names, x@datanames, empty.ok = TRUE)
  checkmate::assert_flag(deparse)

  code <- if (!is.null(names)) {
    get_code_dependency(x@code, names)
  } else {
    x@code
  }

  if (!x@verified) {
    code <- c("warning('Code was not verified for reproducibility.')", code)
  }

  if (deparse) {
    code
  } else {
    parse(text = code, keep.source = TRUE)
  }
})
