#' Get Code from `teal_data`
#'
#' Get code from [`teal_data`]. The extraction of the code limited to specific objects can be performed with the usage
#' of `names` parameter. If input `teal_data@verified` is `FALSE` the returned code is pre-pended with a warning
#' stating that the object was not verified.
#'
#' @param object `teal_data`
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
#' get_code(tdata1, names = "a")
#' get_code(tdata1, names = "b")
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 = iris")
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
