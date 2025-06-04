#' Show `teal_data` object
#'
#' Prints `teal_data` object.
#'
#' @param object (`teal_data`)
#' @return Input `teal_data` object.
#' @importFrom methods show
#' @examples
#' teal_data()
#' teal_data(x = iris, code = "x = iris")
#' verify(teal_data(x = iris, code = "x = iris"))
#' @export
setMethod("show", signature = "teal_data", function(object) {
  if (object@verified) {
    cat("\u2705\ufe0e", "code verified\n")
  } else {
    cat("\u2716", "code unverified\n")
  }
  methods::callNextMethod(object)
  invisible(object)
})
