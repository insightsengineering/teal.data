#' Show `teal_data` object
#'
#' Prints `teal_data` object.
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
    cat("\u2705\ufe0e", "verified teal_data object\n")
  } else {
    cat("\u2716", "unverified teal_data object\n")
  }
  rlang::env_print(object@env)
})
