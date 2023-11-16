#' Show `teal_data` object
#'
#' Prints `teal_data` object
#' @param object (`teal_data`)
#' @return nothing
#' @importFrom methods show
#' @examples
#' teal_data()
#' teal_data(x = iris, code = "x = iris")
#' verify(teal_data(x = iris, code = "x = iris"))
#' @export
setMethod("show", "teal_data", function(object) {
  if (object@verified) cat("✅︎", "verified teal_data object\n")
  rlang::env_print(object@env)
})
