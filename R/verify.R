#' Reproducibility Check for `@code` Slot in `teal_data`
#'
#' Verifies the reproducibility of the `teal_data@env` environment using the `@code` slot.
#'
#' @return If the objects returned by the `@code` slot in the input `teal_data` object exactly match
#' the objects in `teal_data@env`, the function updates the `@verified` slot to `TRUE` in the `teal_data` object.
#' If the `@code` fails to recreate objects in `teal_data@env`, an error is thrown.
#'
#' @param teal_data `teal_data` object
#' @examples
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' verify(tdata1)
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 = iris")
#' verify(tdata2)
#' verify(tdata2)@verified
#' tdata2@verified
#'
#' \dontrun{
#' tdata3 <- teal_data()
#' tdata3 <- within(tdata3, {
#'   stop("error")
#' })
#' }
#' @name verify
#' @aliases verify,teal_data-method
#' @aliases verify,qenv.error-method
#' @rdname verify
#' @export
setGeneric("verify", function(x) standardGeneric("verify"))
setMethod("verify", "teal_data", definition = function(x) {
  if (x@verified) {
    return(x)
  }
  new_teal_data <- eval_code(teal_data(), x@code)

  reproducible <- all.equal(x@env, new_teal_data@env)
  if (isTRUE(reproducible)) {
    x@verified <- TRUE
    x
  } else {
    stop("Code verification failed.")
  }
})
setMethod("verify", "qenv.error", definition = function(x) {
  stop(x$trace)
})
