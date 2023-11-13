#' Reproducibility Check for `@code` Slot in `teal_data`
#'
#' Validates the reproducibility of the `teal_data@env` environment using the `@code` slot.
#'
#' @return If the objects returned by the `@code` slot in the input `teal_data` object exactly match
#' the objects in `teal_data@env`, the function updates the `@valid` slot to `TRUE` in the `teal_data` object.
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
#' is_reproducible(tdata1)
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 = iris")
#' is_reproducible(tdata2)
#' is_reproducible(tdata2)@valid
#' tdata2@valid
#'
#' @export
is_reproducible <- function(teal_data) {
  checkmate::assert_class(teal_data, "teal_data")
  if (teal_data@valid) {
    return(teal_data)
  }
  eval_env <- new.env()
  eval(parse(text = teal_data@code), envir = eval_env)

  reproducible <- all.equal(teal_data@env, eval_env)
  if (reproducible) {
    teal_data@valid <- TRUE
    teal_data
  } else {
    stop("Code validation failed.")
  }
}
