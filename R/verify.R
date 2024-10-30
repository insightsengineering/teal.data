#' Verify code reproducibility
#'
#' Checks whether code in `teal_data` object reproduces the stored objects.
#'
#' If objects created by code in the `@code` slot of `x` are `all_equal` to the
#' contents of the `@.xData` slot,
#' the function updates the `@verified` slot to `TRUE` in the returned `teal_data` object.
#' Once verified, the slot will always be set to `TRUE`.
#' If the `@code` fails to recreate objects in `teal_data`'s environment, an
#' error is raised.
#'
#' @return Input `teal_data` object or error.
#'
#' @param x `teal_data` object
#' @examples
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' verify(tdata1)
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
#' verify(tdata2)
#' verify(tdata2)@verified
#' tdata2@verified
#'
#' tdata3 <- teal_data()
#' tdata3 <- within(tdata3, {
#'   stop("error")
#' })
#' try(verify(tdata3)) # fails
#'
#'
#' a <- 1
#' b <- a + 2
#' c <- list(x = 2)
#' d <- 5
#' tdata4 <- teal_data(
#'   a = a, b = b, c = c, d = d,
#'   code = "a <- 1
#'           b <- a
#'           c <- list(x = 2)
#'           e <- 1"
#' )
#' tdata4
#' \dontrun{
#' verify(tdata4) # fails
#' }
#'
#' @name verify
#' @rdname verify
#' @aliases verify,teal_data-method
#' @aliases verify,qenv.error-method
#'
#' @export
setGeneric("verify", function(x) standardGeneric("verify"))
setMethod("verify", signature = "teal_data", definition = function(x) {
  if (x@verified) {
    return(x)
  }
  x_name <- deparse(substitute(x))
  y <- eval_code(teal_data(), get_code(x))

  if (inherits(y, "qenv.error")) {
    stop(conditionMessage(y), call. = FALSE)
  }

  reproduced <- isTRUE(all.equal(teal.code::get_env(x), teal.code::get_env(y)))
  if (reproduced) {
    x@verified <- TRUE
    methods::validObject(x)
    x
  } else {
    error <- "Code verification failed."

    objects_diff <- vapply(
      intersect(names(x), names(y)),
      function(element) {
        isTRUE(all.equal(x[[element]], y[[element]]))
      },
      logical(1)
    )

    names_diff_other <- setdiff(names(y), names(x))
    names_diff_inenv <- setdiff(names(x), names(y))

    if (length(objects_diff)) {
      error <- c(
        error,
        paste0("Object(s) recreated with code that have different structure in ", x_name, ":"),
        paste0("  \u2022 ", names(which(!objects_diff)))
      )
    }
    if (length(names_diff_inenv)) {
      error <- c(
        error,
        paste0("Object(s) not created with code that exist in ", x_name, ":"),
        paste0("  \u2022 ", names_diff_inenv)
      )
    }
    if (length(names_diff_other)) {
      error <- c(
        error,
        paste0("Object(s) created with code that do not exist in ", x_name, ":"),
        paste0("  \u2022 ", names_diff_other)
      )
    }

    stop(paste(error, collapse = "\n"), call. = FALSE)
  }
})
setMethod("verify", signature = "qenv.error", definition = function(x) {
  stop(conditionMessage(x), call. = FALSE)
})
