#' Check Code Reproducibility
#'
#' Checks whether code in `teal_data` reproduces the stored objects.
#'
#' If objects returned by the `@code` slot in the input `teal_data` exactly match objects in `teal_data@env`,
#' the function updates the `@verified` slot to `TRUE` in the returned `teal_data` object. Once verified, the slot
#' will always be set to `TRUE`.
#' If the `@code` fails to recreate objects in `teal_data@env`, an error is raised.
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
#' \dontrun{
#' tdata3 <- teal_data()
#' tdata3 <- within(tdata3, {
#'   stop("error")
#' })
#' verify(tdata3)
#'
#'
#' a <- 1
#' b <- a + 2
#' c <- list(x = 2)
#' d <- 5
#' tdata4 <- teal_data(
#'   a = a, b = b, c = c, d = d,
#'   code =
#'     "
#'     a <- 1
#'     b <- a
#'     c <- list(x = 2)
#'     e <- 1
#'     "
#' )
#' tdata4
#' verify(tdata4)
#' }
#'
#' @name verify
#' @rdname verify
#' @aliases verify,teal_data-method
#' @aliases verify,qenv.error-method
#'
#' @export
setGeneric("verify", function(x) standardGeneric("verify"))
setMethod("verify", "teal_data", definition = function(x) {
  if (x@verified) {
    return(x)
  }
  y <- eval_code(teal_data(), get_code(x))

  if (inherits(y, "qenv.error")) {
    stop(conditionMessage(y), call. = FALSE)
  }

  reproduced <- isTRUE(all.equal(x@env, y@env))
  if (reproduced) {
    x@verified <- TRUE
    methods::validObject(x)
    x
  } else {
    error <- "Code verification failed."

    objects_diff <- vapply(
      intersect(names(x@env), names(y@env)),
      function(element) {
        isTRUE(all.equal(x@env[[element]], y@env[[element]]))
      },
      logical(1)
    )

    names_diff_other <- setdiff(names(y@env), names(x@env))
    names_diff_inenv <- setdiff(names(x@env), names(y@env))

    if (length(objects_diff)) {
      error <- c(
        error,
        "Object(s) recreated with code that have different structure in teal_data:",
        paste0("  \u2022 ", names(which(!objects_diff)))
      )
    }
    if (length(names_diff_inenv)) {
      error <- c(
        error,
        "Object(s) not created with code that exist in teal_data:",
        paste0("  \u2022 ", names_diff_inenv)
      )
    }
    if (length(names_diff_other)) {
      error <- c(
        error,
        "Object(s) created with code that do not exist in teal_data:",
        paste0("  \u2022 ", names_diff_other)
      )
    }

    stop(paste(error, collapse = "\n"))
  }
})
setMethod("verify", "qenv.error", definition = function(x) {
  stop(conditionMessage(x), call. = FALSE)
})
