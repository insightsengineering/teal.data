#' Verify code reproducibility
#'
#' Checks whether code in `teal_data` object reproduces the stored objects.
#'
#' If objects created by code in the `@code` slo|t of `x` are `all_equal` to the contents of the `@env` slot,
#' the function updates the `@verified` slot to `TRUE` in the returned `teal_data` object.
#' Once verified, the slot will always be set to `TRUE`.
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
#' is_verified(tdata1)
#' verify(tdata2)
#' is_verified(verify(tdata2))
#'
#' tdata3 <- teal_data()
#' tdata3 <- within(tdata3, {
#'   stop("error")
#' })
#' try(verify(tdata3)) # fails
#'
#'
#' a <- 1
#' b <- 2
#' tdata4 <- teal_data(
#'   a = a, b = b,
#'   code = "a <- 1
#'           b <- 3"
#' )
#' tdata4
#' \dontrun{
#' verify(tdata4) # fails
#' }
#'
#' @name verify
#' @aliases verify,teal_data-method
#' @aliases verify,qenv.error-method
#' @aliases is_verified,teal_data-method
#' @aliases is_verified,qenv.error-method
NULL

#' @rdname verify
#' @export
setGeneric("verify", function(x) standardGeneric("verify"))
setMethod("verify", signature = "teal_data", definition = function(x) {
  if (is_verified(x)) {
    return(x)
  }
  x_name <- deparse(substitute(x))
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


#' @rdname verify
#' @export
setGeneric("is_verified", function(x) standardGeneric("is_verified"))
setMethod("is_verified", signature = "teal_data", definition = function(x) {
  x@verified
})
setMethod("is_verified", signature = "qenv.error", definition = function(x) {
  stop(conditionMessage(x), call. = FALSE)
})
