#' Check Code Reproducibility
#'
#' Checks whether code in `teal_data` reproduces the stored objects.
#'
#' @return If the objects returned by the `@code` slot in the input `teal_data` object exactly match
#' the objects in `teal_data@env`, the function updates the `@verified` slot to `TRUE` in the `teal_data` object.
#' If the `@code` fails to recreate objects in `teal_data@env`, an error is thrown.
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
#' verify(tdata3)
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

  if (inherits(new_teal_data, "qenv.error")) {
    return(stop(new_teal_data@message, call. = FALSE))
  }

  reproduced <- isTRUE(all.equal(x@env, new_teal_data@env))
  if (reproduced) {
    x@verified <- TRUE
    methods::validObject(x)
    x
  } else {

    names_diff <- setdiff(names(x@env), names(new_teal_data@env))

    objects_diff <- vapply(
      intersect(names(x@env), names(new_teal_data@env)),
      function(element) {
        isTRUE(all.equal(x@env[[element]], new_teal_data@env[[element]]))
      },
      logical(1)
    )

    stop("Code verification failed at object(s):\n", paste(c(names_diff, names(which(!objects_diff))), collapse = '\n'))
  }
})
setMethod("verify", "qenv.error", definition = function(x) {
  stop(x$message, call. = FALSE)
})
