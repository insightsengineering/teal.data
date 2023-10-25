#' Names of Data Sets in `teal_data` Object
#'
#' Get or set the value of the `datanames` slot.
#'
#' The `@datanames` slot in a `teal_data` object specifies which of the variables stored in its environment
#' (the `@env` slot) are data sets to be taken into consideration.
#' The contents of `@datanames` can be specified upon creation and default to all variables in `@env`.
#' Variables created later, which may well be data sets, are not automatically considered such.
#' Use this function to update the slot.
#'
#' @param x (`teal_data`) object to access or modify
#' @param value (`character`) new value for `@datanames`; all elements must be names of variables existing in `@env`
#'
#' @return The contents of `@datanames` or `teal_data` with updated `@datanames`.
#'
#' @examples
#' td <- teal_data(iris = iris)
#' td <- within(td, mtcars <- mtcars)
#' datanames(td)
#' datanames(td) <- c("iris", "mtcars")
#'
#' @export
#'
setGeneric("datanames", function(x) standardGeneric("datanames"))
setMethod("datanames", "teal_data", definition = function(x) {
  x@datanames
})

#' @rdname datanames
#' @export
#'
setGeneric("datanames<-", function(x, value) standardGeneric("datanames<-"))
setMethod("datanames<-", c("teal_data", "character"), definition = function(x, value) {
  if (!all(is.element(value, ls(x@env, all.names = TRUE)))) {
    stop("invalid name")
  }
  x@datanames <- value
  methods::validObject(x)
  x
})
