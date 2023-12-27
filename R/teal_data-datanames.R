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
#'
#' datanames(td) <- c("iris", "mtcars")
#' datanames(td)
#'
#' @name datanames
#' @aliases datanames,teal_data-method
#' @aliases datanames<-,teal_data,character-method
#' @aliases datanames,qenv.error-method
#' @aliases datanames<-,qenv.error,character-method

#' @rdname datanames
#' @export
setGeneric("datanames", function(x) standardGeneric("datanames"))
setMethod("datanames", signature = "teal_data", definition = function(x) {
  x@datanames
})
setMethod("datanames", signature = "qenv.error", definition = function(x) {
  NULL
})

#' @rdname datanames
#' @export
setGeneric("datanames<-", function(x, value) standardGeneric("datanames<-"))
setMethod("datanames<-", signature = c("teal_data", "character"), definition = function(x, value) {
  checkmate::assert_subset(value, names(x@env))
  x@datanames <- value
  methods::validObject(x)
  x
})
setMethod("datanames<-", signature = c("qenv.error", "character"), definition = function(x, value) {
  methods::validObject(x)
  x
})
