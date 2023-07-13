#' Set arguments of a `CallableFunction`
#'
#' @description `r lifecycle::badge("stable")`
#' Set arguments of a `CallableFunction`
#'
#' @param x `CallableFunction` or `TealDatasetConnector`)
#' @param args (`NULL` or named `list`) dynamic arguments to function
#'
#' @return nothing
#' @rdname set_args
#' @export
set_args <- function(x, args) {
  UseMethod("set_args")
}

#' @rdname set_args
#' @export
#' @examples
#' ## Using CallableFunction
#' fun <- callable_function(example_cdisc_data)
#' set_args(fun, list(df = "adsl"))
set_args.CallableFunction <- function(x, args) {
  x$set_args(args)
  return(invisible(x))
}

#' @rdname set_args
#' @export
#' @examples
#' ## Using CallableCode
#' code <- callable_code("example_cdisc_data()")
#' set_args(code, list(df = "adsl"))
set_args.CallableCode <- function(x, args) {
  warning(
    "'CallableCode' is unchangable. Ignoring arguments set by 'set_args'",
    call. = FALSE
  )
  return(invisible(x))
}

#' @rdname set_args
#' @export
#' @examples
#' ## Using TealDatasetConnector
#' ds <- dataset_connector("x", pull_callable = callable_function(data.frame))
#' set_args(ds, list(x = 1:5, y = letters[1:5]))
set_args.TealDatasetConnector <- function(x, args) {
  x$set_args(args)
  return(invisible(x))
}
