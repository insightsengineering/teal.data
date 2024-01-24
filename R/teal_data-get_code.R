#' Get code from `teal_data` object
#'
#' Retrieve code from `teal_data` object.
#'
#' Retrieve code stored in `@code`, which (in principle) can be used to recreate all objects found in `@env`.
#' Use `datanames` to limit the code to one or more of the datasets enumerated in `@datanames`.
#' If the code has not passed verification (with [`verify()`]), a warning will be prepended.
#'
#' @section Extracting dataset-specific code:
#' When `datanames` is specified, the code returned will be limited  to the lines needed to _create_
#' the requested datasets. The code stored in the `@code` slot is analyzed statically to determine
#' which lines the datasets of interest depend upon. The analysis works well when objects are created
#' with standard infix assignment operators (see `?assignOps`) but it can fail in some situations.
#'
#' Consider the following examples:
#'
#' _Case 1: Usual assignments._
#' ```r
#' data <- teal_data() |>
#'   within({
#'     foo <- function(x) {
#'       x + 1
#'     }
#'     x <- 0
#'     y <- foo(x)
#'   })
#' get_code(data, datanames = "y")
#' ```
#' `x` has no dependencies, so `get_code(data, datanames = "x")` will return only the second call.\cr
#' `y` depends on `x` and `foo`, so `get_code(data, datanames = "y")` will contain all three calls.
#'
#' _Case 2: Some objects are created by a function's side effects._
#' ```r
#' data <- teal_data() |>
#'   within({
#'     foo <- function() {
#'       x <<- x + 1
#'     }
#'     x <- 0
#'     foo()
#'     y <- x
#'   })
#' get_code(data, datanames = "y")
#' ```
#' Here, `y` depends on `x` but `x` is modified by `foo` as a side effect (not by reassignment)
#' and so `get_code(data, datanames = "y")` will not return the `foo()` call.\cr
#' To overcome this limitation, code dependencies can be specified manually.
#' Lines where side effects occur can be flagged by adding "`# @linksto <object name>`" at the end.\cr
#' Note that `within` evaluates code passed to `expr` as is and comments are ignored.
#' In order to include comments in code one must use the `eval_code` function instead.
#'
#' ```r
#' data <- teal_data() |>
#'   eval_code("
#'     foo <- function() {
#'       x <<- x + 1
#'     }
#'     x <- 0
#'     foo() # @linksto x
#'     y <- x
#'   ")
#' get_code(data, datanames = "y")
#' ```
#' Now the `foo()` call will be properly included in the code required to recreate `y`.
#'
#' Note that two functions that create objects as side effects, `assign` and `data`, are handled automatically.
#'
#' Here are known cases where manual tagging is necessary:
#' - non-standard assignment operators, _e.g._ `%<>%`
#' - objects used as conditions in `if` statements: `if (<condition>)`
#' - objects used to iterate over in `for` loops: `for(i in <sequence>)`
#' - creating and evaluating language objects, _e.g._ `eval(<call>)`
#'
#'
#' @param object (`teal_data`)
#' @param datanames `r lifecycle::badge("experimental")` (`character`) vector of dataset names to return the code for.
#' For more details see the "Extracting dataset-specific code" section.
#' @param deparse (`logical`) flag specifying whether to return code as `character` (`deparse = TRUE`) or as
#' `expression` (`deparse = FALSE`).
#'
#' @return
#' Either a character string or an expression. If `datanames` is used to request a specific dataset,
#' only code that _creates_ that dataset (not code that uses it) is returned. Otherwise, all contents of `@code`.
#'
#' @examples
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' get_code(tdata1)
#' get_code(tdata1, datanames = "a")
#' get_code(tdata1, datanames = "b")
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
#' get_code(tdata2)
#' get_code(verify(tdata2))
#'
#' @rdname get_code
#' @aliases get_code,teal_data-method
#'
#' @export
setMethod("get_code", signature = "teal_data", definition = function(object, deparse = TRUE, datanames = NULL) {
  checkmate::assert_character(datanames, min.len = 1L, null.ok = TRUE)
  checkmate::assert_flag(deparse)

  code <- if (!is.null(datanames)) {
    get_code_dependency(object@code, datanames)
  } else {
    object@code
  }

  if (!object@verified) {
    code <- c("warning('Code was not verified for reproducibility.')", code)
  }

  if (deparse) {
    if (length(code) == 0) {
      code
    } else {
      paste(code, collapse = "\n")
    }
  } else {
    parse(text = paste(c("{", code, "}"), collapse = "\n"), keep.source = TRUE)
  }
})
