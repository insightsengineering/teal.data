#' Get code from `teal_data` object
#'
#' Retrieve code from `teal_data` object.
#'
#' Retrieve code stored in `@code`, which (in principle) can be used to recreate all objects found in `@env`.
#' Use `datanames` to limit the code to one or more of the data sets enumerated in `@datanames`.
#' If the code has not passed verification (with [`verify()`]), a warning will be prepended.
#'
#' @section Extracting dataset-specific code:
#' When `datanames` is specified `get_code` will limits the output only to the lines of code needed
#' to recreate the requested data sets. `code` stored in the `teal_data` object is analyzed statically
#' to determine dependency tree between each line of the code. Analysis is performed automatically
#' based on the used symbols and it is working in a standard case when a new dataset is created by
#' the assignment operator.
#'
#' For example:
#' ```r
#' data <- teal_data() |>
#'   within(
#'     foo <- function(x) x + 1
#'     x <- 0
#'     y <- foo(x)
#'   )
#' get_code(data, datanames = "y")
#' ```
#'
#' In above case `y` depends on `x` and `foo` so the code returned by `get_code` will contain all
#' three lines of code. `get_code(data, datanames = "x")` will return only the second line of code etc.
#' \cr
#' When a code uses non-standard evaluation `get_code` won't be able to determine relationships
#' between each calls. Consider the case where `y` depends on `x` but `x` is not created by
#' assignment operator. In such cases `get_code(data, y)` will only return the second line of the code:
#' ```r
#' data <- teal_data() |>
#'   within(
#'     foo <- function() {
#'       env <- parent.frame()
#'       env$x <- 0
#'     }
#'     foo()
#'     y <- x
#'   )
#' get_code(data, datanames = "y")
#' ```
#'
#' To overcome limitation from above example `get_code` allows to specify dependencies manually.
#' Adding `# @linksto x` at the end of a line where a non-standard evaluation occurs will "flag"
#' this line as dependent on `x`.
#' NOTE: `expr` passed to `within` function discards comments. To add a code with comments to
#' `teal_data` object use `eval_code` function.
#'
#' ```r
#' data <- teal_data() |>
#'   eval_code("
#'     foo <- function() {
#'       env <- parent.frame()
#'       env$x <- 0
#'     }
#'     foo() # @linksto x
#'     y <- x
#'   ")
#' get_code(data, datanames = "y")
#' ```
#'
#'
#' @param object (`teal_data`)
#' @param datanames `r lifecycle::badge("experimental")` (`character`) vector of data set names to return the code for.
#' @param deparse (`logical`) flag specifying whether to return code as `character` (`deparse = TRUE`) or as
#' `expression` (`deparse = FALSE`).
#'
#' @return
#' Either string or an expression representing code used to create the requested data sets.
#'
#' @examples
#'
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
#' @aliases get_code,teal_data-method
#' @aliases get_code
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
