#' Get Code from `teal_data`
#'
#' Retrieve code from `teal_data` object.
#'
#' Retrieve code stored in `@code`, which (in principle) can be used to recreate all objects found in `@env`.
#' Use `datanames` to limit the code to one or more of the data sets enumerated in `@datanames`.
#' If the code has not passed verification (with [`verify()`]), a warning will be prepended.
#'
#' @section Notes for Developers:
#' To learn more about how a subset of code needed to reproduce a specific data set is extracted from all code,
#' see [`get_code_dependency()`].
#'
#' @param object (`teal_data`)
#' @param datanames (`character`) vector of data set names to return the code for.
#' @param deparse (`logical`) flag specifying whether to return code as `character` (`deparse = TRUE`) or as
#' `expression` (`deparse = FALSE`).
#' @return
#' Either string or an expression representing code used to create the requested data sets.
#' @examples
#'
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' get_code(tdata1)
#' datanames(tdata1) <- c("a", "b", "c")
#' get_code(tdata1, datanames = "a")
#' get_code(tdata1, datanames = "b")
#'
#' tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
#' get_code(tdata2)
#' get_code(verify(tdata2))
#' @aliases get_code,teal_data-method
#' @aliases get_code
#' @export
setMethod("get_code", "teal_data", definition = function(object, deparse = TRUE, datanames = NULL) {
  checkmate::assert_character(datanames, min.len = 1L, null.ok = TRUE)
  checkmate::assert_subset(datanames, datanames(object))
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
