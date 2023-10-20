#' Teal data
#'
#' @description `r lifecycle::badge("stable")`
#' Universal function to pass data to teal application
#'
#' @param ... (`TealDataConnector`, `TealDataset`, `TealDatasetConnector`, `any`)\cr
#'  - When one of the `Teal*` objects are provided, then function returns `TealData` object.
#'    This way of specifying data is deprecated and will be removed in the next release.
#'  - From version 0.4.0, one can provide any object as a named argument and function will
#'    return `teal_data` object. Objects provided in `...` will be stored in `teal_data` environment
#'    under the same name as the argument name.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then no joins between pairs of objects
#' @param code (`character`) code to reproduce the datasets.
#' @param check (`logical`) reproducibility check - whether to perform a check that the pre-processing
#'  code included in the object definitions actually produces those objects.
#'  If `check` is true and preprocessing code is empty an error will be thrown.
#'
#' @return (`TealData` or `teal_data`) object
#'
#' @export
#'
#' @examples
#'
#' teal_data(
#'   x1 = iris,
#'   x2 = mtcars,
#'   code = quote({
#'     x1 <- iris
#'     x2 <- mtcars
#'   })
#' )
teal_data <- function(...,
                      join_keys = NULL,
                      code = "",
                      check = FALSE) {
  data_objects <- list(...)
  if (inherits(join_keys, "JoinKeySet")) {
    join_keys <- teal.data::join_keys(join_keys)
  }
  if (
    checkmate::test_list(data_objects, types = c("TealDataConnector", "TealDataset", "TealDatasetConnector"))
  ) {
    lifecycle::deprecate_warn(
      when = "0.3.1",
      "teal_data(
        data_objects = 'should use data directly. Using TealDatasetConnector and TealDataset is deprecated.
        Find more information on https://github.com/insightsengineering/teal/discussions/945'
      )"
    )
    update_join_keys_to_primary(data_objects, join_keys)

    x <- TealData$new(..., check = check, join_keys = join_keys)
    if (length(code) > 0 && !identical(code, "")) {
      x$set_pull_code(code = code)
    }
    x$check_reproducibility()
    x$check_metadata()
    x
  } else {
    if (!checkmate::test_names(names(data_objects), type = "named")) {
      stop("Dot (`...`) arguments on `teal_data()` must be named.")
    }
    new_teal_data(
      data = data_objects,
      code = code,
      keys = join_keys
    )
  }
}

#' Load `TealData` object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path A (`connection`) or a (`character`)\cr
#'   string giving the pathname of the file or URL to read from. "" indicates the connection `stdin`.
#' @param code (`character`)\cr
#'   reproducible code to re-create object
#'
#' @return `TealData` object
#'
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal.data)
#'
#'      x1 <- dataset(dataname = \"IRIS\",
#'                    x = iris,
#'                    code = \"IRIS <- iris\")
#'
#'      x2 <- dataset(dataname = \"MTCARS\",
#'                    x = mtcars,
#'                    code = \"MTCARS <- mtcars\")
#'
#'      teal_data(x1, x2)"
#'   ),
#'   con = file_example
#' )
#' teal_data_file(file_example, code = character(0))
teal_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "TealData")
  object$mutate(code)
  return(object)
}

#' Add primary keys as join_keys to a dataset self
#'
#' @param data_objects (`list`) of `TealDataset`, `TealDatasetConnector` or `TealDataConnector` objects
#' @param join_keys (`JoinKeys`) object
#'
#' @keywords internal
update_join_keys_to_primary <- function(data_objects, join_keys) {
  lapply(data_objects, function(obj) {
    if (inherits(obj, "TealDataConnector")) {
      update_join_keys_to_primary(obj$get_items(), join_keys)
    } else {
      dataname <- obj$get_dataname()
      if (length(join_keys$get(dataname, dataname)) == 0) {
        join_keys$mutate(
          dataname,
          dataname,
          obj$get_keys()
        )
      }
    }
  })
}
