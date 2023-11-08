#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function is a wrapper around [teal_data()] and guesses `join_keys`
#' for given datasets whose names match ADAM datasets names.
#'
#' @inheritParams teal_data
#' @param join_keys (`JoinKeys`) or a single (`join_key_set`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys.
#'   For ADAM datasets it would be automatically derived.
#'
#' @return
#' - a `TealData` object when `TealDataset` `TealDatasetConnector`, `TealDataConnector` is provided,
#' - a `teal_data` object otherwise.
#'
#' @details This function checks if there were keys added to all data sets
#'
#' @export
#'
#' @examples
#' data <- cdisc_data(
#'   join_keys = join_keys(
#'     join_key("ADSL", "ADTTE", c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))
#'   )
#' )
#'
#' data <- within(data, {
#'   ADSL <- teal.data::example_cdisc_data("ADSL")
#'   ADTTE <- teal.data::example_cdisc_data("ADTTE")
#' })
#'
cdisc_data <- function(...,
                       join_keys = teal.data::cdisc_join_keys(...),
                       code = character(0),
                       check = FALSE) {
  teal_data(..., join_keys = join_keys, code = code, check = check)
}

#' Extrapolate parents from `TealData` classes
#'
#' `r lifecycle::badge("deprecated")`
#'
#' note: This function will be removed once the following classes are defunct:
#'  `TealDataConnector`, `TealDataset`, `TealDatasetConnector`
#'
#' @keywords internal
deprecated_join_keys_extract <- function(data_objects, join_keys) {
  if (
    !checkmate::test_list(
      data_objects,
      types = c("TealDataConnector", "TealDataset", "TealDatasetConnector")
    )
  ) {
    return(join_keys)
  }

  # TODO: check if redundant with same call in teal_data body
  join_keys <- update_join_keys_to_primary(data_objects, join_keys)

  new_parents_fun <- function(data_objects) {
    lapply(
      data_objects,
      function(x) {
        if (inherits(x, "TealDataConnector")) {
          unlist(new_parents_fun(x$get_items()), recursive = FALSE)
        } else {
          list(
            tryCatch(
              x$get_parent(),
              error = function(cond) rep(character(0), length(x$get_datanames()))
            )
          )
        }
      }
    )
  }

  new_parents <- unlist(new_parents_fun(data_objects), recursive = FALSE)

  names(new_parents) <- unlist(lapply(data_objects, function(x) {
    if (inherits(x, "TealDataConnector")) {
      lapply(x$get_items(), function(y) y$get_dataname())
    } else {
      x$get_datanames()
    }
  }))

  if (is_dag(new_parents)) {
    stop("Cycle detected in a parent and child dataset graph.")
  }

  parents(join_keys) <- new_parents
  join_keys <- update_keys_given_parents(join_keys)

  join_keys
}

#' Load `TealData` object from a file
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' @inheritParams teal_data_file
#'
#' @return `TealData` object
#'
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "# code>
#'      ADSL <- teal.data::example_cdisc_data('ADSL')
#'      ADTTE <- teal.data::example_cdisc_data('ADTTE')
#'
#'      cdisc_data(
#'           cdisc_dataset(\"ADSL\", ADSL), cdisc_dataset(\"ADTTE\", ADTTE),
#'           code = \"ADSL <- teal.data::example_cdisc_data('ADSL')
#'                   ADTTE <- teal.data::example_cdisc_data('ADTTE')\",
#'           check = FALSE
#'      )
#'      # <code"
#'   ),
#'   con = file_example
#' )
#'
#' cdisc_data_file(file_example)
cdisc_data_file <- function(path, code = get_code(path)) {
  lifecycle::deprecate_warn(when = "0.1.3", what = "cdisc_data_file()", with = "teal_data_file()")
  object <- object_file(path, "TealData")
  object$mutate(code)
  return(object)
}
