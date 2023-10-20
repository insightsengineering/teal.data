#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function is a wrapper around [teal_data()] and guesses `join_keys`
#' for given datasets which names match ADAM datasets names.
#'
#' @inheritParams teal_data
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys.
#'   For ADAM datasets it would be automatically derived.
#'
#' @return a `TealData` or `teal_data` object
#'
#' @details This function checks if there were keys added to all data sets
#'
#' @export
#'
#' @examples
#'
#' ADSL <- teal.data::example_cdisc_data("ADSL")
#' ADTTE <- teal.data::example_cdisc_data("ADTTE")
#'
#' cdisc_data(
#'   ADSL = ADSL,
#'   ADTTE = ADTTE,
#'   code = quote({
#'     ADSL <- teal.data::example_cdisc_data("ADSL")
#'     ADTTE <- teal.data::example_cdisc_data("ADTTE")
#'   }),
#'   join_keys = join_keys(
#'     join_key("ADSL", "ADTTE", c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))
#'   )
#' )
cdisc_data <- function(...,
                       join_keys = teal.data::join_keys_cdisc(...),
                       code = "",
                       check = FALSE) {
  data_objects <- list(...)

  # todo: is it really important? - to remove
  if (inherits(join_keys, "JoinKeySet")) {
    join_keys <- teal.data::join_keys(join_keys)
  }

  if (
    checkmate::test_list(data_objects, types = c("TealDataConnector", "TealDataset", "TealDatasetConnector"))
  ) {
    lifecycle::deprecate_warn(
      when = "0.3.1",
      "cdisc_data(
        data_objects = 'should use data directly. Using TealDatasetConnector and TealDataset is deprecated.'
      )"
    )
    update_join_keys_to_primary(data_objects, join_keys)

    new_parents_fun <- function(data_objects) {
      lapply(data_objects, function(x) {
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
      })
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
    join_keys$set_parents(new_parents)
    join_keys$update_keys_given_parents()

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
    new_teal_data(data = data_objects, code = code, keys = join_keys)
  }
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
