#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function takes datasets and creates `TealData` object which can be used in `teal` applications.
#'
#' @note This function does not automatically assign keys to `TealDataset`
#' and `TealDatasetConnector` objects passed to it. If the keys are needed
#' they should be assigned before calling `cdisc_data`. See example:
#' ```
#' test_dataset <- dataset("ADAE", teal.data::example_cdisc_data("ADAE")) # does not have keys
#' test_adsl <- cdisc_dataset("ADSL", teal.data::example_cdisc_data("ADSL"))
#' test_data <- cdisc_data(test_dataset, test_adsl)
#' get_keys(test_data, "ADAE") # returns character(0)
#'
#' test_dataset <- cdisc_dataset("ADAE", teal.data::example_cdisc_data("ADAE"))
#' test_data <- cdisc_data(test_dataset, test_adsl)
#' get_keys(test_data, "ADAE") # returns [1] "STUDYID" "USUBJID" "ASTDTM"  "AETERM"  "AESEQ"
#' ```
#' @inheritParams teal_data
#' @param ... (`TealDataConnector`, `TealDataset` or
#'   `TealDatasetConnector`) elements to include.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#'
#' @return a `TealData` object
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
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = "ADSL <- teal.data::example_cdisc_data(\"ADSL\")
#'           ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")",
#'   check = TRUE
#' )
#'
#' # Example with keys
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL,
#'     keys = c("STUDYID", "USUBJID")
#'   ),
#'   cdisc_dataset("ADTTE", ADTTE,
#'     keys = c("STUDYID", "USUBJID", "PARAMCD"),
#'     parent = "ADSL"
#'   ),
#'   join_keys = join_keys(
#'     join_key(
#'       "ADSL",
#'       "ADTTE",
#'       c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID")
#'     )
#'   ),
#'   code = "ADSL <- teal.data::example_cdisc_data(\"ADSL\")
#'           ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")",
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       join_keys = teal.data::join_keys(),
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

    update_join_keys_to_primary(data_objects, join_keys)

    retrieve_parents <- function(x) {lifecycle::deprecate_warn(
      when = "0.3.1",
      "cdisc_data(
        data_objects = 'should use data directly. Using TealDatasetConnector and TealDataset is deprecated.'
      )"
    )
      tryCatch(
        x$get_parent(),
        error = function(cond) rep(character(0), length(x$get_datanames()))
      )
    }

    new_parents_fun <- function(data_objects) {
      lapply(data_objects, function(x) {
        if (inherits(x, "TealDataConnector")) {
          unlist(new_parents_fun(x$get_items()), recursive = FALSE)
        } else {
          list(retrieve_parents(x))
        }
      })
    }

    new_parents <- unlist(new_parents_fun(data_objects), recursive = FALSE)

    names(new_parents) <- unlist(lapply(data_objects, function(x) {
      if (inherits(x, "TealDataConnector")) {
        lapply(x$get_items(), function(z) z$get_dataname())
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

    if (is_pulled(x)) {
      new_tdata(
        env = lapply(x$get_datasets(), function(x) x$get_raw_data()),
        code = x$get_code(),
        keys = x$get_join_keys()
      )
    } else {
      x
    }
  } else {
    if (!checkmate::test_names(names(data_objects), type = "named")) {
      stop("Dot (`...`) arguments on `teal_data()` must be named.")
    }
    new_tdata(env = data_objects, code = code, keys = join_keys)
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
