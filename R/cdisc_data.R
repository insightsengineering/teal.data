#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function takes datasets and creates `CDISCTealData` object which can be used in `teal` applications.
#'
#' @note This function does not automatically assign keys to `TealDataset`
#' and `TealDatasetConnector` objects passed to it. If the keys are needed
#' they should be assigned before calling `cdisc_data`. See example:
#' ```
#' library(scda)
#' test_dataset <- dataset("ADAE", synthetic_cdisc_data("latest")$adae) # does not have keys
#' test_adsl <- cdisc_dataset("ADSL", synthetic_cdisc_data("latest")$adsl)
#' test_data <- cdisc_data(test_dataset, test_adsl)
#' get_keys(test_data, "ADAE") # returns character(0)
#'
#' test_dataset <- cdisc_dataset("ADAE", synthetic_cdisc_data("latest")$adae)
#' test_data <- cdisc_data(test_dataset, test_adsl)
#' get_keys(test_data, "ADAE") # returns [1] "STUDYID" "USUBJID" "ASTDTM"  "AETERM"  "AESEQ"
#' ```
#' @inheritParams teal_data
#' @param ... (`TealDataConnector`, `TealDataset` or
#'   `TealDatasetConnector`) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#'
#' @return a `CDISCTealData` object
#'
#' @details This function checks if there were keys added to all data sets
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' latest_data <- synthetic_cdisc_data("latest")
#' ADSL <- latest_data$adsl
#' ADTTE <- latest_data$adtte
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = "ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte",
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
#'   code = "ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte",
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       join_keys = teal.data::join_keys(),
                       code = "",
                       check = FALSE) {
  data_objects <- list(...)
  checkmate::assert_list(
    data_objects,
    types = c("TealDataset", "TealDatasetConnector", "TealDataConnector")
  )
  if (inherits(join_keys, "JoinKeySet")) {
    join_keys <- teal.data::join_keys(join_keys)
  }

  get_primary_keys(data_objects, join_keys)

  if (length(join_keys$get_parents()) == 0) {
    retrieve_parents <- function(x) {
      tryCatch(
        x$get_parent(),
        error = function(cond) rep(character(0), length(x$get_datanames()))
      )
    }

    new_parents <- unlist(
      lapply(data_objects, function(x) {
        if (inherits(x, "TealDataConnector")) {
          lapply(x$get_items(), function(z) z$get_parent())
        } else {
          list(retrieve_parents(x))
        }
      }),
      recursive = FALSE
    )

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
  }

  x <- TealData$new(..., check = check, join_keys = join_keys)

  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }

  x$check_reproducibility()
  x$check_metadata()
  return(x)
}
