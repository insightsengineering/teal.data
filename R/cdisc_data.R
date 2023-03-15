#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function takes datasets and creates `TealData` object which can be used in `teal` applications.
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
#' @param data (`list` named)
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
#'   code = "ADSL <- synthetic_cdisc_data('latest')$adsl
#'           ADTTE <- synthetic_cdisc_data('latest')$adtte",
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
#'   code = "ADSL <- synthetic_cdisc_data('latest')$adsl
#'           ADTTE <- synthetic_cdisc_data('latest')$adtte",
#'   check = TRUE
#' )
cdisc_data <- function(data,
                       join_keys = default_cdisc_join_keys(names(data)),
                       code = "",
                       check = FALSE) {
  join_keys$update_keys_given_parents()
  # check_reproducibility()
  # check_metadata()
  new_tdata(
    env = data,
    code = code,
    join_keys = join_keys
  )
}