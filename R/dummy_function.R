#' Generate sample CDISC datasets
#'
#' Retrieves example CDISC datasets for use in examples and testing.
#'
#' This function returns a dummy dataset and should only be used within `teal.data`.
#' Note that the datasets are not created and maintained in `teal.data`, they are retrieved from
#' [`random.cdisc.data`][random.cdisc.data::random.cdisc.data-package] package.
#'
#' @param dataname (`character(1)`) name of a CDISC dataset.
#'
#' @return A CDISC dataset as a `data.frame`.
#'
#' @export
example_cdisc_data <- function(dataname = c("ADSL", "ADAE", "ADLB", "ADCM", "ADEX", "ADRS", "ADTR", "ADTTE", "ADVS")) {
  dataname <- sprintf("r%s", match.arg(dataname))
  dynGet(dataname, ifnotfound = stop(dataname, " not found"), inherits = TRUE)
}
