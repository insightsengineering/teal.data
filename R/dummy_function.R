#' This function returns a dummy dataset for testing examples and should only be used within `teal.data`.
#'
#' It is not meant to retrieve the `SCDA` dataset, and the dataset itself is not maintained here.
#'
#' This function creates a copy of the `SCDA` data for testing purposes.
#'
#' `CDISC` data includes `ADSL`, `ADAE`, `ADLB`, `ADCM`, `ADEX`, `ADRS`, `ADTR` and `ADTTE`.
#'
#' @param dataname name of the `CDISC` dataset
#'
#' @return `cdisc_data`
#'
#' @export
example_cdisc_data <- function(dataname) {
  # Define the available datasets
  datasets <- c("ADSL", "ADAE", "ADLB", "ADCM", "ADEX", "ADRS", "ADTR", "ADTTE", "ADVS")

  # Check if the provided dataname is valid
  if (dataname %in% datasets) {
    dataset <- get(paste0("r", dataname))
    return(dataset)
  } else {
    stop("Invalid dataname. Please provide one of the following: ", paste(datasets, collapse = ", "))
  }
}
