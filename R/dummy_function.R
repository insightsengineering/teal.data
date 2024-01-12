#' Generate sample `CDISC` datasets
#'
#' Retrieves example `CDISC` datasets for use in examples and testing.
#'
#' This function returns a dummy dataset and should only be used within `teal.data`.
#' Note that the datasets are not created and maintained in `teal.data`, they are retrieved its dependencies.
#'
#' @param dataname (`character(1)`) name of a `CDISC` dataset
#'
#' @return A `CDISC` dataset as a `data.frame`.
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
