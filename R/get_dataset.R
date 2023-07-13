#' Get dataset from `TealDatasetConnector`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Get dataset from `TealDatasetConnector`
#' @param x (`TealDatasetConnector` or `TealDatasetConnector` or `TealDataAbstract`)
#' @param dataname (`character`) a name of dataset to be retrieved
#' @details See `help(TealDataConnector)` and `help(TealData)` for more complex examples.
#' @return (`TealDataset`)
#' @export
get_dataset <- function(x, dataname) {
  UseMethod("get_dataset")
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealDatasetConnector --------
#' pull_fun_adae <- callable_function(
#'   function() {
#'     example_cdisc_data("ADAE")
#'   }
#' )
#'
#' ADSL <- example_cdisc_data("ADSL")
#'
#' dc <- dataset_connector(
#'   dataname = "ADAE", pull_callable = pull_fun_adae,
#'   keys = get_cdisc_keys("ADSL")
#' )
#'
#' \dontrun{
#' load_dataset(dc)
#' get_dataset(dc)
#' }
#'
get_dataset.TealDatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDatasetConnector can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealDataset --------
#' ADSL <- example_cdisc_data("ADSL")
#' x <- dataset("ADSL", ADSL)
#'
#' get_dataset(x)
get_dataset.TealDataset <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDataset can contain only one dataset.")
  }
  return(x$get_dataset())
}

#' @rdname get_dataset
#' @export
#' @examples
#'
#' # TealData  (not containing connectors) --------
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = example_cdisc_data("ADSL"),
#'   code = "library(teal.data)\nADSL <- example_cdisc_data(\"ADSL\")"
#' )
#'
#' adae <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = example_cdisc_data("ADAE"),
#'   code = "library(teal.data)\nADAE <- example_cdisc_data(\"ADAE\")"
#' )
#'
#' rd <- teal.data:::TealData$new(adsl, adae)
#' get_dataset(rd, "ADSL")
get_dataset.TealDataAbstract <- function(x, dataname = NULL) {
  if (is.null(dataname)) {
    stop(paste(
      "To get single dataset from data class one must specify the name of the dataset.",
      "To get all datasets please use get_datasets()"
    ))
  }
  return(x$get_dataset(dataname = dataname))
}
