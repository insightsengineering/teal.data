#' Load data from connection
#'
#' @description `r lifecycle::badge("stable")`
#' Load data from connection. Function used on [`TealDatasetConnector`] and
#' [`TealDataset`] to obtain data from connection.
#'
#' @param x (`TealDatasetConnector` or `TealDataset`)
#' @param args (`NULL` or named `list`)\cr
#'   additional dynamic arguments passed to function which loads the data.
#' @param try (`logical`) whether perform function evaluation inside `try` clause
#' @param conn Optional (`TealDataConnection`) object required to pull the data.
#' @param ... not used, only for support of S3
#'
#' @return `x` with loaded `dataset` object
#' @export
load_dataset <- function(x, ...) {
  UseMethod("load_dataset")
}

#' @rdname load_dataset
#' @examples
#'
#' # TealDataset --------
#' ADSL <- example_cdisc_data("ADSL")
#' ADSL_dataset <- dataset("ADSL", x = ADSL)
#'
#' load_dataset(ADSL_dataset)
#' @export
load_dataset.TealDataset <- function(x, ...) { # nolint
  check_ellipsis(...)
  return(invisible(x$get_dataset()))
}

#' @rdname load_dataset
#' @examples
#'
#' # TealDatasetConnector --------
#'
#' random_data_connector <- function(dataname) {
#'   fun_dataset_connector(
#'     dataname = dataname,
#'     fun = example_cdisc_data,
#'     fun_args = list(dataname = dataname),
#'   )
#' }
#'
#' adsl <- random_data_connector(dataname = "ADSL")
#' load_dataset(adsl)
#' get_dataset(adsl)
#'
#' adae <- random_data_connector(dataname = "ADAE")
#' load_dataset(adae)
#' get_dataset(adae)
#' @export
load_dataset.TealDatasetConnector <- function(x, args = NULL, try = FALSE, conn = NULL, ...) { # nolint
  check_ellipsis(...)
  if (!is.null(conn)) {
    stopifnot(inherits(conn, "TealDataConnection"))

    conn$open()
    conn_obj <- conn$get_conn()

    x$get_pull_callable()$assign_to_env("conn", conn_obj)
  }

  x$pull(args = args, try = try)

  return(invisible(x))
}

#' Load datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x ([`TealData`], [`TealDataset`] or [`TealDatasetConnector`])
#' @param args (`NULL` or named `list`)\cr
#'   additional dynamic arguments passed to function which loads the data. Applicable only on [`TealDatasetConnector`])
#' @param try (`logical`)\cr
#'   whether perform function evaluation inside `try` clause. Applicable only on [`TealDatasetConnector`])
#' @param ... (not used)\cr
#'  only for support of S3
#'
#' @export
#' @return If executed in the interactive session shiny app is opened to load the data. If executed in
#'  shiny application - it returns shiny server module.
load_datasets <- function(x, ...) {
  UseMethod("load_datasets")
}

#' @rdname load_datasets
#' @examples
#'
#' # TealDataset ------
#' ADSL <- example_cdisc_data("ADSL")
#' x <- dataset("ADSL", x = ADSL)
#'
#' load_datasets(x)
#' @export
load_datasets.TealDataset <- function(x, ...) { # nolint
  check_ellipsis(...)
  return(invisible(x$get_dataset()))
}

#' @rdname load_datasets
#' @examples
#'
#' # TealDatasetConnector ------
#' random_data_connector <- function(dataname) {
#'   fun_dataset_connector(
#'     dataname = dataname,
#'     fun = example_cdisc_data,
#'     fun_args = list(dataname = dataname),
#'   )
#' }
#'
#' adsl <- random_data_connector(dataname = "ADSL")
#' load_datasets(adsl)
#' get_dataset(adsl)
#'
#' adae <- random_data_connector(dataname = "ADAE")
#' load_datasets(adae)
#' get_dataset(adae)
#' @export
load_datasets.TealDatasetConnector <- function(x, args = NULL, try = FALSE, ...) { # nolint
  check_ellipsis(...)
  x$pull(args = args, try = try)
  return(invisible(x))
}


#' @rdname load_datasets
#' @export
#' @examples
#'
#' # TealDataConnector --------
#' random_data_connector <- function(dataname) {
#'   fun_dataset_connector(
#'     dataname = dataname,
#'     fun = example_cdisc_data,
#'     fun_args = list(dataname = dataname),
#'   )
#' }
#'
#' adsl <- random_data_connector(dataname = "ADSL")
#' adrs <- random_data_connector(dataname = "ADRS")
#'
#' rdc <- cdisc_data(adsl, adrs)
#' \dontrun{
#' load_datasets(rdc)
#' }
load_datasets.TealDataConnector <- function(x, ...) { # nolint
  check_ellipsis(...)
  if (interactive()) {
    x$launch()
  } else {
    return(invisible(x))
  }
}

#' @rdname load_datasets
#' @export
#' @examples
#'
#' # TealData --------
#' random_data_connector <- function(dataname) {
#'   fun_dataset_connector(
#'     dataname = dataname,
#'     fun = example_cdisc_data,
#'     fun_args = list(dataname = dataname),
#'   )
#' }
#'
#' adsl <- random_data_connector(dataname = "ADSL")
#' adlb <- random_data_connector(dataname = "ADLB")
#' adrs <- random_data_connector(dataname = "ADRS")
#'
#' tc <- cdisc_data(adsl, adlb, adrs)
#' \dontrun{
#' load_datasets(tc)
#' }
load_datasets.TealData <- function(x, ...) { # nolint
  check_ellipsis(...)
  if (interactive()) {
    x$launch()
  } else {
    return(invisible(x))
  }
}
