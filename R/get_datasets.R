#' Get a [`TealDataset`] objects.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x ([`TealData`])\cr
#'  object containing datasets.
#' @export
#' @return `list` or `TealDataset` objects
get_datasets <- function(x) {
  UseMethod("get_datasets")
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # TealData --------
#' library(scda)
#' latest_data <- synthetic_cdisc_data("latest")
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = latest_data$adsl,
#'   code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' adae <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = latest_data$adae,
#'   code = "library(scda)\nADTTE <- synthetic_cdisc_data(\"latest\")$adae"
#' )
#'
#' rd <- cdisc_data(adsl, adae)
#' get_datasets(rd)
#'
#' # TealDataConnector --------
#' adsl_cf <- callable_function(function() synthetic_cdisc_data("latest")$adsl)
#' adsl <- cdisc_dataset_connector(
#'   dataname = "ADSL",
#'   pull_callable = adsl_cf,
#'   keys = get_cdisc_keys("ADSL")
#' )
#' adlb_cf <- callable_function(function() synthetic_cdisc_data("latest")$adlb)
#' adlb <- cdisc_dataset_connector(
#'   dataname = "ADLB",
#'   pull_callable = adlb_cf,
#'   keys = get_cdisc_keys("ADLB")
#' )
#'
#' rdc <- relational_data_connector(
#'   connection = data_connection(),
#'   connectors = list(adsl, adlb)
#' )
#'
#' rdc$set_ui(function(id, connection, connectors) p("Example UI"))
#' rdc$set_server(
#'   function(id, connection, connectors) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         # Note this is simplified as we are not opening a real connection here
#'         for (connector in connectors) {
#'           set_args(connector, args = list(name = input$name))
#'           # pull each dataset
#'           connector$get_server()(id = connector$get_dataname())
#'           if (connector$is_failed()) {
#'             break
#'           }
#'         }
#'       }
#'     )
#'   }
#' )
#' \dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#' }
#'
#' # TealData --------
#' \dontrun{
#' drc <- cdisc_data(rdc, adae)
#' get_datasets(drc)
#' }
get_datasets.TealDataAbstract <- function(x) { # nolint
  res <- x$get_datasets()
  if (length(res) == 0) {
    return(invisible(NULL))
  }
  res
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # TealDatasetConnector --------
#' library(scda)
#' adsl_cf <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' rdc <- cdisc_dataset_connector("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
#' \dontrun{
#' load_datasets(rdc)
#' get_datasets(rdc)
#' }
get_datasets.TealDatasetConnector <- function(x) { # nolint
  res <- x$get_dataset()
  if (length(res) == 0) {
    return(invisible(NULL))
  }
  res
}

#' @rdname get_datasets
#' @export
#' @examples
#'
#' # TealDataset --------
#' library(scda)
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' get_datasets(adsl)
get_datasets.TealDataset <- function(x) {
  x
}
