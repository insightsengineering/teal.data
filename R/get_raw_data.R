#' Retrieve raw data
#'
#' @param x (`TealDataset`, `TealDatasetConnector`, `TealDataAbstract`)\cr
#'   object
#' @param dataname (`character`)\cr
#'  Name of dataset to return raw data for.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @return `data.frame` with the raw data inserted into the R6 objects. In case of
#' `TealDataAbstract`, `list` of `data.frame` can be returned
#' if user doesn't specify `dataname` - (`get_raw_data` from all datasets).
#'
#' @export
get_raw_data <- function(x, dataname = NULL) {
  checkmate::assert_string(dataname, null.ok = TRUE)
  UseMethod("get_raw_data")
}

#' @export
#' @rdname get_raw_data
#' @examples
#'
#' # TealDataset ---------
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' x <- dataset(dataname = "ADSL", x = ADSL)
#' get_raw_data(x)
get_raw_data.TealDataset <- function(x, dataname = NULL) {
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDataset can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @export
#' @rdname get_raw_data
#' @examples
#'
#' # TealDatasetConnector ---------
#' library(scda)
#' pull_fun_adsl <- callable_function(
#'   function() {
#'     synthetic_cdisc_data("latest")$adsl
#'   }
#' )
#' dc <- dataset_connector("ADSL", pull_fun_adsl)
#' load_dataset(dc)
#' get_raw_data(dc)
get_raw_data.TealDatasetConnector <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    warning("'dataname' argument ignored - TealDatasetConnector can contain only one dataset.")
  }
  x$get_raw_data()
}

#' @rdname get_raw_data
#' @export
#' @examples
#'
#' # TealData ----------------
#' library(scda)
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = synthetic_cdisc_data("latest")$adsl,
#'   code = "library(scda)\nADSL <- synthetic_cdisc_data(\"latest\")$adsl"
#' )
#'
#' adtte <- cdisc_dataset(
#'   dataname = "ADTTE",
#'   x = synthetic_cdisc_data("latest")$adtte,
#'   code = "library(scda)\nADTTE <- synthetic_cdisc_data(\"latest\")$adtte"
#' )
#'
#' rd <- teal.data:::TealData$new(adsl, adtte)
#' get_raw_data(rd)
#'
#' # TealDataConnector --------
#' library(scda)
#' adsl <- scda_cdisc_dataset_connector(dataname = "ADSL", "adsl")
#' adlb <- scda_cdisc_dataset_connector(dataname = "ADLB", "adlb")
#'
#' open_fun <- callable_function(library)
#' open_fun$set_args(list(package = "scda"))
#'
#' con <- data_connection(open_fun = open_fun)
#' con$set_open_server(
#'   function(id, connection) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         connection$open(try = TRUE)
#'         return(invisible(connection))
#'       }
#'     )
#'   }
#' )
#'
#' rdc <- relational_data_connector(connection = con, connectors = list(adsl, adlb))
#'
#' rdc$set_ui(
#'   function(id, connection, connectors) {
#'     ns <- NS(id)
#'     tagList(
#'       connection$get_open_ui(ns("open_connection")),
#'       textInput(ns("name"), p("Choose", code("scda data version")), value = "latest"),
#'       do.call(
#'         what = "tagList",
#'         args = lapply(
#'           connectors,
#'           function(connector) {
#'             div(
#'               connector$get_ui(
#'                 id = ns(connector$get_dataname())
#'               ),
#'               br()
#'             )
#'           }
#'         )
#'       )
#'     )
#'   }
#' )
#'
#' rdc$set_server(
#'   function(id, connection, connectors) {
#'     moduleServer(
#'       id = id,
#'       module = function(input, output, session) {
#'         # opens connection
#'         connection$get_open_server()(id = "open_connection", connection = connection)
#'         if (connection$is_opened()) {
#'           for (connector in connectors) {
#'             set_args(connector, args = list(name = input$name))
#'             # pull each dataset
#'             connector$get_server()(id = connector$get_dataname())
#'             if (connector$is_failed()) {
#'               break
#'             }
#'           }
#'         }
#'       }
#'     )
#'   }
#' )
#'
#' \dontrun{
#' load_datasets(rdc)
#' get_raw_data(rdc)
#' }
#'
#' # TealData (with connectors) --------
#' drc <- cdisc_data(rdc)
#' \dontrun{
#' get_raw_data(drc)
#' }
get_raw_data.TealDataAbstract <- function(x, dataname = NULL) { # nolint
  if (!is.null(dataname)) {
    datasets_names <- x$get_datanames()
    if (dataname %in% datasets_names) {
      if (is_pulled(x$get_items(dataname))) {
        get_raw_data(
          get_dataset(x, dataname = dataname)
        )
      } else {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.", dataname),
          call. = FALSE
        )
      }
    } else {
      stop("The dataname supplied does not exist.")
    }
  } else {
    lapply(
      get_datasets(x),
      get_raw_data
    )
  }
}
