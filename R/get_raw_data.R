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
#' ADSL <- teal.data::example_cdisc_data("ADSL")
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
#' library(magrittr)
#' pull_fun_adsl <- callable_function(teal.data::example_cdisc_data) %>%
#'   set_args(list(dataname = "ADSL"))
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
#' adsl <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = teal.data::example_cdisc_data("ADSL"),
#'   code = "library(teal.data)\nADSL <- teal.data::example_cdisc_data(\"ADSL\")"
#' )
#'
#' adtte <- cdisc_dataset(
#'   dataname = "ADTTE",
#'   x = teal.data::example_cdisc_data("ADTTE"),
#'   code = "library(teal.data)\nADTTE <- teal.data::example_cdisc_data(\"ADTTE\")"
#' )
#'
#' rd <- teal.data:::TealData$new(adsl, adtte)
#' get_raw_data(rd)
#'
#' # TealDataConnector --------
#' library(magrittr)
#'
#' slice_cdisc_data <- function(dataname, n) {
#'   head(example_cdisc_data(dataname), n)
#' }
#'
#' random_data_connector <- function(dataname) {
#'   fun_dataset_connector(
#'     dataname = dataname,
#'     fun = slice_cdisc_data,
#'     fun_args = list(dataname = dataname),
#'   )
#' }
#'
#' open_fun <- callable_function(library)
#' open_fun$set_args(list(package = "teal.data"))
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
#' rdc <- relational_data_connector(
#'   connection = con,
#'   connectors = list(random_data_connector("ADSL"), random_data_connector("ADLB"))
#' )
#'
#' rdc$set_ui(
#'   function(id, connection, connectors) {
#'     ns <- NS(id)
#'     tagList(
#'       connection$get_open_ui(ns("open_connection")),
#'       numericInput(inputId = ns("n"), label = "Choose number of records", min = 0, value = 1),
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
#'             set_args(connector, args = list(n = input$n))
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
