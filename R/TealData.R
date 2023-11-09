## TealData ====
#' @title Manage multiple `TealDataConnector`, `TealDatasetConnector` and `TealDataset` objects.
#'
#' @description `r lifecycle::badge("experimental")`
#' Class manages `TealDataConnector`, `TealDatasetConnector` and
#' `TealDataset` objects and aggregate them in one collection.
#' Class also decides whether to launch app before initialize teal application.
#'
#' @param ... (`TealDataConnector`, `TealDataset`, `TealDatasetConnector`)\cr
#'   objects
#' @param join_keys (`join_keys`) or a single (`join_key_set`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then an empty `join_keys` object is passed by default.
#' @param check (`logical`) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @examples
#' adsl_cf <- callable_function(teal.data::example_cdisc_data)$set_args(list(dataname = "ADSL"))
#' adlb_cf <- callable_function(teal.data::example_cdisc_data)$set_args(list(dataname = "ADLB"))
#' adrs_cf <- callable_function(teal.data::example_cdisc_data)$set_args(list(dataname = "ADRS"))
#' adtte_cf <- callable_function(teal.data::example_cdisc_data)$set_args(list(dataname = "ADTTE"))
#' x1 <- cdisc_dataset_connector("ADSL", adsl_cf, keys = get_cdisc_keys("ADSL"))
#' x2 <- cdisc_dataset_connector("ADRS", adrs_cf, keys = get_cdisc_keys("ADRS"))
#' x3 <- cdisc_dataset(
#'   dataname = "ADAE",
#'   x = teal.data::example_cdisc_data("ADAE"),
#'   code = "library(teal.data)\nADAE <- teal.data::example_cdisc_data(\"ADAE\")"
#' )
#' x4 <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"))
#' tc <- teal.data:::TealData$new(x1, x2, x3, x4)
#' tc$get_datanames()
#' \dontrun{
#' tc$launch()
#' get_datasets(tc) # equivalent to tc$get_datasets()
#' tc$get_dataset("ADAE")
#' tc$check()
#' }
#'
#' x <- cdisc_dataset(
#'   dataname = "ADSL",
#'   x = teal.data::example_cdisc_data("ADSL"),
#'   code = "library(teal.data)\nADSL <- teal.data::example_cdisc_data(\"ADSL\")"
#' )
#'
#' x2 <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"))
#' tc <- teal.data:::TealData$new(x, x2)
#' \dontrun{
#' # This errors as we have not pulled the data
#' # tc$get_datasets()
#' # pull the data and then we can get the datasets
#' tc$launch()
#' tc$get_datasets()
#' get_raw_data(tc)
#' }
#'
TealData <- R6::R6Class( # nolint
  classname = "TealData",
  inherit = TealDataAbstract,
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `TealData` class
    initialize = function(..., check = FALSE, join_keys = teal.data::join_keys()) {
      checkmate::assert_class(join_keys, "join_keys")

      dot_args <- list(...)
      is_teal_data <- checkmate::test_list(
        dot_args,
        types = c("TealDataConnector", "TealDataset", "TealDatasetConnector")
      )
      if (!all(is_teal_data)) {
        stop("All elements should be of TealDataset(Connector) or TealDataConnector class")
      }

      datanames <- unlist(lapply(dot_args, get_dataname))
      private$check_names(datanames)

      private$datasets <- dot_args

      self$set_check(check)

      private$pull_code <- CodeClass$new()
      private$mutate_code <- CodeClass$new()

      private$join_keys <- join_keys

      self$id <- sample.int(1e11, 1, useHash = TRUE)

      logger::log_trace(
        "TealData initialized with data: { paste(self$get_datanames(), collapse = ' ') }."
      )
      return(invisible(self))
    },
    #' @description
    #' Creates a copy of the object with keeping valid references
    #' between `TealDataset` and `TealDatasetConnector` objects
    #' @param deep (`logical(1)`)\cr
    #'  argument passed to `clone` method. If `TRUE` deep copy is made
    #' @return self invisible
    copy = function(deep = FALSE) {
      new_self <- self$clone(deep = deep)
      new_self$reassign_datasets_vars()
      logger::log_trace("TealData$copy{if (deep) ' deep-' else ' '}copied self.")
      invisible(new_self)
    },
    #' @description
    #' Prints this `TealData`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)

      cat(sprintf(
        "A %s object containing %d TealDataset/TealDatasetConnector object(s) as element(s):\n",
        class(self)[1],
        length(private$datasets)
      ))

      for (i in seq_along(private$datasets)) {
        cat(sprintf("--> Element %d:\n", i))
        print(private$datasets[[i]])
      }

      invisible(self)
    },
    # ___ getters ====
    #' @description
    #' Get data connectors.
    #'
    #' @return (`list`) with all `TealDatasetConnector` or `TealDataConnector` objects.
    get_connectors = function() {
      return(Filter(
        function(x) {
          inherits(x, "TealDatasetConnector") || inherits(x, "TealDataConnector")
        },
        private$datasets
      ))
    },
    #' @description
    #' Get all datasets and all dataset connectors
    #'
    #' @param dataname (`character` value)\cr
    #'   name of dataset connector to be returned. If `NULL`, all connectors are returned.
    #'
    #' @return `list` with all datasets and all connectors
    get_items = function(dataname = NULL) {
      checkmate::assert_string(dataname, null.ok = TRUE)

      get_sets <- function(x) {
        if (inherits(x, "TealDataConnector")) {
          x$get_items()
        } else {
          x
        }
      }

      sets <- unlist(lapply(private$datasets, get_sets))
      names(sets) <- vapply(sets, get_dataname, character(1))

      if (checkmate::test_string(dataname)) {
        if (!(dataname %in% self$get_datanames())) {
          stop(paste("dataset", dataname, "not found"))
        }
        return(sets[[dataname]])
      } else {
        return(sets)
      }
    },

    #' @description
    #' Get join keys between two datasets.
    #'
    #' @param dataset_1 (`character`) name of first dataset.
    #' @param dataset_2 (`character`) name of second dataset.
    #' @return (`character`) named character vector x with names(x) the
    #' columns of `dataset_1` and the values of `(x)` the corresponding join
    #' keys in `dataset_2` or `character(0)` if no relationship
    get_join_keys = function(dataset_1 = NULL, dataset_2 = NULL) {
      if (is.null(dataset_1) && is.null(dataset_2)) {
        private$join_keys
      } else if (is.null(dataset_1)) {
        private$join_keys[[dataset_2]]
      } else if (is.null(dataset_2)) {
        private$join_keys[[dataset_1]]
      } else {
        private$join_keys[[dataset_1, dataset_2]]
      }
    },

    #' @description
    #' returns the parents list of the datasets.
    #'
    #' @return named (`list`) of the parents of all datasets.
    get_parents = function() {
      parents(private$join_keys)
    },

    # ___ shiny ====

    #' @description
    #'
    #' Get a shiny-module UI to render the necessary app to
    #' derive `TealDataConnector` object's data
    #'
    #' @param id (`character`) item ID for the shiny module
    #' @return the `shiny` `ui` function
    get_ui = function(id) {
      if (is.null(private$ui)) {
        div(id = id, "Data Loaded")
      } else {
        private$ui(id)
      }
    },
    #' @description
    #'
    #' Get a shiny-module server to render the necessary app to
    #' derive `TealDataConnector` object's data
    #'
    #' @return `shiny` server module.
    get_server = function() {
      if (is.null(private$server)) {
        return(
          function(id) {
            moduleServer(
              id = id,
              module = function(input, output, session) {
                reactive(self)
              }
            )
          }
        )
      } else {
        function(id) {
          moduleServer(
            id = id,
            module = private$server
          )
        }
      }
    },
    #' @description
    #'
    #' Launch an app that allows to run the user-interfaces of all
    #' `TealDataConnector` and `TealDatasetConnector` modules
    #'
    #' This piece is mainly used for debugging.
    launch = function() {
      # if no data connectors can append any dataset connectors
      # and not load an app
      if (self$is_pulled()) {
        stop("All the datasets have already been pulled.")
      }

      # otherwise load TealDataConnector and
      # TealDatasetConnector with shiny app
      shinyApp(
        ui = fluidPage(
          theme = get_teal_bs_theme(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              self$get_ui(id = "main_app"),
              shinyjs::hidden(
                tags$div(
                  id = "data_loaded",
                  div(
                    h3("Data successfully loaded."),
                    p("You can close this window and get back to R console.")
                  )
                )
              ),
              include_js_files(),
              br()
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          dat <- self$get_server()(id = "main_app")

          observeEvent(dat(), {
            if (self$is_pulled()) {
              shinyjs::show("data_loaded")
              stopApp()
            }
          })
          NULL
        }
      )
    },

    # ___ mutate ====
    #' @description
    #' Change join_keys for a given pair of dataset names
    #' @param dataset_1,dataset_2 (`character`) datasets for which join_keys are to be returned
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate_join_keys = function(dataset_1, dataset_2, val) {
      private$join_keys[[dataset_1]][[dataset_2]] <- val
      private$join_keys
    },

    # ___ check ====
    #' @description
    #' Check there is consistency between the datasets and join_keys
    #' @return raise and error or invisible `TRUE`
    check_metadata = function() {
      if (isFALSE(self$is_pulled())) {
        # all the checks below required data to be already pulled
        return(invisible(TRUE))
      }

      for (dataset in self$get_datasets()) {
        dataname <- get_dataname(dataset)
        dataset_colnames <- dataset$get_colnames()

        # expected columns in this dataset from join_keys specification
        join_key_cols <- unique(unlist(lapply(self$get_join_keys(dataname), names)))
        if (!is.null(join_key_cols) && !all(join_key_cols %in% dataset_colnames)) {
          stop(
            paste(
              "The join key specification requires dataset",
              dataname,
              "to contain the following columns:",
              paste(join_key_cols, collapse = ", ")
            )
          )
        }

        # check if primary keys in dataset
        primary_key_cols <- self$get_join_keys(dataname, dataname)
        if (!is.null(primary_key_cols) && !all(primary_key_cols %in% dataset_colnames)) {
          stop(
            paste(
              "The primary keys specification requires dataset",
              dataname,
              "to contain the following columns:",
              paste(primary_key_cols, collapse = ", ")
            )
          )
        }
        dataset$check_keys()
      }

      logger::log_trace("TealData$check_metadata metadata check passed.")

      return(invisible(TRUE))
    }
  ),

  ## __Private Fields ====
  private = list(
    join_keys = NULL,
    ui = function(id) {
      ns <- NS(id)

      # connectors ui(s) + submit button
      fluidPage(
        include_js_files(),
        theme = get_teal_bs_theme(),
        shinyjs::hidden(
          column(
            id = ns("delayed_data"),
            width = 8,
            offset = 2,
            div(
              tagList(
                lapply(
                  private$datasets,
                  function(x) {
                    div(
                      if (inherits(x, "TealDataConnector")) {
                        ui <- x$get_ui(id = ns(x$id))
                        if (is.null(ui)) {
                          ui <- div(
                            h4("TealDataset Connector for: ", lapply(x$get_datanames(), code)),
                            p(icon("check"), "Ready to Load")
                          )
                        }
                        ui
                      } else if (inherits(x, "TealDatasetConnector")) {
                        ui <- x$get_ui(id = ns(paste0(x$get_datanames(), collapse = "_")))
                        if (is.null(ui)) {
                          ui <- div(
                            h4("TealDataset Connector for: ", code(x$get_dataname())),
                            p(icon("check"), "Ready to Load")
                          )
                        }
                        ui
                      } else {
                        div(h4("Data(set) for: ", lapply(x$get_datanames(), code)), p(icon("check"), "Loaded"))
                      },
                      br()
                    )
                  }
                ),
                actionButton(inputId = ns("submit"), label = "Submit all")
              ),
              `data-proxy-click` = ns("submit") # handled by jscode in custom.js - hit enter to submit
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      logger::log_trace("TealData$server initializing...")

      shinyjs::show("delayed_data")
      for (dc in self$get_connectors()) {
        if (inherits(dc, "TealDataConnector")) {
          dc$get_preopen_server()(id = dc$id)
        }
      }
      rv <- reactiveVal(NULL)
      observeEvent(input$submit, {
        logger::log_trace("TealData$server@1 submit button clicked.")
        # load data from all connectors
        for (dc in self$get_connectors()) {
          if (inherits(dc, "TealDataConnector")) {
            dc$get_server()(
              id = dc$id,
              connection = dc$get_connection(),
              connectors = dc$get_items()
            )
          } else if (inherits(dc, "TealDatasetConnector")) {
            dc$get_server()(id = dc$get_dataname())
          }
          if (dc$is_failed()) {
            break
          }
        }

        if (self$is_pulled()) {
          logger::log_trace("TealData$server@1 data is pulled.")
          withProgress(value = 1, message = "Checking data reproducibility", {
            # We check first and then mutate.
            #  mutate_code is reproducible by default we assume that we don't
            #  have to check the result of the re-evaluation of the code
            self$check_reproducibility()
          })

          withProgress(value = 1, message = "Executing processing code", {
            self$execute_mutate()
            self$check_metadata()
          })
          logger::log_info("Data ready to pass to the application.")
          shinyjs::hide("delayed_data")
          rv(self)
        }
      })
      return(rv)
    }
  )
)
