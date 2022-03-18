## TealDataConnection ====
#'
#' @description `r lifecycle::badge("stable")`
#' @title A `TealDataConnection` class of objects
#'
#' Objects of this class store the connection to a data source.
#' It can be a database or server connection.
#'
#' @examples
#' open_fun <- callable_function(data.frame) # define opening function
#' open_fun$set_args(list(x = 1:5)) # define fixed arguments to opening function
#'
#' close_fun <- callable_function(sum) # define closing function
#' close_fun$set_args(list(x = 1:5)) # define fixed arguments to closing function
#'
#' ping_fun <- callable_function(function() TRUE)
#'
#' x <- data_connection( # define connection
#'   ping_fun = ping_fun, # define ping function
#'   open_fun = open_fun, # define opening function
#'   close_fun = close_fun # define closing function
#' )
#'
#' x$set_open_args(args = list(y = letters[1:5])) # define additional arguments if necessary
#'
#' x$open() # call opening function
#' x$get_open_call() # check reproducible R code
#'
#' # get data from connection via TealDataConnector$get_dataset()
#' \dontrun{
#' x$open(args = list(x = 1:5, y = letters[1:5])) # able to call opening function with arguments
#' x$close() # call closing function
#' }
#'
TealDataConnection <- R6::R6Class( # nolint
  ## __Public Methods ====
  "TealDataConnection",
  public = list(
    #' @description
    #' Create a new `TealDataConnection` object
    #'
    #' @param open_fun (`CallableFunction`) function to open connection
    #' @param close_fun (`CallableFunction`) function to close connection
    #' @param ping_fun (`CallableFunction`) function to ping connection
    #' @param if_conn_obj optional, (`logical`) whether to store `conn` object returned from opening
    #'   connection
    #' @return new `TealDataConnection` object
    initialize = function(open_fun = NULL, close_fun = NULL, ping_fun = NULL, if_conn_obj = FALSE) {
      checkmate::assert_flag(if_conn_obj)
      if (!is.null(open_fun)) {
        stopifnot(inherits(open_fun, "Callable"))
        private$set_open_fun(open_fun)
      }
      if (!is.null(close_fun)) {
        stopifnot(inherits(close_fun, "Callable"))
        private$set_close_fun(close_fun)
      }
      if (!is.null(ping_fun)) {
        stopifnot(inherits(ping_fun, "Callable"))
        private$set_ping_fun(ping_fun)
      }
      private$if_conn_obj <- if_conn_obj

      private$open_ui <- function(id) {
        NULL
      }
      private$ping_ui <- function(id) {
        NULL
      }
      private$close_ui <- function(id) {
        NULL
      }

      logger::log_trace(
        sprintf(
          "TealDataConnection initialized with:%s%s%s%s.",
          if (!is.null(open_fun)) " open_fun" else "",
          if (!is.null(close_fun)) " close_fun" else "",
          if (!is.null(ping_fun)) " ping_fun" else "",
          if (if_conn_obj) " conn" else ""
        )
      )
      invisible(self)
    },
    #' @description
    #' Finalize method closing the connection.
    #'
    #' @return NULL
    finalize = function() {
      self$close(silent = TRUE, try = TRUE)
      NULL
    },
    #' @description
    #' If connection is opened
    #'
    #' If open connection has been successfully evaluated
    #'
    #' @return (`logical`) if connection is open
    is_opened = function() {
      return(private$opened)
    },
    #' @description
    #' Check if connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if connection failed, else `FALSE`
    is_failed = function() {
      self$is_open_failed() || self$is_close_failed()
    },
    #' @description
    #' Run simple application that uses its `ui` and `server` fields to open the
    #' connection.
    #'
    #' Useful for debugging
    #'
    #' @return An object that represents the app
    launch = function() {
      shinyApp(
        ui = fluidPage(
          include_js_files(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$div(
                id = "connection_inputs",
                self$get_open_ui(id = "data_connection"),
                actionButton("submit", "Submit"),
                `data-proxy-click` = "submit" # handled by jscode in custom.js - hit enter to submit
              ),
              shinyjs::hidden(
                tags$div(
                  id = "connection_set",
                  div(
                    h3("Connection successfully set."),
                    p("You can close this window and get back to R console.")
                  )
                )
              )
            )
          )
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          preopen_server <- self$get_preopen_server()
          if (!is.null(preopen_server)) {
            preopen_server(id = "data_connection", connection = self)
          }
          observeEvent(input$submit, {
            rv <- reactiveVal(NULL)
            open_server <- self$get_open_server()
            if (!is.null(open_server)) {
              rv(open_server(id = "data_connection", connection = self))
            }
            observeEvent(rv(), {
              if (self$is_opened()) {
                removeUI(sprintf("#%s", session$ns("connection_inputs")))
                shinyjs::show("connection_set")
                stopApp()
              }
            })
          })
        }
      )
    },
    # ___ open connection -----
    #' @description
    #' Open the connection.
    #'
    #' Note that if the connection is already opened then it does nothing.
    #'
    #' @param args (`NULL` or named `list`) additional arguments not set up previously
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #' @param try (`logical`) whether perform function evaluation inside `try` clause
    #'
    #' @return returns `self` if successful or if connection has been already
    #' opened. If `open_fun` fails, app returns an error in form of
    #' `shinyjs::alert` (if `try = TRUE`) or breaks the app (if `try = FALSE`)
    #'
    open = function(args = NULL, silent = FALSE, try = FALSE) {
      logger::log_trace("TealDataConnection$open opening the connection...")
      checkmate::assert_list(args, min.len = 0, names = "unique", null.ok = TRUE)
      if (isFALSE(private$check_open_fun(silent = silent))) {
        return()
      }
      if (isTRUE(private$opened) && isTRUE(private$ping())) {
        private$opened <- TRUE
        logger::log_trace("TealDataConnection$open connection already opened - skipped.")
        return(invisible(self))
      } else {
        open_res <- private$open_fun$run(args = args, try = try)
        if (!self$is_open_failed()) {
          private$opened <- TRUE
          if (private$if_conn_obj && !is.null(open_res)) {
            private$conn <- open_res

            if (!is.null(private$close_fun)) {
              private$close_fun$assign_to_env("conn", private$conn)
            }
            if (!is.null(private$ping_fun)) {
              private$ping_fun$assign_to_env("conn", private$conn)
            }
          }
          logger::log_trace("TealDataConnection$open connection opened.")
        } else {
          private$opened <- FALSE
          private$conn <- NULL
          logger::log_error("TealDataConnection$open connection failed to open.")
        }

        return(invisible(self))
      }
    },

    #' @description
    #' Get internal connection object
    #'
    #' @return `connection` object
    get_conn = function() {
      return(private$conn)
    },
    #' @description
    #' Get executed open connection call
    #'
    #' @param deparse (`logical`) whether return deparsed form of a call
    #' @param args (`NULL` or named `list`) additional arguments not set up previously
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed `call` object
    get_open_call = function(deparse = TRUE, args = NULL, silent = FALSE) {
      checkmate::assert_flag(deparse)
      checkmate::assert_list(args, min.len = 0, names = "unique", null.ok = TRUE)
      if (isFALSE(private$check_open_fun(silent = silent))) {
        return()
      }
      open_call <- private$open_fun$get_call(deparse = FALSE, args = args)

      if (private$if_conn_obj) {
        open_call <- call("<-", as.name("conn"), open_call)
      }

      if (isTRUE(deparse)) {
        deparse1(open_call, collapse = "\n")
      } else {
        open_call
      }
    },
    #' @description
    #' Get error message from last connection
    #'
    #' @return (`character`)\cr
    #'  text of the error message or `character(0)` if last
    #'  connection was successful.
    get_open_error_message = function() {
      return(private$open_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module prior opening connection.
    #'
    #' @return (`function`) shiny server prior opening connection.
    get_preopen_server = function() {
      return(private$preopen_server)
    },
    #' @description
    #' Get shiny server module to open connection.
    #'
    #' @return (`function`) shiny server to open connection.
    get_open_server = function() {
      return(private$open_server)
    },
    #' @description
    #' Get Shiny module with inputs to open connection
    #'
    #' @param id `character` shiny element id
    #'
    #' @return (`function`) shiny ui to set arguments to open connection function.
    get_open_ui = function(id) {
      return(private$open_ui(id))
    },
    #' @description
    #' Check if open connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if open connection failed, else `FALSE`
    is_open_failed = function() {
      if (!is.null(private$open_fun)) {
        private$open_fun$is_failed()
      } else {
        FALSE
      }
    },
    #' @description
    #' Set open connection function argument
    #'
    #' @param args (`NULL` or named `list`) with values where list names are argument names
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return (`self`) invisibly for chaining.
    set_open_args = function(args, silent = FALSE) {
      checkmate::assert_list(args, min.len = 0, names = "unique", null.ok = TRUE)
      if (isFALSE(private$check_open_fun(silent = silent))) {
        return()
      }
      private$open_fun$set_args(args)
      logger::log_trace("TealDataConnection$set_open_args open args set.")

      return(invisible(self))
    },
    #' @description
    #' Set pre-open connection server function
    #'
    #' This function will be called before submit button will be hit.
    #'
    #' @param preopen_module (`function`)\cr
    #'  A shiny module server function
    #'
    #' @return (`self`) invisibly for chaining.
    set_preopen_server = function(preopen_module) {
      stopifnot(inherits(preopen_module, "function"))
      module_name <- "open_conn"
      if (all(names(formals(preopen_module)) %in% c("input", "output", "session", "connection"))) {
        private$preopen_server <- function(input, output, session, connection) {
          callModule(preopen_module, id = module_name, connection = connection)
        }
      } else if (all(names(formals(preopen_module)) %in% c("id", "connection"))) {
        private$preopen_server <- function(id, connection) {
          moduleServer(
            id = id,
            module = function(input, output, session) {
              preopen_module(id = module_name, connection = connection)
            }
          )
        }
      } else {
        stop(paste(
          "set_preopen_server accepts only a valid shiny module",
          "definition with a single additional parameter 'connection'."
        ))
      }
      logger::log_trace("TealDataConnection$set_preopen_server preopen_server set.")

      invisible(self)
    },
    #' @description
    #' Set open connection server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic `ui` as `server` function is executed after hitting submit
    #' button.
    #'
    #' @param open_module (`function`)\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return (`self`) invisibly for chaining.
    set_open_server = function(open_module) {
      stopifnot(inherits(open_module, "function"))
      module_name <- "open_conn"
      if (all(names(formals(open_module)) %in% c("input", "output", "session", "connection"))) {
        private$open_server <- function(input, output, session, connection) {
          withProgress(message = "Opening connection", value = 1, {
            callModule(open_module, id = module_name, connection = connection)
          })
        }
      } else if (all(names(formals(open_module)) %in% c("id", "connection"))) {
        private$open_server <- function(id, connection) {
          moduleServer(
            id = id,
            module = function(input, output, session) {
              withProgress(message = "Opening connection", value = 1, {
                open_module(id = module_name, connection = connection)
              })
            }
          )
        }
      } else {
        stop(paste(
          "set_open_server accepts only a valid shiny module",
          "definition with a single additional parameter 'connection'."
        ))
      }
      logger::log_trace("TealDataConnection$set_open_server open_server set.")

      invisible(self)
    },
    #' @description
    #' Set open connection UI function
    #'
    #' @param open_module (`function`)\cr
    #'  shiny module as function. Inputs specified in this `ui` are passed to server module
    #'  defined by `set_open_server` method.
    #'
    #' @return (`self`) invisibly for chaining.
    set_open_ui = function(open_module) {
      stopifnot(inherits(open_module, "function"))
      stopifnot(identical(names(formals(open_module)), "id"))

      private$open_ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("open_conn"),
            open_module(id = ns("open_conn"))
          )
        )
      }
      logger::log_trace("TealDataConnection$set_open_ui open_ui set.")

      invisible(self)
    },
    # ___ close connection -------
    #' @description
    #' Close the connection.
    #'
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #' @param try (`logical`) whether perform function evaluation inside `try` clause
    #'
    #' @return returns (`self`) if successful. For unsuccessful evaluation it
    #' depends on `try` argument: if `try = TRUE` then returns
    #' `error`, for `try = FALSE` otherwise
    close = function(silent = FALSE, try = FALSE) {
      logger::log_trace("TealDataConnection$close closing the connection...")
      if (isFALSE(private$check_close_fun(silent = silent))) {
        return()
      }
      close_res <- private$close_fun$run(try = try)
      if (inherits(close_res, "error")) {
        logger::log_error("TealDataConnection$close failed to close the connection.")
        return(close_res)
      } else {
        private$opened <- FALSE
        private$conn <- NULL
        logger::log_trace("TealDataConnection$close connection closed.")
        return(invisible(NULL))
      }
    },
    #' @description
    #' Get executed close connection call
    #'
    #' @param deparse (`logical`) whether return deparsed form of a call
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return optionally deparsed `call` object
    get_close_call = function(deparse = TRUE, silent = FALSE) {
      checkmate::assert_flag(deparse)
      if (isFALSE(private$check_close_fun(silent = silent))) {
        return()
      }
      private$close_fun$get_call(deparse = deparse)
    },
    #' @description
    #' Get error message from last connection
    #'
    #' @return (`character`)\cr
    #'  text of the error message or `character(0)` if last
    #'  connection was successful.
    get_close_error_message = function() {
      return(private$close_fun$get_error_message())
    },
    #' @description
    #' Get shiny server module to close connection.
    #'
    #' @return the `server function` to close connection.
    get_close_server = function() {
      return(private$close_server)
    },
    #' @description
    #' Check if close connection has not failed.
    #'
    #' @return (`logical`) `TRUE` if close connection failed, else `FALSE`
    is_close_failed = function() {
      if (!is.null(private$close_fun)) {
        private$close_fun$is_failed()
      } else {
        FALSE
      }
    },

    #' @description
    #' Set close connection function argument
    #'
    #' @param args (named `list`) with values where list names are argument names
    #' @param silent (`logical`) whether convert all "missing function" errors to messages
    #'
    #' @return (`self`) invisibly for chaining.
    set_close_args = function(args, silent = FALSE) {
      checkmate::assert_list(args, min.len = 0, names = "unique", null.ok = TRUE)
      if (isFalse(private$check_close_fun(silent = silent))) {
        return()
      }
      private$close_fun$set_args(args)
      logger::log_trace("TealDataConnection$set_close_args close_args set")

      return(invisible(self))
    },

    #' @description
    #' Set close connection UI function
    #'
    #' @param close_module (`function`)\cr
    #'  shiny module as function. Inputs specified in this `ui` are passed to server module
    #'  defined by `set_close_server` method.
    #'
    #' @return (`self`) invisibly for chaining.
    set_close_ui = function(close_module) {
      stopifnot(inherits(close_module, "function"))
      stopifnot(identical(names(formals(close_module)), "id"))

      private$close_ui <- function(id) {
        ns <- NS(id)
        tags$div(
          tags$div(
            id = ns("close_conn"),
            close_module(id = ns("close_conn"))
          )
        )
      }
      logger::log_trace("TealDataConnection$close_ui close_ui set.")

      return(invisible(self))
    },

    #' @description
    #' Set close-connection server function
    #'
    #' This function will be called after submit button will be hit. There is no possibility to
    #' specify some dynamic `ui` as `server` function is executed after hitting submit
    #' button.
    #'
    #' @param close_module (`function`)\cr
    #'  A shiny module server function that should load data from all connectors
    #'
    #' @return (`self`) invisibly for chaining.
    set_close_server = function(close_module) {
      stopifnot(inherits(close_module, "function"))
      if (all(names(formals(close_module)) %in% c("input", "output", "session", "connection"))) {
        function(input, output, session, connection) {
          connection$close(try = TRUE)

          if (connection$is_close_failed()) {
            shinyjs::alert(
              paste(
                "Error closing connection\nError message: ",
                connection$get_close_error_message()
              )
            )
          }
          invisible(connection)
        }
      } else if (all(names(formals(close_module)) %in% c("id", "connection"))) {
        function(id, connection) {
          moduleServer(
            id,
            function(input, output, session) {
              connection$close(try = TRUE)

              if (connection$is_close_failed()) {
                shinyjs::alert(
                  paste(
                    "Error closing connection\nError message: ",
                    connection$get_close_error_message()
                  )
                )
              }
              invisible(connection)
            }
          )
        }
      } else {
        stop(paste(
          "set_close_server accepts only a valid shiny module",
          "definition with a single additional parameter 'connection'."
        ))
      }
      logger::log_trace("TealDataConnection$set_close_server close_server set.")

      invisible(self)
    }
  ),
  ## __Private Fields ====
  private = list(
    # callableFunctions
    open_fun = NULL,
    close_fun = NULL,
    ping_fun = NULL,

    # connection object
    if_conn_obj = FALSE,
    conn = NULL,

    # shiny elements
    open_ui = NULL,
    close_ui = NULL,
    ping_ui = NULL,
    preopen_server = NULL,
    open_server = NULL,
    close_server = NULL,
    ping_server = NULL,
    opened = FALSE,

    ## __Private Methods ====
    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: open_fun is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },
    check_open_fun = function(silent = FALSE) {
      checkmate::assert_flag(silent)

      if (is.null(private$open_fun)) {
        msg <- "Open connection function not set"
        if (silent) {
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    check_close_fun = function(silent = FALSE) {
      checkmate::assert_flag(silent)

      if (is.null(private$close_fun)) {
        msg <- "Close connection function not set"
        if (silent) {
          return(FALSE)
        } else {
          stop(msg)
        }
      } else {
        return(TRUE)
      }
    },
    # @description
    # Set close connection function
    #
    # @param fun (`Callable`) function to close connection
    #
    # @return (`self`) invisibly for chaining.
    set_close_fun = function(fun) {
      stopifnot(inherits(fun, "Callable"))
      private$close_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set open connection function
    #
    # @param fun (`Callable`) function to open connection
    #
    # @return (`self`) invisibly for chaining.
    set_open_fun = function(fun) {
      stopifnot(inherits(fun, "Callable"))
      private$open_fun <- fun
      return(invisible(self))
    },
    # @description
    # Set a ping function
    #
    # @param fun (`Callable`) function to ping connection
    #
    # @return (`self`) invisibly for chaining.
    set_ping_fun = function(fun) {
      stopifnot(inherits(fun, "Callable"))
      private$ping_fun <- fun
      return(invisible(self))
    },
    # @description
    # Ping the connection.
    #
    # @return (`logical`)
    ping = function() {
      logger::log_trace("TealDataConnection$ping pinging the connection...")
      if (!is.null(private$ping_fun)) {
        ping_res <- isTRUE(private$ping_fun$run())
        logger::log_trace("TealDataConnection$ping ping result: { ping_res }.")
        return(ping_res)
      } else {
        return(invisible(NULL))
      }
    }
  )
)

#' The constructor for `TealDataConnection` class.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param open_fun (`CallableFunction`) function to open connection
#' @param close_fun (`CallableFunction`) function to close connection
#' @param ping_fun (`CallableFunction`) function to ping connection
#' @param if_conn_obj optional, (`logical`) whether to store `conn` object returned from opening
#'
#' @examples
#' open_fun <- callable_function(data.frame) # define opening function
#' open_fun$set_args(list(x = 1:5)) # define fixed arguments to opening function
#'
#' close_fun <- callable_function(sum) # define closing function
#' close_fun$set_args(list(x = 1:5)) # define fixed arguments to closing function
#'
#' ping_fun <- callable_function(function() TRUE)
#'
#' x <- data_connection( # define connection
#'   ping_fun = ping_fun, # define ping function
#'   open_fun = open_fun, # define opening function
#'   close_fun = close_fun # define closing function
#' )
#'
#' x$set_open_args(args = list(y = letters[1:5])) # define additional arguments if necessary
#'
#' x$open() # call opening function
#' x$get_open_call() # check reproducible R code
#'
#' # get data from connection via TealDataConnector$get_dataset()
#' \dontrun{
#' x$open(args = list(x = 1:5, y = letters[1:5])) # able to call opening function with arguments
#' x$close() # call closing function
#' }
#'
#' @return `TealDataConnection` object
#' @export
data_connection <- function(open_fun = NULL, close_fun = NULL, ping_fun = NULL, if_conn_obj = FALSE) {
  TealDataConnection$new(
    open_fun = open_fun, close_fun = close_fun, ping_fun = ping_fun, if_conn_obj = if_conn_obj
  )
}
