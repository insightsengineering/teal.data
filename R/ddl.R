#' DDL object
#'
#' Object to execute custom DDL code in the shiny session
#'
#' @expr (`expression`)\cr
#'  Syntatically valid R code to be executed in the shiny session.
#'  shouldn't be specified when `code` is specified.
#'
#' @param code (`character`, `language`)\cr
#'  Object containing code to be evaluated to load data. Shouldn't be specified when `expr`
#'  is specified.
#'
#'
#' @param ui (`shiny.tag`)\cr
#'   `shiny` ui module containing inputs which `id` correspond to the
#'   args in the `code`.
#'
#' @param server (`function(id, mask_args, code, postprocess_fun)`)\cr
#'   `shiny` server module returning data. This server should execute
#'   `code` and return a reactive data containing necessary data. To handle
#'   evaluation and code masking process it is recommended to use `ddl_run`.
#'   Package provides universal `username_password_server` which
#'   runs [ddl_run] function, which returns `` object.
#'   Details in the the example
#'
#' @param mask_args (`list` named)\cr
#'   arguments to be substituted in the `code`. These
#'   argument are going to replace arguments set through
#'   `ui` and `server`. Example use case is when app user
#'   is asked to input a password and we'd like to skip this
#'   input in the reproducible code. Typically users password
#'   is substituted with `askpass::askpass()` call, so the
#'   returned code is still executable but secure.
#'
#' @param postprocess_fun (`function(env, code)`)\cr
#'   Function to be run after code is run. This function suppose
#'   has two arguments:
#'   - `env` (`environment`) returned as a result of the code evaluation
#'   - code (`character`) `code` provided with resolved (substituted) args.
#'
#' @param datanames (`character`)\cr
#'   Names of the objects to be created from the code evaluation.
#'   If not specified (`character(0)`), all objects will be used to `teal_data` function
#'   (via `env_list` in `postprocess_fun`).
#'
#' @inheritParams teal_data
#'
#'
#' @export
ddl <- function(expr,
                code,
                ui = submit_button_ui,
                mask_args = list(),
                server = submit_button_server,
                join_keys = teal.data::join_keys(),
                datanames) {
  if (!missing(expr) && !missing(code)) {
    stop("Only one of `expr` or `code` should be specified")
  }
  if (!missing(expr)) {
    code <- substitute(expr)
  }
  if (is.character(code)) {
    code <- parse(text = code)
  }

  if (missing(datanames)) {
    stop("`dataname` argument is required")
  }


  # function creates  object from the code, input and mask_args
  # function defined here to have access to the arguments
  ddl_run <- function(online_args = list()) {
    # substitute by online args and evaluate
    env <- list2env(list(input = online_args))
    eval(code, envir = env)

    if (identical(ls(env), character(0))) {
      warning("DDL code returned NULL. Returning empty  object")
    }

    # don't pass non-dataset bindings further
    # we don't want to initialize  with them
    env_list <- as.list(env)[datanames]

    # substitute by offline args
    for (i in names(mask_args)) {
      online_args[[i]] <- mask_args[[i]]
    }
    code <- .substitute_inputs(code, args = online_args)

    # create  object
    obj <- teal.data::new_teal_data(env = env_list, code = as.expression(code), keys = join_keys)

    if (!inherits(obj, "teal_data")) {
      stop("postprocess_fun should return `teal_data` object")
    }

    obj
  }

  # changing enclosing environment of the server to have access to ddl_fun function
  # Thanks to this ddl object contains only ui and server functions
  #  and server function can be run just by calling ddl$server("<id>")!
  environment(server) <- environment()

  structure(
    list(ui = ui, server = server),
    datanames = datanames,
    join_keys = join_keys,
    class = "ddl"
  )
}

#' @name submit_button_module
#'
#' @inheritParams ddl
#' @param id (`character`) `shiny` module id.
NULL

#' @rdname submit_button_module
#' @export
submit_button_ui <- function(id) {
  ns <- NS(id)
  actionButton(inputId = ns("submit"), label = "Submit")
}

#' @rdname submit_button_module
#' @export
submit_button_server <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(online_args = reactiveValuesToList(input))
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive teal_data...
    return(tdata)
  })
}

#' substitute inputs in the code
#'
#' Function replaces symbols in the provided code prefixed with `input$` or `input[["`
#' by values of the `args` argument.
#'
#' @param code (`language`) code to substitute
#' @param args (`list`) named list or arguments
.substitute_inputs <- function(code, args) {
  code <- if (identical(as.list(code)[[1L]], as.symbol("{"))) {
    as.list(code)[-1L]
  } else {
    code
  }

  code_strings <- vapply(code, deparse1, character(1L))
  code_strings <- gsub("(input\\$)(\\w+)", "\\.(\\2\\)", code_strings)
  code_strings <- gsub("(input\\[\\[\")(\\w+)(\"\\]\\])", "\\.(\\2\\)", code_strings)

  # Use bquote to obtain code with input values and masking values.
  lapply(code_strings, function(x) {
    bquote_call <- substitute(bquote(code), list(code = str2lang(x)))
    eval(bquote_call, envir = list2env(args))
  })
}

# todo: to remove -------------
open_conn <- function(username, password) {
  if (password != "pass") stop("Invalid credentials. 'pass' is the password") else TRUE
}
close_conn <- function(conn) {
  message("closed")
  return(NULL)
}
