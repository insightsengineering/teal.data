#' DDL object
#'
#' Object to execute custom DDL code in the shiny session
#'
#' @param code (`character`)\cr
#'   Code to be evaluated and returned to the `postprocess_fun`
#'
#' @param ui (`shiny.tag`)\cr
#'   `shiny` ui module containing inputs which `id` correspond to the
#'   args in the `code`.
#'
#' @param server (`function(id, offline_args, code, postprocess_fun)`)\cr
#'   `shiny` server module returning data. This server suppose to execute
#'   DDL code and return a reactive data containing necessary data.
#'   Package provides universal `username_password_server` which
#'   runs [ddl_run] function, which returns `tdata` object.
#'   Details in the the example
#'
#' @param offline_args (`list` named)\cr
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
#' @examples
#'
#' # simple example
#'
#' x <- ddl(
#'   # code to be run when app user presses submit
#'   code = "
#'   ADSL <- scda::synthetic_cdisc_data({ version })$adsl
#'   ADTTE <- scda::synthetic_cdisc_data({ version })$adtte
#'   ADRS <- scda::synthetic_cdisc_data({ version })$adrs
#'   ",
#'
#'   # ui they wish to use for the loading data
#'   ui = function(id) {
#'     ns <- NS(id)
#'     tagList(
#'       textInput(ns("version"), label = "SCDA version", value = "latest"),
#'       actionButton(ns("submit"), label = "Submit")
#'     )
#'   },
#'
#'   server = function(id, offline_args, code, postprocess_fun) {
#'     moduleServer(id, function(input, output, session) {
#'       # run code
#'       tdata <- eventReactive(input$submit, {
#'       ddl_run(
#'       offline_args = offline_args,
#'       code = code,
#'       postprocess_fun = postprocess_fun,
#'       online_args = reactiveValuesToList(input)
#'     )
#'    })
#'
#'   # function returning data objects
#'   postprocess_fun = function(env_list, code) {
#'     do.call(teal.data::cdisc_data, args = c(env_list, code = code))
#'   }
#' )
#'
#' app <- shinyApp(
#'   ui = fluidPage(
#'     fluidRow(
#'       column(3, h1("User Inputs"), x$ui(id = "custom_ui")),
#'       column(9, h1("R code"), verbatimTextOutput("output"))
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     loaded_data <- x$server(id = "custom_ui", x$offline_args, x$code, x$postprocess_fun)
#'     output$output <- renderText({
#'       req(loaded_data())
#'       teal.code::get_code(loaded_data()) |> paste(collapse = "\n")
#'     })
#'   }
#' )
#'
#' \dontrun{
#' shiny::runApp(app)
#' }
#'
#' # example with username and password
#'
#' @export
ddl <- function(code,
                postprocess_fun = function(env_list, code) {
                  do.call(teal.data::teal_data, args = c(env_list, code = code))
                },
                offline_args = list(),
                ui = submit_button_ui,
                server = submit_button_server) {
  structure(
    list(
      code = code,
      ui = ui,
      server = server,
      offline_args = offline_args,
      postprocess_fun = postprocess_fun
    ),
    class = "ddl"
  )
}

#' Creates `tdata` object
#'
#' Resolves arguments and executes custom DDL `code`.
#' Custom `code` and `data` created from code evaluation
#' are passed to the `postprocess_fun`
#'
#' @inheritParams ddl
#'
#' @export
ddl_run <- function(code, offline_args, online_args, postprocess_fun) {
  env_list <- ddl_eval_substitute(code = code, args = online_args)

  for (i in names(offline_args)) {
    online_args[[i]] <- offline_args[[i]]
  }

  if (!is.null(env_list)) {
    code <- glue_code(code, args = online_args)
    # create tdata object
    postprocess_fun(
      env_list,
      # {username} is converted to askpass here
      code = unclass(code)
    ) # would need error handling here
  } else {
    NULL
  }
}

#' Substitute and evaluate ddl code
#'
#' @inheritParams ddl
#' @param args (`list` named)\cr
#'   Containing elements named after arguments in the code
#'   enclosed in currly brackets ex. `{ arg_name }`
#' @return `list` of objects being a result of the code evaluation
#' @examples
#' ddl_eval_substitute("x <- { arg }", list(arg = 1))
#' ddl_eval_substitute("x <- { arg }", list(arg = "a"))
#' ddl_eval_substitute("a <- 1; x <- { arg } + 1", list(arg = quote(a)))
#' ddl_eval_substitute("a <- b", list(b = 1))
ddl_eval_substitute <- function(code, args) {
  tryCatch( # at the moment the try catch is around everything - should be around the eval only
    expr = {
      # extract arguments from the UI
      # create the call by replacing { xyz } with the value from the args$xyz
      call_str <- glue_code(code, args)

      # create environment to run the code
      e <- list2env(args, parent = parent.env(.GlobalEnv))

      # evaluate the code
      eval(parse(text = call_str), envir = e)

      # return a list
      as.list(e)
    },
    error = function(cond) {
      showNotification(cond$message, type = "error")
      NULL
    }
  )
}

#' Substitute ddl code args
#'
#' Substitutes code arguments with `args`. Parts of the code
#' wrapped in curly brackets ex. `{ arg_name }` are replaced
#' with corresponding list elements
#' @inheritsParams ddl_eval_substitute
#' @return `character`
#' @examples
#' glue_code("x <- { arg }", list(arg = 1))
#' glue_code("x <- { arg }", list(arg = "a"))
#' glue_code("a <- 1; x <- { arg } + 1", list(arg = quote(a)))
#' glue_code(
#'   "a <- connect(login = { login }, password = { pass})",
#'   list(
#'     login = quote(askpass::askpass()),
#'     password = quote(askpass::askpass())
#'   )
#' )
glue_code <- function(code, args) {
  args <- lapply(args, function(x) {
    if (is.character(x)) {
      dQuote(x, q = FALSE)
    } else if (is.language(x)) {
      deparse1(x)
    } else {
      x
    }
  })
  glue::glue(code, .envir = args)
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
submit_button_server <- function(id, offline_args, code, postprocess_fun) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(
        code = code,
        offline_args = offline_args,
        online_args = reactiveValuesToList(input),
        postprocess_fun = postprocess_fun
      )
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive tdata...
    return(tdata)
  })
}
