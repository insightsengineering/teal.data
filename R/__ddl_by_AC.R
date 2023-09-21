#' Create input module.
#'
#' Template for creating a simple module that will put up input widgets and produce `tdata`.
#'
#' Define any inputs necessary to connect to a remote data source and produce data,
#' as well as a function (`on_submit`) that will create the desired data sets.
#'
#' One of the inputs must be an action button (or action link) called `"submit"`.
#' When clicked, the `on_submit` function will be run.
#'
#' `on_submit` must take one argument called `inputs`,
#' which will be a list of all input elements defined in the UI function except `"submit"`.
#' The function body must contain all code necessary to obtain the desired data sets and nothing else.
#' Do not return values, just assign your data sets to appropriate variables (see examples).
#'
#' Clicking the `submit` button/link will run the function provided in `on_submit`.
#' The obtained data sets will be packed into a `tdata` object.
#' The body of `on_submit` will be recorded in the resulting `tdata`.
#'
#' The `mask` argument can be used to mask input values used as arguments in the recorded code.
#' This should be a named list with names corresponding to input elements being masked,
#' and elements containing masked values. The masked values may include quoted `call`s.
#'
#' Input elements will be put in a div of class `connector-input`.
#'
#' @param ... any number of `shiny.tag`s
#' @param on_submit function to run after clicking the `submit` button, see `Details`
#' @param mask optional list specifying how to mask the code run by `on_submit`, see `Details`
#' @return A`reactive` expression returning a `tdata` object.
#'
#' #' @examples
#' library(shiny)
#' module <- input_template(
#'   textInput("user", "username", placeholder = "who goes there?"),
#'   passwordInput("pass", "password", placeholder = "friend or foe?"),
#'   actionButton("submit", "get it"),
#'   on_submit = function(input) {
#'     example_data <- paste(input$user, input$pass, sep = " -- ")
#'   },
#'   mask = list(pass = "MASKED PASSWORD")
#' )
#' ui <- fluidPage(
#'   tagList(
#'     module$ui("id"),
#'     verbatimTextOutput("value"),
#'     verbatimTextOutput("code")
#'   )
#' )
#' server <- function(input, output, session) {
#'   tdata <- module$server("id")
#'   output[["value"]] <- renderPrint({
#'     req(tdata())
#'     teal.code::get_var(tdata(), "example_data")
#'   })
#'   output[["code"]] <- renderPrint({
#'     req(tdata())
#'     cat(teal.code::get_code(tdata()), cat(sep = "\n"))
#'   })
#' }
#' if (interactive()) shinyApp(ui, server)
#'
input_template <- function(..., on_submit, mask) {
  args <- list(...)
  checkmate::assert_list(args, types = "shiny.tag")

  args  <- as.list(substitute(list(...)))[-1L]
  inputIds <- vapply(args, function(x) match.call(eval(x[[1L]]), x)[["inputId"]], character(1L))

  checkmate::assert_true(
    is.element("submit", inputIds),
    .var.name = "A \"submit\" element is specified."
  )

  submit <- unlist(eval(args[[which(inputIds == "submit")]]))
  submit_class <- submit[grep("class$", names(submit))]
  checkmate::assert_true(
    grepl("action-button", submit_class),
    .var.name = "The \"submit\" element has class \"action-button\"."
  )

  # Wrap `inputIds` arguments in in `ns` calls.
  args <- lapply(args, function(call) {
    call <- match.call(eval(call[[1]]), call)
    call <- as.list(call)
    call[["inputId"]] <- call("ns", call[["inputId"]])
    as.call(call)
  })


  ui <- function(id) {
    ns <- NS(id)
    div(
      class = "connector-input",
      lapply(args, eval, envir = environment())
    )
  }

  if (missing(mask)) mask <- list()
  tracked_request <- with_substitution(on_submit, mask)
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      result <- eventReactive(input[["submit"]], {
        inputs <- sapply(setdiff(inputIds, "submit"), function(x) input[[x]], simplify = FALSE)
        tryCatch(
          do.call(tracked_request, list(inputs)),
          error = function(e) validate(need(FALSE, sprintf("Error: %s", e$message)))
        )
      })
      result
    })
  }

  list(
    ui = ui,
    server = server
  )
}


#' wrapper for `on_submit` functions
#'
#' Wrap a function that makes some assignments in its body to return a `tdata` object with optionally masked code.
#'
#' Code found in the body of `fun` will be run in order to obtain the desired data sets.
#' References to `input$<name>` will be substituted with input values of the accompanying `shiny` module
#' for the purposes of code execution. If `mask` is provided, those references will be substituted with mask values
#' for the purposes of storing code.
#'
#' @param fun a function that takes exactly one argument, `input`, which is a named list
#' @param mask optional named list to specify code masking; see `input_template` for details
#'
#' @return
#' A `tdata` object containing variables that were created in the body of `fun`
#' and the entirety of the body of `fun` in the `@code` slot.
#'
#' @keywords internal
with_substitution <- function(fun, mask) {
  checkmate::assert_true(
    identical(names(formals(fun)), "input"),
    .var.name = "'on_submit' function only takes 'input' argument"
  )
  checkmate::assert_list(mask, names = "unique")
  function(...) {
    # Get input values from call arguments.
    call_args <- as.list(match.call(fun))$input
    checkmate::assert_list(call_args, names = "unique", .var.name = "input")
    # Add non-masked arguments to mask.
    mask <- c(mask, call_args)
    mask <- mask[!duplicated(names(mask))]

    # Extract function body as list of calls.
    fun_body <- body(fun)
    code <-
      if (is.expression(fun_body)) {
        as.list(fun_body)
      } else if (is.call(fun_body)) {
        if (identical(as.list(fun_body)[[1L]], as.symbol("{"))) {
          as.list(fun_body)[-1L]
        } else {
          list(fun_body)
        }
      } else if (is.name(fun_body)) {
        fun_body
      } else {
        stop("with_substitution: don't know ho to handle this kind of function body")
      }

    # Convert calls to strings and substitute argument references by bquote references.
    code_strings <- vapply(code, deparse1, character(1L))
    code_strings <- gsub("(input\\$)(\\w+)", "\\.(\\2\\)", code_strings)
    code_strings <- gsub("(input\\[\\[\")(\\w+)(\"\\]\\])", "\\.(\\2\\)", code_strings)
    # Use bquote to obtain code with input values and masking values.
    code_input <- lapply(code_strings, function(x) do.call(bquote, list(str2lang(x), call_args)))
    code_masked <- lapply(code_strings, function(x) do.call(bquote, list(str2lang(x), mask)))

    # Evaluate input code in separate environment.
    env <- new.env()
    eval(as.expression(code_input), env)
    # Create `tdata` with masked code.
    new_tdata(as.list(env), as.expression(code_masked))
  }
}


library(shiny)
devtools::load_all()

# mock database connection
pullme <- function(username, password) {
  if (username == "user" && password == "pass") {
    message("connection established")
  } else {
    stop("invalid credentials")
  }
}
closeme <- function() {
  message("connection closed")
}


thefun <- function(input) {
  on.exit(try(closeme()))
  pullme(username = input$user, password = input$pass)
  adsl <- scda::synthetic_cdisc_data('latest')$adsl
  adtte <- scda::synthetic_cdisc_data('latest')$adtte
}
themask <- list(
  user = quote(askpass("who are you?")),
  pass = quote(askpass("password please"))
)

module <- input_template(
  on_submit = thefun,
  mask = themask,
  textInput("user", "username", value = "user", placeholder = "who goes there?"),
  passwordInput("pass", "password", value = "pass", placeholder = "friend or foe?"),
  actionButton("submit", "get it")
)
ui <- fluidPage(
  tagList(
    module$ui("id"),
    uiOutput("val")
  )
)
server <- function(input, output, session) {
  tdata <- module$server("id")
  output[["value"]] <- renderPrint({
    tdata()
  })
  output[["code"]] <- renderPrint({
    teal.code::get_code(tdata()) %>% cat(sep = "\n")
  })
  output[["val"]] <- renderUI({
    tagList(
      verbatimTextOutput("value"),
      verbatimTextOutput("code")
    )
  })
}
if (interactive()) shinyApp(ui, server)
