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
#' @param datanames (`character`)\cr
#'   Names of the objects to be created from the code evaluation.
#'   If not specified (`character(0)`), all objects will be used to `teal_data` function
#'   (via `env_list` in `postprocess_fun`).
#'
#' @inheritParams teal_data
#'
#'
#' @examples
#' @export
ddl <- function(code,
                ui = submit_button_ui,
                server = submit_button_server,
                offline_args = list(),
                postprocess_fun = function(env_list, code, join_keys) {
                  do.call(teal.data::teal_data, args = c(env_list, code = code, join_keys = join_keys))
                },
                join_keys = teal.data::join_keys(),
                datanames) {
  if (missing(datanames)) {
    stop("`dataname` argument is required")
  }

  structure(
    list(
      code = code,
      ui = ui,
      server = server,
      offline_args = offline_args,
      postprocess_fun = postprocess_fun,
      datanames = datanames,
      join_keys = join_keys
    ),
    class = "ddl"
  )
}

#' Creates `tdata` object
#'
#' Resolves arguments and executes custom DDL `code`.
#' Custom `code` is substituted by `online_args` and evaluated. Then obtained code is
#' substituted again by `offline_args` and passed to the `postprocess_fun`.
#'
#' @inheritParams ddl
#' @param online_args (`list` named)\cr
#'  Arguments to be substituted in the `code` and evaluated. Result of the evaluation
#'  is based on the provided (dynamic) arguments.
#'
#' @return `tdata` containing objects created:
#' - `env` created by the `code` substitution and evaluation using
#' `online_args`, while the `code`.
#' - `code` with substituted `offline_args.
#' - `join_keys` specified in the `ddl` object.
#'
#' @export
ddl_run <- function(x, online_args = list()) {
  checkmate::assert_class(x, "ddl")
  # substitute by online args and evaluate
  env_list <- ddl_eval_substitute(code = x$code, args = online_args)
  if (is.null(env_list)) {
    warning("DDL code returned NULL. Returning empty tdata object")
  }

  # don't pass non-dataset bindings further
  # we don't want to initialize tdata with them
  env_list <- env_list[x$datanames]

  # substitute by offline args
  for (i in names(x$offline_args)) {
    online_args[[i]] <- x$offline_args[[i]]
  }
  code <- glue_code(x$code, args = online_args)
  # create tdata object
  obj <- x$postprocess_fun(
    env_list,
    code = unclass(code),
    join_keys = x$join_keys
  )
  if (!inherits(obj, "tdata")) {
    stop("postprocess_fun should return tdata object")
  }

  obj
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
submit_button_server <- function(id, x) {
  moduleServer(id, function(input, output, session) {
    tdata <- eventReactive(input$submit, {
      ddl_run(x = x, online_args = reactiveValuesToList(input))
    })

    # would need to make sure we handle reactivity correctly here as teal::init expects not reactive tdata...
    return(tdata)
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
