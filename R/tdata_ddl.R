#' @export
tdata_ddl <- function(code,
                      tdata_function,
                      offline_args = username_password_args(),
                      ui = username_password_ui,
                      server = username_password_server) {
  structure(
    list(
      code = code,
      tdata_function = tdata_function,
      offline_args = offline_args,
      ui = ui,
      server = server
    ),
    class = "tdata_ddl"
  )
}


#' @export
foo <- function(offline_args, code, tdata_function, input) {
  new_offline_args <- reactiveValuesToList(input)
  env_list <- create_processing_environment(code = code, args = new_offline_args)

  for (i in names(offline_args)) {
    new_offline_args[[i]] <- offline_args[[i]]
  }

  if (!is.null(env_list)) {
    code <- glue99(code, args = new_offline_args)
    # create tdata object
    tdata_function(
      env_list,
      # {username} is converted to askpass here
      code = unclass(code)
    ) # would need error handling here
  }
  else {
    NULL
  }
}


#' @export
create_processing_environment <- function(code, args) {
  tryCatch( # at the moment the try catch is around everything - should be around the eval only
    expr = {

      # extract arguments from the UI
      # create the call by replacing {username} with the value from the ui
      call_str <- glue99(code, args)

      #create environment to run the code in
      e <- new.env(parent = parent.env(.GlobalEnv))

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

#
glue99 <- function(code, args) {
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
