## CallablePythonCode ====
#'
#' @title A `CallablePythonCode` class of objects
#' @keywords internal
#'
CallablePythonCode <- R6::R6Class( # nolint

  ## __Public Methods ====
  classname = "CallablePythonCode",
  inherit = CallableFunction,
  public = list(
    #' @description
    #' Create a new `CallablePythonCode` object
    #'
    #' @param fun (`function`)\cr
    #'  function to be evaluated in class. Function should be named
    #' @param env (\code{environment})\cr
    #'  environment where function will be evaluated
    #' @return new `CallablePythonCode` object
    initialize = function(fun, env = new.env(parent = parent.env(globalenv()))) {
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("Cannot load package 'reticulate' - please install the package.", call. = FALSE)
      }

      super$initialize(fun = fun, env = env)
      logger::log_trace("CallablePythonCode initialized.")
      return(invisible(self))
    },
    #' @description
    #'   For scripts and code that contain multiple objects, save the name
    #'   of the object that corresponds to the final dataset of interest.
    #'   This is required for running python scripts with `reticulate`.
    #'
    #' @param x (`character`) the name of the object produced by the code
    #'   or script.
    #'
    #' @return (`self`) invisibly for chaining.
    set_object = function(x) {
      private$object <- x
      private$refresh()
      logger::log_trace("CallablePythonCode$set_object object set.")
      return(invisible(self))
    },
    #' @description
    #'   Execute `Callable` python code.
    #'
    #' @param args (`NULL` or named `list`)\cr
    #'  supplied for callable functions only, these are dynamic arguments passed to
    #'  `reticulate::py_run_string` or `reticulate::py_run_file`. Dynamic arguments
    #'  are executed in this call and are not saved which means that `self$get_call()`
    #'  won't include them later.
    #' @param try (`logical` value)\cr
    #'  whether perform function evaluation inside `try` clause
    #'
    #' @return nothing or output from function depending on `return`
    #' argument. If `run` fails it will return object of class `simple-error` error
    #' when `try = TRUE` or will stop if `try = FALSE`.
    run = function(args = NULL, try = FALSE) {
      withr::with_options(
        list(reticulate.engine.environment = private$env),
        res <- super$run(args = args, try = try)
      )
      if (is.null(res)) {
        stop("The specified python object returned NULL or does not exist in the python code")
      }
      res
    }
  ),

  ## __Private Fields ====
  private = list(
    object = NULL,
    duplicate_vars = list(), # variables that already exist in the global env but were supplied as pull vars
    vars_to_assign = list(), # during $run, these variables will be temporarily assigned to the .GlobalEnv
    ## __Private Methods ====
    # @description
    # Refresh call with function name and saved arguments
    #
    # @return nothing
    refresh = function() {
      # replaced str2lang found at:
      # https://rlang.r-lib.org/reference/call2.html
      private$call <- as.call(
        c(rlang::parse_expr(private$fun_name), private$args)
      )

      private$call <- rlang::parse_expr(
        sprintf("%s[[%s]]", deparse1(private$call, collapse = "\n"), deparse1(private$object, collapse = "\n"))
      )
    }
  )
)
## PythonCodeClass ====
#'
#' @title A `CallablePythonCode` class of objects
#' @description `r lifecycle::badge("experimental")`
#'
PythonCodeClass <- R6::R6Class( # nolint
  classname = "PythonCodeClass",
  inherit = CodeClass,

  ## __Public Methods ====
  public = list(
    #' @description
    #'   Evaluates internal code within environment
    #'
    #' @param vars (named `list`) additional pre-requisite vars to execute code
    #' @param dataname (`character`) name of the data frame object to be returned
    #' @param envir (`environment`) environment in which code will be evaluated
    #'
    #' @return `data.frame` containing the mutated dataset
    eval = function(vars = list(), dataname = NULL, envir = new.env(parent = parent.env(.GlobalEnv))) {
      checkmate::assert_list(vars, min.len = 0, names = "unique")
      execution_environment <- envir

      for (vars_idx in seq_along(vars)) {
        var_name <- names(vars)[[vars_idx]]
        var_value <- vars[[vars_idx]]
        if (inherits(var_value, "TealDatasetConnector") || inherits(var_value, "TealDataset")) {
          var_value <- get_raw_data(var_value)
        }
        assign(envir = execution_environment, x = var_name, value = var_value)
      }

      # execute
      rlang::with_options(
        super$eval(envir = execution_environment),
        reticulate.engine.environment = execution_environment
      )

      # return early if only executing and not grabbing the dataset
      if (is.null(dataname)) {
        return(as.environment(as.list(execution_environment)))
      }

      if (!is.data.frame(execution_environment[[dataname]])) {
        out_msg <- sprintf(
          "\n%s\n\n - Code from %s needs to return a data.frame assigned to an object of dataset name.",
          self$get_code(),
          self$get_dataname()
        )

        rlang::with_options(
          .expr = stop(out_msg, call. = FALSE),
          warning.length = max(min(8170, nchar(out_msg) + 30), 100)
        )
      }

      new_set <- execution_environment[[dataname]]
      logger::log_trace("PythonCodeClass$eval successfuly evaluated the code.")

      return(new_set)
    }
  )
)

#' Python Code
#'
#' `r lifecycle::badge("experimental")`
#' Create a python code object directly from python code or a
#' script containing python code.
#'
#' @details
#'   Used to mutate dataset connector objects with python code. See
#'   [`mutate_dataset`] or [`mutate_data`] for details.
#'
#' @param code (`character`)\cr
#'   Code to mutate the dataset. Must contain the `dataset$dataname`.
#' @param script (`character`)\cr
#'   file that contains python Code that can be read using `reticulate::py_run_script`.
#'
#' @return (`PythonCodeClass`) object containing python code
#' @export
#'
#' @examples
#' \dontrun{
#' library(scda)
#' library(reticulate)
#'
#' # mutate dataset object
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' x <- scda_cdisc_dataset_connector("ADSL", "adsl")
#'
#' x %>% mutate_dataset(python_code("import pandas as pd
#' r.ADSL = pd.DataFrame({'x': [1]})"))
#'
#' x$get_code()
#' x$pull()
#' x$get_raw_data()
#'
#' # mutate data object
#'
#' y <- 8
#' tc <- cdisc_data(
#'   scda_cdisc_dataset_connector("ADSL", "adsl"),
#'   scda_cdisc_dataset_connector("ADLB", "adlb")
#' )
#'
#' tc %>% mutate_data(python_code("import pandas as pd
#' r.ADSL = pd.DataFrame({'STUDYID': [r.y], 'USUBJID': [r.y]})"), vars = list(y = y))
#'
#'
#' load_datasets(tc) # submit all
#' ds <- tc$get_dataset("ADSL")
#' ds$get_raw_data()
#' }
python_code <- function(code = character(0), script = character(0)) {
  if (!xor(missing(code), missing(script))) stop("Exactly one of 'code' and 'script' is required")

  if (length(script) > 0) {
    code <- deparse(call("py_run_file", script))
  } else {
    code <- deparse(call("py_run_string", code))
  }
  py <- PythonCodeClass$new()
  py$set_code(code)

  return(py)
}
