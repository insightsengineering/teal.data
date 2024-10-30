setOldClass("join_keys")

#' Reproducible data
#'
#' Reproducible data container class. Inherits code tracking behavior from [`teal.code::qenv-class`].
#'
#' This class provides an isolated environment in which to store and process data with all code being recorded.
#' The environment, code, data set names, and data joining keys are stored in their respective slots.
#' These slots should never be accessed directly, use the provided get/set functions.
#'
#' As code is evaluated in `teal_data`, messages and warnings are stored in their respective slots.
#' If errors are raised, a `qenv.error` object is returned.
#'
#' @name teal_data-class
#' @rdname teal_data-class
#'
#' @slot env (`environment`) environment containing data sets and possibly auxiliary variables.
#'  Access variables with [get_var()] or [`[[`].
#'  No setter provided. Evaluate code to add variables into `@env`.
#' @slot code (`character`) vector representing code necessary to reproduce the contents of `@env`.
#'  Access with [get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#' @slot id (`integer`) random identifier assigned to each element of `@code`. Used internally.
#' @slot warnings (`character`) vector of warnings raised when evaluating code.
#'  Access with [get_warnings()].
#' @slot messages (`character`) vector of messages raised when evaluating code.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in `@env`.
#'  Access or modify with [join_keys()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been proven to yield contents of `@env`.
#'  Used internally. See [`verify()`] for more details.
#'
#' @import teal.code
#' @keywords internal
setClass(
  Class = "teal_data",
  contains = "qenv",
  slots = c(join_keys = "join_keys", verified = "logical"),
  prototype = list(
    join_keys = join_keys(),
    verified = logical(0)
  )
)

#' Initialize `teal_data` object
#'
#' @name new_teal_data
#'
#' @param data (`named list`) of data objects.
#' @param code (`character` or `language`) code to reproduce the `data`.
#'   Accepts and stores comments also.
#' @param join_keys (`join_keys`) object
#' @rdname new_teal_data
#' @keywords internal
new_teal_data <- function(data, code = character(0), join_keys = join_keys()) {
  checkmate::assert_list(data)
  checkmate::assert_class(join_keys, "join_keys")
  if (!any(is.language(code), is.character(code))) {
    stop("`code` must be a character or language object.")
  }

  if (is.language(code)) {
    code <- paste(lang2calls(code), collapse = "\n")
  }
  if (length(code)) {
    code <- paste(code, collapse = "\n")
  }
  verified <- (length(code) == 0L && length(data) == 0L)

  id <- sample.int(.Machine$integer.max, size = length(code))

  new_env <- rlang::env_clone(list2env(data), parent = parent.env(.GlobalEnv))
  lockEnvironment(new_env, bindings = TRUE)

  methods::new(
    "teal_data",
    .xData = new_env,
    code = code,
    warnings = rep("", length(code)),
    messages = rep("", length(code)),
    id = id,
    join_keys = join_keys,
    verified = verified
  )
}
