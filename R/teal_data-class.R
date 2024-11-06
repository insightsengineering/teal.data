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
#' @slot .xData (`environment`) environment containing data sets and possibly
#'  auxiliary variables.
#'  Access variables with [get()], [`$`], [get_var()] or [`[[`].
#'  No setter provided. Evaluate code to add variables into `@.xData`.
#' @slot code (`character`) vector representing code necessary to reproduce the
#'  contents of `@.xData`.
#'  Access with [get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#' @slot id (`integer`) random identifier assigned to each element of `@code`.
#'  Used internally.
#' @slot warnings (`character`) vector of warnings raised when evaluating code.
#'  Access with [get_warnings()].
#' @slot messages (`character`) vector of messages raised when evaluating code.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in
#' `@.xData`.
#'  Access or modify with [join_keys()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been
#'  proven to yield contents of `@.xData`.
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

#' It initializes the `teal_data` class
#'
#' Accepts .xData as a list and converts it to an environment before initializing
#' parent constructor (`qenv`).
#' @noRd
setMethod(
  "initialize",
  "teal_data",
  function(.Object, .xData = list(), join_keys = join_keys(), ...) { # nolint: object_name.
    # Allow .xData to be a list and convert it to an environment
    if (!missing(.xData) && inherits(.xData, "list")) {
      .xData <- rlang::env_clone(list2env(.xData), parent = parent.env(.GlobalEnv)) # nolint: object_name.
      lockEnvironment(.xData, bindings = TRUE)
    }
    checkmate::assert_environment(.xData)

    .Object <- methods::callNextMethod(.Object, .xData, join_keys = join_keys, ...) # nolint: object_name.

    # teal data specific slots
    checkmate::assert_class(join_keys, "join_keys")
    .Object@verified <- (length(.Object@code) == 0L && length(.Object@.xData) == 0L) # nolint: object_name.
    .Object@join_keys <- join_keys # nolint: object_name.

    .Object
  }
)
