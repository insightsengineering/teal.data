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
#'  Access variables with [get()], [`$`], [teal.code::get_var()] or [`[[`].
#'  No setter provided. Evaluate code to add variables into `@.xData`.
#' @slot code (`list` of `character`) representing code necessary to reproduce the contents of `qenv`.
#'  Access with [teal.code::get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in
#' `@.xData`.
#'  Access or modify with [join_keys()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been
#'  proven to yield contents of `@.xData`.
#'  Used internally. See [`verify()`] for more details.
#'
#' @inheritSection teal.code::`qenv-class` Code
#'
#' @import teal.code
#' @keywords internal
setClass(
  Class = "teal_data",
  contains = "qenv",
  slots = c(join_keys = "join_keys", verified = "logical")
)

#' It initializes the `teal_data` class
#'
#' Accepts .xData as a list and converts it to an environment before initializing
#' parent constructor (`qenv`).
#' @noRd
setMethod(
  "initialize",
  "teal_data",
  function(.Object, .xData, join_keys, code, ...) { # nolint: object_name.
    print("init teal_data")
    if (missing(.xData)) .xData <- new.env()
    if (missing(join_keys)) join_keys <- teal.data::join_keys()
    if (missing(code)) code <- character(0L)
    args <- list(...)

    checkmate::assert_environment(.xData)
    checkmate::assert_class(join_keys, "join_keys")
    checkmate::assert_list(args, names = "named")
    if (!any(is.language(code), is.character(code))) {
      stop("`code` must be a character or language object.")
    }

    if (is.language(code)) {
      code <- paste(lang2calls(code), collapse = "\n")
    }

    methods::callNextMethod(
      .Object,
      .xData,
      join_keys = join_keys,
      verified = (length(code) == 0L && length(.xData) == 0L),
      code = code2list(code),
      ...
    )
  }
)

#' Reshape code to the list
#'
#' List will be divided by the calls. Each element of the list contains `id` and `dependency` attributes.
#'
#' @param code `character` with the code.
#'
#' @return list of `character`s of the length equal to the number of calls in `code`.
#'
#' @keywords internal
#' @noRd
code2list <- function(code) {
  checkmate::assert_character(code, null.ok = TRUE)
  if (length(code) == 0) {
    return(list())
  }

  parsed_code <- parse(text = code, keep.source = TRUE)

  code_list <- if (length(parsed_code)) {
    lapply(split_code(code), function(current_code) {
      parsed_code <- parse(text = current_code, keep.source = TRUE)
      attr(current_code, "dependency") <- extract_dependency(parsed_code)
      current_code
    })
  } else {
    # empty code like "", or just comments
    attr(code, "dependency") <- extract_dependency(parsed_code) # in case comment contains @linksto tag
    list(code)
  }
  names(code_list) <- sample.int(.Machine$integer.max, length(code_list))
  code_list
}
