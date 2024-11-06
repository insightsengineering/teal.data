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
#' @slot code (`list` of `character`) representing code necessary to reproduce the contents of `@env`.
#'  Access with [get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#'  Read more in Code section.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in `@env`.
#'  Access or modify with [join_keys()].
#' @slot datanames (`character`) vector of names of data sets in `@env`.
#'  Used internally to distinguish them from auxiliary variables.
#'  Access or modify with [datanames()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been proven to yield contents of `@env`.
#'  Used internally. See [`verify()`] for more details.
#'
#' @section Code:
#'
#' Each code element is a character representing one call. Each element has possible attributes:
#' - `warnings` (`character`) the warnings output when evaluating the code element
#' - `messages` (`character`) the messages output when evaluating the code element
#' - `id (`integer`) random identifier of the code element to make sure uniqueness when joining
#' - `dependency` (`character`) names of objects that appear in this call and gets affected by this call,
#' separated by `<-` (objects on LHS of `<-` are affected by this line, and objects on RHS are affecting this line)
#'
#' @import teal.code
#' @keywords internal
setClass(
  Class = "teal_data",
  contains = "qenv",
  slots = c(join_keys = "join_keys", datanames = "character", verified = "logical"),
  prototype = list(
    join_keys = join_keys(),
    datanames = character(0),
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
#' @param datanames (`character`) names of datasets passed to `data`.
#'   Needed when non-dataset objects are needed in the `env` slot.
#' @rdname new_teal_data
#' @keywords internal
new_teal_data <- function(data,
                          code = character(0),
                          join_keys = join_keys(),
                          datanames = names(data)) {
  checkmate::assert_list(data)
  checkmate::assert_class(join_keys, "join_keys")
  if (is.null(datanames)) datanames <- character(0) # todo: allow to specify
  checkmate::assert_character(datanames)
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

  new_env <- rlang::env_clone(list2env(data), parent = parent.env(.GlobalEnv))
  lockEnvironment(new_env, bindings = TRUE)

  # GO WITH BELOW OR
  # apply eval_code(code) in here
  # but eval_code() works on an object and teal_data/qenv does not exist YET
  attr(code, "id") <- sample.int(.Machine$integer.max, 1)
  if (length(code)) {
    split_code <- teal.code:::split_code(code)
    split_code <- lapply(split_code, function(current_code) {
      attr(current_code, "id") <- sample.int(.Machine$integer.max, 1)
      current_call <- parse(text = trimws(current_code), keep.source = TRUE)
      pd <- utils::getParseData(current_call)
      pd <- teal.code:::normalize_pd(pd)
      call_pd <- teal.code:::extract_calls(pd)[[1]]

      attr(current_code, "dependency") <-
        c(teal.code:::extract_side_effects(call_pd), teal.code:::extract_occurrence(call_pd))
      current_code
    })
  } else {
    split_code <- list(character(0))
    # TODO: do we assign
    # attr(split_code[[1]], "id") <- sample.int() ?
    # TODO: do we need
    # attr(split_code[[1]], "dependency") <- cahracter(0) ?
  }

  # TODO:
  # warnings are empty and messages are empty
  # should we reconsider `@verified` field and should we reconsider eval_code in here?

  datanames <- .get_sorted_datanames(datanames = datanames, join_keys = join_keys, env = new_env)

  methods::new(
    "teal_data",
    env = new_env,
    code = code,
    join_keys = join_keys,
    datanames = datanames,
    verified = verified
  )
}
