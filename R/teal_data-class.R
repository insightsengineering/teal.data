setOldClass("JoinKeys")

#' @import teal.code
#' @export
setClass(
  Class = "teal_data",
  contains = "qenv",
  slots = c(join_keys = "JoinKeys", datanames = "character"),
  prototype = list(
    join_keys = join_keys(),
    datanames = character(0)
  )
)

#' Initialize `teal_data` object
#'
#' Initialize `teal_data` object.
#' @name new_teal_data
#'
#' @param code (`character(1)` or `language`) code to evaluate. Accepts and stores comments also.
#' @param env (`list`) List of data.
#' @param keys (`JoinKeys`) object
#' @param datanames (`character`) names of datasets in `env`. Needed when non-dataset
#'   objects are needed in the `env` slot.
#'
#' @examples
#' new_teal_data(env = list(a = 1), code = quote(a <- 1))
#' new_teal_data(env = list(a = 1), code = parse(text = "a <- 1"))
#' new_teal_data(env = list(a = 1), code = "a <- 1")
#'
#' @export
setGeneric("new_teal_data", function(env = new.env(), code = expression(), keys = join_keys(), datanames = character()) {
  standardGeneric("new_teal_data")
})

#' @rdname new_teal_data
#' @export
setMethod(
  "new_teal_data",
  signature = c(env = "list", code = "expression", keys = "ANY"),
  function(env, code, keys = join_keys(), datanames = names(env)) {
    new_env <- rlang::env_clone(list2env(env), parent = parent.env(.GlobalEnv))
    lockEnvironment(new_env, bindings = TRUE)
    id <- sample.int(.Machine$integer.max, size = length(code))
    methods::new(
      "teal_data",
      env = new_env,
      code = code,
      warnings = rep("", length(code)),
      messages = rep("", length(code)),
      id = id,
      join_keys = keys,
      datanames = as.character(union(names(env), names(keys$get())))
    )
  }
)

#' @rdname new_teal_data
#' @export
setMethod(
  "new_teal_data",
  signature = c(env = "list", code = "language", keys = "ANY"),
  function(env, code, keys = join_keys(), datanames = names(env)) {
    code_expr <- as.expression(code)
    new_teal_data(env = env, code = code_expr, keys = keys, datanames = datanames)
  }
)

#' @rdname new_teal_data
#' @export
setMethod(
  "new_teal_data",
  signature = c(env = "list", code = "character", keys = "ANY"),
  function(env, code, keys = join_keys(), datanames = names(env)) {
    code_expr <- parse(text = code)
    new_teal_data(env = env, code = code_expr, keys = keys, datanames = datanames)
  }
)
