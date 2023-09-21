setOldClass("JoinKeys")

#' @import teal.code
#' @export
setClass(
  Class = "tdata",
  contains = "qenv",
  slots = c(join_keys = "JoinKeys", datanames = "character"),
  prototype = list(
    join_keys = join_keys(),
    datanames = character(0)
  )
)

#' Initialize `tdata` object
#'
#' Initialize `tdata` object.
#' @name new_tdata
#'
#' @param code (`character(1)` or `language`) code to evaluate. Accepts and stores comments also.
#' @param env (`list`) List of data.
#'
#' @examples
#' new_tdata(env = list(a = 1), code = quote(a <- 1))
#' new_tdata(env = list(a = 1), code = parse(text = "a <- 1"))
#' new_tdatas(env = list(a = 1), code = "a <- 1")
#'
#' @export
setGeneric("new_tdata", function(env = new.env(), code = expression(), keys = join_keys()) {
  standardGeneric("new_tdata")
})

#' @rdname new_tdata
#' @export
setMethod(
  "new_tdata",
  signature = c(env = "list", code = "expression", keys = "ANY"),
  function(env, code, keys = join_keys()) {
    new_env <- rlang::env_clone(list2env(env), parent = parent.env(.GlobalEnv))
    lockEnvironment(new_env, bindings = TRUE)
    id <- sample.int(.Machine$integer.max, size = length(code))
    methods::new(
      "tdata",
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

#' @rdname new_tdata
#' @export
setMethod(
  "new_tdata",
  signature = c(env = "list", code = "language", keys = "ANY"),
  function(env, code, keys = join_keys()) {
    code_expr <- as.expression(code)
    new_tdata(env = env, code = code_expr, keys = keys)
  }
)

#' @rdname new_tdata
#' @export
setMethod(
  "new_tdata",
  signature = c(env = "list", code = "character", keys = "ANY"),
  function(env, code, keys = join_keys()) {
    code_expr <- parse(text = code)
    new_tdata(env = env, code = code_expr, keys = keys)
  }
)
