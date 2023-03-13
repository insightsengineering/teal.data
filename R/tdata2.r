setOldClass("JoinKeys")

#' @export
setClass(
  Class = "tdata2",
  slots = c(qenv = "qenv", join_keys = "JoinKeys", datanames = "character"),
  prototype = list(
    qenv = teal.code::new_qenv(),
    join_keys = join_keys(),
    datanames = character(0)
  )
)

#' @export
setGeneric("new_tdata2", function(data = list(), code = character(0), join_keys = join_keys()) {
  standardGeneric("new_tdata2")
})

#' @export
setMethod(
  "new_tdata2",
  signature = c(data = "list", code = "ANY", join_keys = "ANY"),
  function(data, code, join_keys) {
    qenv <- teal.code::new_qenv(env = list2env(data), code = code)
    methods::new(
      "tdata2",
      qenv = qenv,
      join_keys = join_keys,
      datanames = names(data)
    )
  }
)

#' @importFrom teal.code eval_code
#' @export
setMethod("eval_code", signature = c("tdata2", "ANY"), function(object, code) {
  teal.code::eval_code(object@qenv, code = code)
})

#' @export
setMethod("[[", signature = c("tdata2", "ANY", "missing"), function(x, i, j, ...) {
  x@qenv[[i]]
})

#' @export
setMethod("names", signature = "tdata2", function(x) {
  ls(x@qenv)
})
