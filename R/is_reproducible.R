#' Reproducibility Check for `@code` Slot in `teal_data`
#' @param teal_data `teal_data` object
#' @examples
#' tdata1 <- teal_data()
#' tdata1 <- within(tdata1, {
#'   a <- 1
#'   b <- a^5
#'   c <- list(x = 2)
#' })
#' is_reproducible(tdata1)
#' @export
is_reproducible <- function(teal_data) {
  checkmate::assert_class(teal_data, "teal_data")
  if (teal_data@valid) {
    teal_data
  }
  hashes_qenv <- sapply(ls(teal_data@env), function(x) digest::digest(get(x, env = teal_data@env)))
  eval_env <- new.env()
  eval(parse(text = teal_data@code), envir = eval_env)
  hashes_eval_qenv <- sapply(ls(eval_env), function(x) digest::digest(get(x, env = eval_env)))

  reproducible <- identical(hashes_qenv, hashes_eval_qenv)
  if (reproducible) {
    teal_data@valid <- TRUE
  } else {
    stop("@env is not reproducible with @code.")
  }
}
