#' Helper function to deep copy `R6` object
#'
#' When cloning an R6 object the private function
#' `deep_clone` is automatically used. To ensure a complete
#' clone the private function should call this function
#'
#' @param name (`character`) argument passed by `deep_clone` function.
#' @param value (any `R` object) argument passed by `deep_clone` function.
#' @keywords internal
deep_clone_r6 <- function(name, value) {
  if (checkmate::test_list(value, types = "R6")) {
    lapply(value, function(x) x$clone(deep = TRUE))
  } else if (R6::is.R6(value)) {
    value$clone(deep = TRUE)
  } else if (is.environment(value)) {
    new_env <- as.environment(as.list(value, all.names = TRUE))
    parent.env(new_env) <- parent.env(value)
    new_env
  } else {
    value
  }
}
