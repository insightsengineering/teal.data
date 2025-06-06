#' Append elements to join_keys
#'
#' This function appends elements to the join_keys slot of a teal_data object.
#' It follows the same pattern as base R's append() function.
#'
#' @param x A teal_data object
#' @param values (`join_keys`) object to append
#' @param after Integer, the position after which the elements are to be appended.
#'   If negative or zero, the values are prepended to the join_keys.
#'   If missing, the values are appended at the end.
#'
#' @return A teal_data object with updated join_keys
#'
#' @examples
#' data <- teal_data()
#' keys1 <- join_keys(
#'   join_key("ADSL", "ADSL", "USUBJID"),
#'   join_key("ADAE", "ADAE", "USUBJID"),
#'   join_key("ADSL", "ADAE", c(USUBJID = "USUBJID"))
#' )
#' data <- append_join_keys(data, keys1)
#' join_keys(data)data
#'
#' @export
append_join_keys <- function(x, values, after = length(join_keys(x))) {
  checkmate::assert_class(x, "teal_data")
  checkmate::assert_int(after, lower = 0, upper = length(join_keys(x)), null.ok = TRUE)
  checkmate::assert_class(value, "join_keys")

  join_keys(x) <- append(join_keys(x), values, after = after)
  x
} 
