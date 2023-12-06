#' Whether the variable name is good to use within Show R Code
#'
#' Spaces are problematic because the variables must be escaped
#' with backticks.
#' Also, they should not start with a number as R may silently make
#' it valid by changing it.
#' Therefore, we only allow alphanumeric characters with underscores.
#' The first character of the `name` must be an alphabetic character
#' and can be followed by alphanumeric characters.
#'
#' @md
#'
#' @param name `character, single or vector` name to check
#' @keywords internal
#'
#' @examples
#' teal.data:::check_simple_name("aas2df")
#' teal.data:::check_simple_name("ADSL")
#' teal.data:::check_simple_name("ADSLmodified")
#' teal.data:::check_simple_name("ADSL_2")
#' teal.data:::check_simple_name("a1")
#' # the following fail
#' \dontrun{
#' teal.data:::check_simple_name("1a")
#' teal.data:::check_simple_name("ADSL.modified")
#' teal.data:::check_simple_name("ADSL_modified")
#' teal.data:::check_simple_name("a1...")
#' }
check_simple_name <- function(name) {
  checkmate::assert_character(name, min.len = 1, any.missing = FALSE)
  if (!grepl("^[[:alpha:]][a-zA-Z0-9_]*$", name, perl = TRUE)) {
    stop(
      "name '",
      name,
      "' must only contain alphanumeric characters (with underscores)",
      " and the first character must be an alphabetic character"
    )
  }
}
