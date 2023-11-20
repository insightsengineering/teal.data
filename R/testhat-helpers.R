#' Test if Two Objects are (Nearly) Equal
#'
#' `all.equal(target, current)` is a utility to compare `join_keys` objects target
#' and current testing `near equality`.
#'
#' If they are different, comparison is still made to some extent, and a report
#' of the differences is returned.
#' Do not use all.equal directly in if expressions—either use `isTRUE(all.equal(....))`
#' or identical if appropriate.
#'
#' @inheritParams base::all.equal
#' @param ... further arguments for different methods. Not used with `join_keys`.
#'
#' @details
#' The parents attribute comparison tolerates `NULL` and empty lists and will find
#' no difference.
#'
#' The list containing all the relationships is treated like a map and ignores
#' entries with `NULL` if they exist.
#'
#' @seealso [base::all.equal()]
#' @keywords internal
all.equal.join_keys <- function(target, current, ...) {
  .as_map <- function(.x) {
    old_attributes <- attributes(.x)
    # Keep only non-list attributes
    old_attributes[["names"]] <- NULL
    old_attributes[["original_class"]] <- old_attributes[["class"]]
    old_attributes[["class"]] <- NULL
    old_attributes[["__parents__"]] <- if (!length(old_attributes[["__parents__"]])) {
      list()
    } else {
      old_attributes[["__parents__"]][order(names(old_attributes[["__parents__"]]))]
    }
    attr(.x, "class") <- "list"

    # Remove nulls
    .x <- Filter(Negate(is.null), .x)

    # Sort named components, preserving positions of unnamed
    nx <- rlang::names2(.x)
    is_named <- nx != ""
    if (any(is_named)) {
      idx <- seq_along(.x)
      idx[is_named] <- idx[is_named][order(nx[is_named])]
      .x <- .x[idx]
    }
    attributes(.x) <- utils::modifyList(old_attributes, attributes(.x) %||% list())
    .x
  }
  x <- .as_map(target)
  y <- .as_map(current)
  print("inner")
  all.equal(x, y)
}
