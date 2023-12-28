#' The Names of an `join_keys` Object
#' @inheritParams base::`names<-`
#' @return For `names`, it returns `NULL` or a character vector containing the names of datasets.\cr
#' For `names<-.join_keys`, it returns the updated 'join_keys' object with the keys modified.
#' (Note that the value of names(x) <- value is that of the assignment,
#' value, not the return value from the left-hand side.)
#' @export
`names<-.join_keys` <- function(x, value) {
  new_x <- unclass(x)
  parent_list <- parents(x)
  # Update inner keys
  for (old_name in setdiff(names(new_x), value)) {
    old_entry <- new_x[[old_name]]
    new_name <- value[names(new_x) == old_name]

    # Change 2nd-tier first
    for (sub_name in names(old_entry)) {
      names(new_x[[sub_name]])[names(new_x[[sub_name]]) == old_name] <- new_name
    }

    # Change in first tier
    names(new_x)[names(new_x) == old_name] <- new_name

    # changing name in the parents
    if (length(parent_list)) {
      names(parent_list)[names(parent_list) == old_name] <- new_name
      ind <- vapply(parent_list, identical, logical(1), old_name)
      parent_list[ind] <- new_name
      attr(new_x, "parents") <- parent_list
    }
  }

  class(new_x) <- c("join_keys", "list")
  new_x
}
