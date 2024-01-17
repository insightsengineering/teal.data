#' The names of a `join_keys` object
#'
#' @inheritParams base::`names<-`
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
