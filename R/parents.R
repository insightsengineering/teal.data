#' A name
#' @export
parents <- function(join_keys_obj) {
  UseMethod("parents", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk)
parents.Placeholder <- function(join_keys_obj) {
  rlang::`%||%`(attr(join_keys_obj, "__parents__"), list())
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk) <- list(ADSL = "ADTTE")
`parents<-` <- function(join_keys_obj, value) {
  UseMethod("parents<-", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk) <- list(ds1 = "ds2", "ds3" = "ds4")
#' parents(jk)["ADTTE"] <- "ADSL"
#' parents(jk)["ADTTE"] <- "ADSL2"
`parents<-.Placeholder` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }
  checkmate::assert_list(value, types = "character", names = "named", min.len = 1)
  new_parents <- attr(join_keys_obj, "__parents__")

  for (dataset in names(value)) {
    parent <- new_parents[[dataset]]
    checkmate::assert(
      checkmate::check_null(parent),
      checkmate::check_true(
        length(parent) == 0 &&
          length(value[[dataset]]) == 0
      ),
      checkmate::check_true(parent == value[[dataset]]),
      "Please check the difference between provided datasets parents and provided join_keys parents."
    )
    if (is.null(parent)) {
      new_parents[[dataset]] <- value[[dataset]]
    }
  }
  attr(join_keys_obj, "__parents__") <- new_parents
  join_keys_obj
}

# -----------------------------------------------------------------------------
#
# Helpers (non-exported)

#' Check if parent/child are valid
#'
#' @keywords internal
#' @examples
#' jk <- new_join_keys()
#' jk["ds1", "ds2"] <- character(0)
#' parents(jk) <- list(ds1 = "ds2")
#' check_parent_child(jk)
check_parent_child <- function(join_keys_obj) {
  jk_parents <- parents(join_keys_obj)
  if (length(jk_parents) > 0) {
    for (idx1 in seq_along(jk_parents)) {
      name_from <- names(jk_parents)[[idx1]]
      for (idx2 in seq_along(jk_parents[[idx1]])) {
        name_to <- jk_parents[[idx1]][[idx2]]
        keys_from <- join_keys_obj[name_from, name_to]
        keys_to <- join_keys_obj[name_to, name_from]
        if (length(keys_from) == 0 && length(keys_to) == 0) {
          stop(sprintf("No join keys from %s to its parent (%s) and vice versa", name_from, name_to))
        }
        if (length(keys_from) == 0) {
          stop(sprintf("No join keys from %s to its parent (%s)", name_from, name_to))
        }
        if (length(keys_to) == 0) {
          stop(sprintf("No join keys from %s parent name (%s) to %s", name_from, name_to, name_from))
        }
      }
    }
  }
}
