#' Helper function to assert if two key sets contain incompatible keys
#'
#' return TRUE if compatible, throw error otherwise
#' @keywords internal
assert_compatible_keys <- function(join_key_1, join_key_2) {
  stop_message <- function(dataset_1, dataset_2) {
    stop(
      paste("cannot specify multiple different join keys between datasets:", dataset_1, "and", dataset_2)
    )
  }

  dataset_1_one <- names(join_key_1)
  dataset_2_one <- names(join_key_1[[1]])
  keys_one <- join_key_1[[1]][[1]]

  dataset_1_two <- names(join_key_2)
  dataset_2_two <- names(join_key_2[[1]])
  keys_two <- join_key_2[[1]][[1]]

  # if first datasets and the second datasets match and keys
  # must contain the same named elements
  if (dataset_1_one == dataset_1_two && dataset_2_one == dataset_2_two) {
    if (!identical(sort(keys_one), sort(keys_two))) {
      stop_message(dataset_1_one, dataset_2_one)
    }
  }

  # if first dataset of join_key_1 matches second dataset of join_key_2
  # and the first dataset of join_key_2 must match second dataset of join_key_1
  # and keys must contain the same elements but with names and values swapped
  if (dataset_1_one == dataset_2_two && dataset_2_one == dataset_1_two) {
    if (
      xor(length(keys_one) == 0, length(keys_two) == 0) ||
        !identical(sort(keys_one), sort(stats::setNames(names(keys_two), keys_two)))
    ) {
      stop_message(dataset_1_one, dataset_2_one)
    }
  }

  # otherwise they are compatible
  return(TRUE)
}

#' Helper function checks the parent-child relations are valid
#'
#' @param x (`join_keys`) object to assert validity of relations
#'
#' @return `join_keys` invisibly
#'
#' @keywords internal
assert_parent_child <- function(x) {
  jk <- join_keys(x)
  jk_parents <- parents(jk)

  checkmate::assert_class(jk, c("join_keys", "list"))

  if (!is.null(jk_parents)) {
    for (idx1 in seq_along(jk_parents)) {
      name_from <- names(jk_parents)[[idx1]]
      for (idx2 in seq_along(jk_parents[[idx1]])) {
        name_to <- jk_parents[[idx1]][[idx2]]
        keys_from <- jk[[name_from]][[name_to]]
        keys_to <- jk[[name_to]][[name_from]]
        if (length(keys_from) == 0 && length(keys_to) == 0) {
          stop(sprintf("No join keys from %s to its parent (%s) and vice versa", name_from, name_to))
        }
      }
    }
  }
  invisible(x)
}

assert_compatible_keys2 <- function(x, y) {
  # Helper to flatten join_keys / join_key_set
  flatten_join_key_sets <- function(value) {
    value <- unclass(value)
    Reduce(
      init = list(),
      f = function(u, v, ...) {
        el <- value[v][[1]]
        res <- lapply(seq_along(el), function(ix) el[ix])
        names(res) <- rep(v, length(res))
        append(u, res)
      },
      x = names(value)
    )
  }

  x <- flatten_join_key_sets(x)
  y <- flatten_join_key_sets(y)

  for (idx_1 in seq_along(x)) {
    for (idx_2 in seq_along(y)) {
      assert_compatible_keys(x[idx_1], y[idx_2])
    }
  }
  TRUE
}

#' Updates the keys of the datasets based on the parents.
#'
#' @param x (`join_keys`) object to update the keys.
#'
#' @return (`self`) invisibly for chaining
#'
#' @keywords internal
update_keys_given_parents <- function(x) {
  jk <- join_keys(x)

  checkmate::assert_class(jk, "join_keys", .var.name = checkmate::vname(x))

  datanames <- names(jk)
  duplicate_pairs <- list()
  for (d1 in datanames) {
    d1_pk <- jk[[d1]][[d1]]
    d1_parent <- parent(jk, d1)
    for (d2 in datanames) {
      if (identical(d2, d1) || paste(d2, d1) %in% duplicate_pairs) {
        next
      }
      if (length(jk[[d1]][[d2]]) == 0) {
        d2_parent <- parent(jk, d2)
        d2_pk <- jk[[d2]][[d2]]

        fk <- if (identical(d1, d2_parent)) {
          # first is parent of second -> parent keys -> first keys
          d1_pk
        } else if (identical(d1_parent, d2)) {
          # second is parent of first -> parent keys -> second keys
          d2_pk
        } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
          # both has the same parent -> common keys to parent
          keys_d1_parent <- sort(jk[[d1]][[d1_parent]])
          keys_d2_parent <- sort(jk[[d2]][[d2_parent]])

          common_ix_1 <- unname(keys_d1_parent) %in% unname(keys_d2_parent)
          common_ix_2 <- unname(keys_d2_parent) %in% unname(keys_d1_parent)

          if (all(!common_ix_1)) {
            # No common keys between datasets - leave empty
            next
          }

          structure(
            names(keys_d2_parent)[common_ix_2],
            names = names(keys_d1_parent)[common_ix_1]
          )
        } else {
          # cant find connection - leave empty
          next
        }
        jk[[d1]][[d2]] <- fk # mutate join key
        duplicate_pairs <- append(duplicate_pairs, paste(d1, d2))
      }
    }
  }
  # check parent child relation
  assert_parent_child(x = jk)

  jk
}
