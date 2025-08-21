#' Check Compatibility of keys
#'
#' Helper function to assert if two key sets contain incompatible keys.
#'
#' @return Returns `TRUE` if successful, otherwise raises error.
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

  TRUE # otherwise they are compatible
}

#' Validate parent-child key
#'
#' Helper function checks the parent-child relations are valid.
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

#' Verify key set compatibility
#'
#' Helper function to ensuring compatibility between two sets of keys
#'
#' @return Returns `TRUE` if successful, otherwise raises error.
#' @keywords internal
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

#' Append indirect links
#'
#' Adds the keys between datasets if they are connected by the same set of keys
#' via other foreign datasets.
#'
#' @param x (`join_keys`) object to update the keys.
#'
#' @return (`self`) invisibly for chaining
#'
#' @keywords internal
.append_indirect_links <- function(x) {
  for (node_name in names(x)) {
    x <- .search_recursive(x, node_name)
  }
  x
}

.search_recursive <- function(x, node_name, parent_name, nodes_visited = node_name) {
  children_names <- setdiff(names(x[[node_name]]), nodes_visited)
  nodes_visited <- c(nodes_visited, children_names)
  if (length(children_names)) {
    for (child_name in children_names) {
      # todo: make sure they are connected correctly with the keys
      if (!missing(parent_name)) {
        ancestors_key_pair <- x[[parent_name]][[node_name]] # !important: using x[a, b] will result in infinite loop
        this_key_pair <- x[[node_name]][[child_name]]
        if (!identical(unname(ancestors_key_pair), names(this_key_pair))) next
        x <- c(
          x,
          join_key(
            dataset_1 = parent_name,
            dataset_2 = child_name,
            keys = setNames(unname(this_key_pair), names(ancestors_key_pair)),
            directed = FALSE
          )
        )
      }
      x <- .search_recursive(x = x, node_name = child_name, parent_name = node_name, nodes_visited = nodes_visited)
      nodes_visited <- union(nodes_visited, names(x[[node_name]]))
    }
  }
  x
}
