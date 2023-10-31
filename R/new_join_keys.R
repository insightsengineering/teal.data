#' Setter for join keys
#'
#' @param data (`JoinKeys`) empty object to set the new relationship pairs.
#' @param value (`JoinKeySet` or list of `JoinKeySet`) relationship pairs to add
#' to `JoinKeys` list.
#'
#' @rdname get_keys
`join_keys<-` <- function(join_keys_obj, value) {
  UseMethod("join_keys<-", join_keys_obj)
}

#' @rdname get_keys
#' @export
#' @examples
#' jk <- new_join_keys()
#' join_keys(jk)
#' join_keys(jk) <- join_key("ds1", "ds2", "some_col2")
`join_keys<-.Placeholder` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }

  if (length(join_keys_obj) > 0) {
    stop("Keys already set, please use mutate_join_keys() or to change them")
  }

  if (inherits(value, "JoinKeySet")) value <- list(value)

  checkmate::assert_list(value, types = "JoinKeySet", min.len = 1)

  # check if any JoinKeySets share the same datasets but different values
  for (idx_1 in seq_along(value)) {
    for (idx_2 in seq_along(value[idx_1])) {
      assert_compatible_keys(value[[idx_1]], value[[idx_2]])
    }
    join_keys_obj <- join_pair(join_keys_obj, value[[idx_1]])
  }

  logger::log_trace("JoinKeys keys are set.")

  join_keys_obj
}

#' @title Getter for JoinKeys that returns the relationship between pairs of datasets
#' @param x JoinKeys object to extract the join keys
#' @param dataset_1 (`character`) name of first dataset.
#' @param dataset_2 (`character`) name of second dataset.
#'
#' @export
#'
#' @examples
#' jk <- new_join_keys()
#' join_keys(jk) <- list(ds1 = list(ds2 = "some_col"))
#' jk["ds1", "ds2"]
`[.Placeholder` <- function(x, dataset_1, dataset_2 = dataset_1) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  x[[dataset_1]][[dataset_2]]
}

#' @rdname sub-.Placeholder
#' @param keys value to assign
#' @export
#' @keywords internal
#' @examples
#' jk <- new_join_keys()
#' jk["ds1", "ds2"]
#' join_keys(jk) <- join_key("ds1", "ds2", "some_col")
#' jk["ds1", "ds2"]
#' jk["ds1", "ds2"] <- "new_col"
#' jk["ds1", "ds2"]
`[<-.Placeholder` <- function(data, dataset_1, dataset_2 = dataset_1, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  if (is.null(data[[dataset_1]])) data[[dataset_1]] <- list()

  data[[dataset_1]][[dataset_2]] <- value

  if (identical(dataset_1, dataset_2)) {
    return(data)
  }

  if (is.null(data[[dataset_2]])) data[[dataset_2]] <- list()

  if (
    checkmate::test_character(value, min.len = 1) &&
      !checkmate::test_names(names(value))
  ) {
    value <- setNames(value, value)
  } else if (
    checkmate::test_character(value, min.len = 1)
  ) {
    # Invert key
    value <- setNames(names(value), value)
  }

  data[[dataset_2]][[dataset_1]] <- value

  data
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' jk <- new_join_keys()
#' join_keys(jk) <- list(ds1 = list(ds2 = "some_col"))
#' mutate_join_keys(jk, "ds2", "ds3", "another")
mutate_join_keys.Placeholder <- function(x, dataset_1, dataset_2, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  checkmate::assert_character(value, any.missing = FALSE)

  res <- join_pair(x, join_key(dataset_1, dataset_2, value))

  logger::log_trace(
    sprintf(
      "JoinKeys updated the keys between %s and %s to %s",
      dataset_1,
      dataset_2,
      paste(val, collapse = ", ")
    )
  )

  res
}

#' Split the `JoinKeys` object into a named list of join keys objects with an
#' element for each dataset
#'
#' @return (`list`) a list of `JoinKeys` object
#' @export
#' @examples
#' jk <- new_join_keys()
#' jk["ds1", "ds2"] <- "some_col"
#' jk["ds1", "ds3"] <- "new_col"
#' split_join_keys(jk)
split_join_keys <- function(keys) {
  checkmate::assert_multi_class(keys, classes = c("JoinKeys", "Placeholder"))

  if (checkmate::test_class(keys, "JoinKeys")) {
    keys <- keys$get()
    class(keys) <- c("Placeholder", "list")
  }

  list_of_list_of_join_key_set <- lapply(
    names(keys),
    function(dataset_1) {
      lapply(
        names(keys[[dataset_1]]),
        function(dataset_2) join_key(dataset_1, dataset_2, get_join_key(keys, dataset_1, dataset_2))
      )
    }
  )
  res <- lapply(list_of_list_of_join_key_set, function(.x) do.call(join_keys, .x))
  names(res) <- names(keys)

  logger::log_trace("JoinKeys keys split.")
  return(res)
}

#' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
#'
#' @param keys_1 `JoinKeys` object
#' @param keys_2  `list` of `JoinKeys` objects or single `JoinKeys` object
#'
#' @return (`JoinKeys`) a new object with the resulting merge
#'
#' @export
#'
#' @examples
#' jk1 <- new_join_keys()
#' jk1["ds1", "ds2"] <- "some_col"
#' jk2 <- new_join_keys()
#' jk2["ds1", "ds3"] <- "new_col"
#' merge_join_keys(jk1, jk2)
merge_join_keys <- function(keys_1, keys_2) {
  if (checkmate::test_class(keys_1, "JoinKeys")) {
    keys_1 <- keys_1$get()
    class(keys_1) <- c("Placeholder", "list")
  }
  checkmate::assert_multi_class(keys_1, c("JoinKeys", "Placeholder"))

  if (inherits(keys_2, c("JoinKeys", "Placeholder"))) keys_2 <- list(keys_2)
  checkmate::assert_list(keys_2, types = c("JoinKeys", "Placeholder"), min.len = 1)

  new_keys <- keys_1

  for (jk in keys_2) {
    if (checkmate::test_class(jk, "JoinKeys")) jk <- jk$get()
    for (dataset_1 in names(jk)) {
      for (dataset_2 in names(jk[[dataset_1]])) {
        new_keys[dataset_1, dataset_2] <- jk[[dataset_1]][[dataset_2]]
      }
    }
  }
  logger::log_trace("JoinKeys keys merged.")
  return(new_keys)
}

#' Updates the keys of the datasets based on the parents.
#'
#' @return (`self`) invisibly for chaining
#'
#' @export
#'
#' @examples
update_keys_given_parents <- function(join_keys_obj) {
  jk <- join_keys(join_keys_obj)

  checkmate::assert_class(jk, "Placeholder", .var.name = vname(join_keys_obj))

  datanames <- names(jk)
  duplicate_pairs <- list()
  for (d1 in datanames) {
    d1_pk <- jk[d1, d1]
    d1_parent <- parents(jk)[[d1]]
    for (d2 in datanames) {
      if (paste(d2, d1) %in% duplicate_pairs) {
        next
      }
      if (length(jk[d1, d2]) == 0) {
        d2_parent <- parents(jk)[[d2]]
        d2_pk <- jk[d2, d2]

        fk <- if (identical(d1, d2_parent)) {
          # first is parent of second -> parent keys -> first keys
          d1_pk
        } else if (identical(d1_parent, d2)) {
          # second is parent of first -> parent keys -> second keys
          d2_pk
        } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
          # both has the same parent -> parent keys
          jk[d1_parent, d1_parent]
        } else {
          # cant find connection - leave empty
          next
        }
        jk <- mutate_join_keys(jk, d1, d2, fk)
        duplicate_pairs <- append(duplicate_pairs, paste(d1, d2))
      }
    }
  }
  # check parent child relation
  assert_parent_child(join_keys_obj = jk)

  jk
}

#' Prints `JoinKeys`.
#'
#' @param ... additional arguments to the printing method
#' @return the `x` parameter
#'
#' @export
print.Placeholder <- function(x, ...) {
  check_ellipsis(...)
  keys_list <- x
  class(keys_list) <- "list"
  if (length(keys_list) > 0) {
    cat(sprintf(
      "A JoinKeys object containing foreign keys between %s datasets:\n",
      length(keys_list)
    ))
    print.default(keys_list)
  } else {
    cat("An empty JoinKeys object.")
  }
  invisible(x)
}

# -----------------------------------------------------------------------------
#
#
# Helpers (non-exported)
#

#' Internal constructor
#'
#' @return an empty `JoinKeys` list
#'
#' @keywords internal
new_join_keys <- function() {
  result <- list()
  class(result) <- c("Placeholder", "list")
  result
}

#' Get value of a single relationship pair
#'
#' @param join_keys_obj (`JoinKeys`) object that holds the relationship keys.
#' @param dataset_1 (`character(1)`) one of the datasets to retrieve keys (
#' order of the datasets is irrelevant).
#' @param dataset_2 (`character(1)`) the other dataset to retrieve keys (the
#' order of the datasets is irrelevant).
#'
#' @return Character vector with keys or (if one of the datasets is omitted) a
#' list of relationship pairs. If both datasets are omitted it returens the
#' `JoinKeys` object
#'
#' @keywords internal
get_join_key <- function(join_keys_obj, dataset_1, dataset_2) {
  checkmate::assert_multi_class(join_keys_obj, c("teal_data", "Placeholder"))
  jk <- join_keys(join_keys_obj)

  if (missing(dataset_1) && missing(dataset_2)) {
    return(jk)
  }
  if (missing(dataset_2)) {
    return(jk[[dataset_1]])
  }
  if (missing(dataset_1)) {
    return(jk[[dataset_2]])
  }
  if (is.null(jk[[dataset_1]][[dataset_2]])) {
    return(character(0))
  }
  return(jk[[dataset_1]][[dataset_2]])
}

#' Helper function to add a new pair to a `JoinKeys` object
#'
#' @param join_keys_obj (`JoinKeys`) Object with existing pairs.
#' @param join_key_obj (`JoinKeySet`) relationship pair to add.
#'
#' @examples
#' jk <- new_join_keys()
#' jk <- join_pair(jk, join_key("ds1", "ds2", "value"))
#' jk <- join_pair(jk, join_key("ds3", "ds2", "value"))
join_pair <- function(join_keys_obj, join_key_obj) {
  checkmate::assert_multi_class(join_keys_obj, c("JoinKeys", "Placeholder"))
  checkmate::assert_class(join_key_obj, "JoinKeySet")

  if (checkmate::test_class(join_keys_obj, "JoinKeys")) {
    join_keys_obj <- join_keys_obj$get()
    class(join_keys_obj) <- "Placeholder"
  }

  dataset_1 <- join_key_obj$dataset_1
  dataset_2 <- join_key_obj$dataset_2
  keys <- join_key_obj$keys

  join_keys_obj[dataset_1, dataset_2] <- keys
  join_keys_obj
}

#' Check the JoinKeys class membership of an argument
#' @inheritParams checkmate::assert_class
#' @param extra_classes (`character` vector) with extra classes to check. Can be used
#'
#' @return `x` invisibly
#'
#' @keywords internal
assert_join_keys <- function(x, .var.name = checkmate::vname(x)) {
  checkmate::assert_class(x, classes = c("Placeholder"), .var.name = .var.name)
}

#' Helper function to assert if two key sets contain incompatible keys
#'
#' return TRUE if compatible, throw error otherwise
#' @keywords internal
assert_compatible_keys <- function(join_key_1, join_key_2) {
  error_message <- function(dataset_1, dataset_2) {
    stop(
      paste("cannot specify multiple different join keys between datasets:", dataset_1, "and", dataset_2)
    )
  }

  # if first datasets and the second datasets match and keys
  # must contain the same named elements
  if (join_key_1$dataset_1 == join_key_2$dataset_1 && join_key_1$dataset_2 == join_key_2$dataset_2) {
    if (!identical(sort(join_key_1$keys), sort(join_key_2$keys))) {
      error_message(join_key_1$dataset_1, join_key_1$dataset_2)
    }
  }

  # if first dataset of join_key_1 matches second dataset of join_key_2
  # and the first dataset of join_key_2 must match second dataset of join_key_1
  # and keys must contain the same elements but with names and values swapped
  if (join_key_1$dataset_1 == join_key_2$dataset_2 && join_key_1$dataset_2 == join_key_2$dataset_1) {
    # have to handle empty case differently as names(character(0)) is NULL
    if (length(join_key_1$keys) == 0 && length(join_key_2$keys) == 0) {
      return(TRUE)
    }

    if (
      xor(length(join_key_1$keys) == 0, length(join_key_2$keys) == 0) ||
        !identical(sort(join_key_1$keys), sort(setNames(names(join_key_2$keys), join_key_2$keys)))
    ) {
      error_message(join_key_1$dataset_1, join_key_1$dataset_2)
    }
  }

  # otherwise they are compatible
  return(TRUE)
}

#' Helper function checks the parent-child relations are valid
#'
#' @param join_keys_obj (`JoinKeys`) object to assert validity of relations
#'
#' @return `join_keys_obj` invisibly
#'
assert_parent_child <- function(join_keys_obj) {
  jk <- join_keys(join_keys_obj)
  jk_parents <- parents(jk)

  assert_join_keys(jk)

  if (!is.null(jk_parents)) {
    for (idx1 in seq_along(jk_parents)) {
      name_from <- names(jk_parents)[[idx1]]
      for (idx2 in seq_along(jk_parents[[idx1]])) {
        name_to <- jk_parents[[idx1]][[idx2]]
        keys_from <- jk[name_from, name_to]
        keys_to <- jk[name_to, name_from]
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
  invisible(join_keys_obj)
}
