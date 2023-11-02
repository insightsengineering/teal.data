# Constructors ====

#' Create a `JoinKeys` out of a list of `JoinKeySet` objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details Note that join keys are symmetric although the relationship only needs
#' to be specified once.
#
#' @param ... optional, a `JoinKeySet` objects created using the `join_key` function.
#'
#' @return `JoinKeys`
#'
#' @export
#'
#' @examples
#' # setting join keys
#'
#' jk <- join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' jk
#'
#' # or
#' jk <- join_keys()
#' jk["dataset_A", "dataset_B"] <- c("col_1" = "col_a")
#' jk["dataset_A", "dataset_C"] <- c("col_2" = "col_x", "col_3" = "col_y")
#' jk
join_keys <- function(...) {
  x <- rlang::list2(...)

  # Getter
  if (checkmate::test_list(x, len = 1, types = c("JoinKeys"))) {
    return(x[[1]])
  } else if (checkmate::test_list(x, len = 1, types = c("teal_data"))) {
    return(x[[1]]@join_keys)
  } else if (checkmate::test_list(x, len = 1, types = c("TealData"))) {
    return(x[[1]]$get_join_keys())
  }

  # Constructor
  res <- new_join_keys()
  if (length(x) > 0) {
    join_keys(res) <- x
  }

  res
}

#' @details
#' The setter assignment `join_keys() <- ...` will only work for an empty
#' `JoinKey` object, otherwise `mutate_join_keys()` must be used.
#' @rdname join_keys
#'
#' @param join_keys_obj (`JoinKeys`) empty object to set the new relationship pairs.
#' @param value (`JoinKeySet` or list of `JoinKeySet`) relationship pairs to add
#' to `JoinKeys` list.
`join_keys<-` <- function(join_keys_obj, value) {
  UseMethod("join_keys<-", join_keys_obj)
}

#' @details
#' The setter assignment `join_keys() <- ...` will only work for an empty
#' `JoinKey` object, otherwise `mutate_join_keys()` must be used.
#'
#' @rdname join_keys
#' @export
#' @examples
#'
#' # Using the setter (assignment)
#'
#' jk <- join_keys()
#' join_keys(jk)
#' join_keys(jk) <- join_key("ds1", "ds2", "some_col")
#' join_keys(jk) <- join_key("ds3", "ds4", "some_col2")
#' join_keys(jk)["ds1", "ds3"] <- "some_col3"
`join_keys<-.JoinKeys` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }

  # Assume assignment of join keys as a merge operation
  #  Needed to support join_keys(jk)[ds1, ds2] <- "key"
  if (test_join_keys(value)) {
    return(merge_join_keys(join_keys_obj, value))
  }

  # Assignment of list of JoinKeySet will merge it with existing JoinKeys
  if (length(join_keys_obj) > 0 && checkmate::test_list(value, types = "JoinKeySet", min.len = 1)) {
    jk <- new_join_keys()
    join_keys(jk) <- value
    message("note: Keys already set, merging new list of JoinKeySet with existing keys.")
    return(merge_join_keys(join_keys_obj, jk))
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

#' @rdname join_keys
#' @export
#' @examples
#'
#' td <- teal_data()
#' join_keys(td)["ds1", "ds2"] <- "key1"
#' join_keys(td)["ds2", "ds2"] <- "key2"
#' join_keys(td)["ds3", "ds2"] <- "key3"
`join_keys<-.teal_data` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }

  if (test_join_keys(value)) {
    join_keys_obj@join_keys <- merge_join_keys(join_keys_obj@join_keys, value)
    return(join_keys_obj@join_keys)
  }

  join_keys_obj@join_keys$set(value)
  join_keys_obj
}

#' @details
#' Getter for JoinKeys that returns the relationship between pairs of datasets.
#'
#' @rdname join_keys
#'
#' @param join_keys_obj (`JoinKeys`) object to extract the join keys
#' @param dataset_1 (`character`) name of first dataset.
#' @param dataset_2 (`character`) name of second dataset.
#'
#' @export
#'
#' @examples
#'
#' # Getter for JoinKeys
#'
#' jk <- join_keys()
#' join_keys(jk) <- join_key("ds1", "ds2", "some_col")
#' jk["ds1", "ds2"]
#' jk["ds1"]
#' jk[["ds1"]]
`[.JoinKeys` <- function(join_keys_obj, dataset_1 = NULL, dataset_2 = NULL) {
  if (checkmate::test_integerish(dataset_1, len = 2)) {
    # if dataset_1 is an index integet vector, then return itself
    #  trick to make the following work: join_keys(jk)["ds1", "ds2"] <- "key"
    return(join_keys_obj)
  }
  checkmate::assert_string(dataset_1, null.ok = TRUE)
  if (missing(dataset_2)) {
    # protection if dataset_2 is passed through by a function
    dataset_2 <- NULL
  }
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  if (is.null(dataset_1) && is.null(dataset_2)) {
    return(join_keys_obj)
  }
  if (is.null(dataset_2)) {
    return(join_keys_obj[[dataset_1]])
  }
  if (is.null(dataset_1)) {
    return(join_keys_obj[[dataset_2]])
  }

  result <- join_keys_obj[[dataset_1]][[dataset_2]]
  if (is.null(result)) {
    return(character(0))
  }
  result
}

#' @details
#' Setter via index directly (bypassing the need to use `join_key()`).
#' When `dataset_2` is omitted, it will create a primary key with `dataset_2 = dataset_1`.
#'
#' @rdname join_keys
#'
#' @param value (`character` vector) value to assign.
#'
#' @export
#'
#' @examples
#'
#' # Setter via index
#'
#' jk <- join_keys()
#' join_keys(jk) <- join_key("ds1", "ds2", "(original) pair key")
#'
#' # overwrites previously defined key
#' jk["ds1", "ds2"] <- "(new) pair key"
#'
#' # Creates primary key by only defining `dataset_1`
#' jk["ds1"] <- "primary_key"
#' jk
`[<-.JoinKeys` <- function(join_keys_obj, dataset_1, dataset_2 = dataset_1, value) {
  join_keys_obj <- add_key(join_keys_obj, dataset_1, dataset_2, value)

  class(join_keys_obj) <- unique(c(class(join_keys_obj), "tmp_assignment"))

  join_keys_obj
}

# wrappers ====
#' Mutate `JoinKeys` with a new values
#'
#' @description `r lifecycle::badge("experimental")`
#' Mutate `JoinKeys` with a new values
#'
#' @param x (`JoinKeys`) object to be modified
#' @param dataset_1 (`character`) one dataset name
#' @param dataset_2 (`character`) other dataset name
#' @param value (named `character`) column names used to join
#'
#' @return modified `JoinKeys` object
#'
#' @export
mutate_join_keys <- function(x, dataset_1, dataset_2, value) {
  UseMethod("mutate_join_keys")
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' jk <- join_keys()
#' join_keys(jk) <- list(ds1 = list(ds2 = "some_col"))
#' mutate_join_keys(jk, "ds2", "ds3", "another")
mutate_join_keys.JoinKeys <- function(x, dataset_1, dataset_2, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  checkmate::assert_character(value, any.missing = FALSE)

  res <- join_pair(x, join_key(dataset_1, dataset_2, value))

  logger::log_trace(
    sprintf(
      "JoinKeys updated the keys between %s and %s to %s",
      dataset_1,
      dataset_2,
      paste(value, collapse = ", ")
    )
  )

  res
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' # teal_data ----
#'
#' ADSL <- teal.data::example_cdisc_data("ADSL")
#' ADRS <- teal.data::example_cdisc_data("ADRS")
#'
#' x <- cdisc_data(
#'   "ADSL" = ADSL,
#'   "ADRS" = ADRS
#' )
#' join_keys(x)["ADSL", "ADRS"]
#'
#' join_keys(x) <- mutate_join_keys(x, "ADSL", "ADRS", c("COLUMN1" = "COLUMN2"))
mutate_join_keys.teal_data <- function(x, dataset_1, dataset_2, value) { # nolint
  join_keys(x) <- mutate_join_keys(join_keys(x), dataset_1, dataset_2, value)
  join_keys(x)
}

#' @rdname split_join_keys
#' @export
split_join_keys <- function(join_keys_obj) {
  UseMethod("split_join_keys", join_keys_obj)
}

#' @rdname split_join_keys
#' @export
split_join_keys.default <- function(join_keys_obj) {
  split_join_keys(join_keys(join_keys_obj))
}

#' Split the `JoinKeys` object into a named list of join keys objects with an element for each dataset
#'
#' @param join_keys_obj (`JoinKeys`) base object to get the keys from.
#'
#' @return (`list`) a list of `JoinKeys` object
#'
#' @rdname split_join_keys
#'
#' @export
#'
#' @examples
#' jk <- join_keys()
#' jk["ds1", "ds2"] <- "some_col"
#' jk["ds1", "ds3"] <- "new_col"
#' split_join_keys(jk)
split_join_keys.JoinKeys <- function(join_keys_obj) {
  assert_join_keys(join_keys_obj)

  list_of_list_of_join_key_set <- lapply(
    names(join_keys_obj),
    function(dataset_1) {
      lapply(
        names(join_keys_obj[[dataset_1]]),
        function(dataset_2) join_key(dataset_1, dataset_2, get_join_key(join_keys_obj, dataset_1, dataset_2))
      )
    }
  )
  res <- lapply(list_of_list_of_join_key_set, function(.x) do.call(join_keys, .x))
  names(res) <- names(join_keys_obj)

  logger::log_trace("JoinKeys keys split.")
  return(res)
}

#' @rdname merge_join_keys
#' @export
merge_join_keys <- function(join_keys_obj, new_join_keys) {
  UseMethod("merge_join_keys", join_keys_obj)
}

#' @rdname merge_join_keys
#' @export
merge_join_keys.default <- function(join_keys_obj, new_join_keys) {
  merge_join_keys(join_keys(join_keys_obj), new_join_keys)
}

#' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
#'
#' @rdname merge_join_keys
#'
#' @param join_keys_obj (`JoinKeys`) object to merge the new_join_keys.
#' @param new_join_keys  `list` of `JoinKeys` objects or single `JoinKeys` object
#'
#' @return a new `JoinKeys` object with the resulting merge.
#'
#' @export
#'
#' @examples
#' jk1 <- join_keys()
#' jk1["ds1", "ds2"] <- "some_col"
#'
#' jk2 <- join_keys()
#' jk2["ds1", "ds3"] <- "new_col"
#'
#' merge_join_keys(jk1, jk2)
merge_join_keys.JoinKeys <- function(join_keys_obj, new_join_keys) {
  assert_join_keys(join_keys_obj)

  if (test_join_keys(new_join_keys)) {
    new_join_keys <- list(new_join_keys)
  }

  checkmate::assert_list(new_join_keys, types = c("JoinKeys"), min.len = 1)

  result <- join_keys_obj

  for (jk in new_join_keys) {
    for (dataset_1 in names(jk)) {
      for (dataset_2 in names(jk[[dataset_1]])) {
        result[[dataset_1]][[dataset_2]] <- jk[[dataset_1]][[dataset_2]]
      }
    }
  }
  logger::log_trace("JoinKeys keys merged.")
  return(result)
}

#' Prints `JoinKeys`.
#'
#' @inheritParams base::print
#' @return the `x` parameter
#'
#' @export
print.JoinKeys <- function(x, ...) {
  check_ellipsis(...)
  keys_list <- x
  my_parents <- parents(keys_list)
  class(keys_list) <- "list"
  if (length(keys_list) > 0) {
    cat(sprintf(
      "A JoinKeys object containing foreign keys between %s datasets:\n",
      length(keys_list)
    ))
    # Hide parents
    attr(keys_list, "__parents__") <- NULL
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
  class(result) <- c("JoinKeys", "list")
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
  jk <- join_keys(join_keys_obj)

  assert_join_keys(jk)

  if (missing(dataset_1) && missing(dataset_2)) {
    return(jk)
  }
  if (missing(dataset_2)) {
    return(jk[dataset_1])
  }
  if (missing(dataset_1)) {
    return(jk[dataset_2])
  }

  jk[dataset_1, dataset_2]
}

#' Internal assignment of value to a JoinKeys object
#'
#' @inheritParams join_keys
#'
#' @keywords internal
add_key <- function(join_keys_obj, dataset_1, dataset_2 = dataset_1, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  # Normalize value
  new_join_key <- join_key(dataset_1, dataset_2, value)
  dataset_1 <- new_join_key$dataset_1
  dataset_2 <- new_join_key$dataset_2
  value <- new_join_key$keys

  # Create pair ds_1 -> ds_2
  if (is.null(join_keys_obj[[dataset_1]])) join_keys_obj[[dataset_1]] <- list()

  join_keys_obj[[dataset_1]][[dataset_2]] <- value

  # Primary key, do nothing else
  if (identical(dataset_1, dataset_2)) {
    return(join_keys_obj)
  }

  # Create symmetrical pair ds_2 -> ds_1
  if (is.null(join_keys_obj[[dataset_2]])) join_keys_obj[[dataset_2]] <- list()

  if (
    checkmate::test_character(value, min.len = 1) &&
      all(is.null(names(value)))
  ) {
    value <- setNames(names(value), value)
  } else if (
    checkmate::test_character(value, min.len = 1)
  ) {
    # Invert key
    value <- setNames(names(value), value)
  }

  join_keys_obj[[dataset_2]][[dataset_1]] <- value
  join_keys_obj
}

#' Helper function to add a new pair to a `JoinKeys` object
#'
#' @param join_keys_obj (`JoinKeys`) Object with existing pairs.
#' @param join_key_obj (`JoinKeySet`) relationship pair to add.
#'
#' @examples
#' jk <- join_keys()
#' jk <- join_pair(jk, join_key("ds1", "ds2", "value"))
#' jk <- join_pair(jk, join_key("ds3", "ds2", "value"))
#' jk
join_pair <- function(join_keys_obj, join_key_obj) {
  assert_join_keys(join_keys_obj)
  checkmate::assert_class(join_key_obj, "JoinKeySet")

  dataset_1 <- join_key_obj$dataset_1
  dataset_2 <- join_key_obj$dataset_2
  keys <- join_key_obj$keys

  join_keys_obj <- add_key(join_keys_obj, dataset_1, dataset_2, keys)
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
  checkmate::assert_class(x, classes = c("JoinKeys"), .var.name = .var.name)
}

#' @rdname assert_join_keys
#' @keywords internal
test_join_keys <- function(x) {
  checkmate::test_class(x, classes = c("JoinKeys"))
}

#' @rdname assert_join_keys
#' @keywords internal
expect_join_keys <- function(x) {
  checkmate::expect_class(x, classes = c("JoinKeys"))
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
