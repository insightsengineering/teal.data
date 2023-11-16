# Constructors ====

#' Create a `join_keys` out of a list of `join_key_set` objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details - `join_keys()`: When called without arguments it will return an
#' empty constructor.
#' - `join_keys(x)`: When called with a single argument it will return the `join_keys`
#' object contained in `x` (if it contains a `join_keys` object).
#' - `join_keys(...)`: When called with a single or more `join_key_set` parameters it will
#' create a new object.
#'
#' Note that join keys are created symmetrically, that is, if `dat1` and `dat2`
#' have a join key of `col1`, then 2 join keys are created, `dat1 → dat2` and
#' `dat2 → dat1`. The only exception is for a primary key.
#'
#' @param ... (optional), when no argument is given the empty constructor is called.
#' Otherwise, when called with only one argument of type: `join_keys` or  `teal_data`
#' it will return the `join_keys` of that object.
#' When called with 1 or more `join_key_set` it will create a new `join_keys` with
#' constructed from the arguments.
#'
#' @return `join_keys` object.
#'
#' @export
#'
#' @examples
#' # Setting join keys ----
#'
#' jk <- join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' jk
#'
#' # or
#' jk <- join_keys()
#' jk[["dataset_A"]][["dataset_B"]] <- c("col_1" = "col_a")
#' jk[["dataset_A"]][["dataset_C"]] <- c("col_2" = "col_x", "col_3" = "col_y")
#' jk
#'
#' td <- teal_data(join_keys = join_keys(join_key("a", "b", "c")))
#' join_keys(td)
#'
#' jk <- join_keys()
#' join_keys(jk)
#'
#' jk <- join_keys()
#' jk <- c(jk, join_keys(join_key("a", "b", "c")))
#' jk <- c(jk, join_keys(join_key("a", "b", "c"), join_key("a", "b2", "c")))
join_keys <- function(...) {
  if (missing(...)) {
    return(new_join_keys())
  }
  x <- rlang::list2(...)
  if (length(x) == 1L) {
    UseMethod("join_keys", x[[1]])
  } else {
    join_keys.default(...)
  }
}

#' @rdname join_keys
#' @export
join_keys.join_keys <- function(...) {
  x <- rlang::list2(...)
  x[[1]]
}

#' @rdname join_keys
#' @export
join_keys.teal_data <- function(...) {
  x <- rlang::list2(...)
  x[[1]]@join_keys
}

#' @rdname join_keys
#' @export
join_keys.TealData <- function(...) {
  x <- rlang::list2(...)
  x[[1]]$get_join_keys()
}

#' @rdname join_keys
#' @export
join_keys.default <- function(...) {
  c(new_join_keys(), ...)
}

#' @rdname join_keys
#' @details
#' The setter assignment `join_keys(obj) <- ...` will merge obj and `...` if obj
#' is not empty.
#'
#' @param x (`join_keys`) empty object to set the new relationship pairs.
#' @param value (`join_key_set` or list of `join_key_set`) relationship pairs to add
#' to `join_keys` list.
#'
#' @export
`join_keys<-` <- function(x, value) {
  checkmate::assert_class(value, classes = c("join_keys", "list"))
  UseMethod("join_keys<-", x)
}

#' @rdname join_keys
#' @export
#' @examples
#'
#' # Using the setter (assignment) ----
#'
#' jk <- join_keys()
#' join_keys(jk) <- join_keys(join_keys(jk), join_key("ds3", "ds4", "some_col2"))
#'
#' join_keys(jk)[["ds1"]][["ds3"]] <- "some_col3"
#' jk
`join_keys<-.join_keys` <- function(x, value) {
  value
}

#' @rdname join_keys
#' @export
#' @examples
#'
#' # Setter for join_keys within teal_data ----
#'
#' td <- teal_data()
#' join_keys(td)[["ds1"]][["ds2"]] <- "key1"
#' join_keys(td)[["ds2"]][["ds2"]] <- "key2"
#' join_keys(td) <- c(join_keys(td), join_keys(join_key("ds3", "ds2", "key3")))
#' join_keys(td)
`join_keys<-.teal_data` <- function(x, value) {
  join_keys(x@join_keys) <- value
  x
}

#' @rdname join_keys
#' @export
#'
#' @examples
#'
#' c(join_keys(join_key("a", "b", "c")), join_keys(join_key("a", "d2", "c")))
c.join_keys <- function(...) {
  join_keys_obj <- rlang::list2(...)[[1]]
  x <- rlang::list2(...)[-1]
  checkmate::assert_multi_class(join_keys_obj, classes = c("join_keys", "join_key_set"))
  checkmate::assert_list(x, types = c("join_keys", "join_key_set"))

  x_merged <- Reduce(
    init = join_keys(),
    x = x,
    f = function(.x, .y) {
      assert_compatible_keys2(.x, .y)
      utils::modifyList(.x, .y, keep.null = FALSE)
    }
  )

  utils::modifyList(join_keys_obj, x_merged, keep.null = FALSE)
}

#' @rdname join_keys
#' @export
#'
#' @examples
#'
#' c(join_key("a", "b", "c"), join_keys(join_key("a", "d2", "c")))
c.join_key_set <- function(...) {
  c.join_keys(...)
}

#' The Names of an `join_keys` Object
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
      parent_list <- lapply(parent_list, function(.x) {
        if (identical(.x, old_name)) {
          new_name
        } else {
          .x
        }
      })
      attr(new_x, "__parents__") <- parent_list
    }
  }

  class(new_x) <- c("join_keys", "list")
  new_x
}

#' @rdname join_keys
#' @details
#' Getter for `join_keys` that returns the relationship between pairs of datasets.
#'
#' @inheritParams base::`[[`
#' @param keep_all_foreign_keys (`logical`) flag that keeps foreign keys and other
#' datasets even if they are not a parent of the selected dataset.
#'
#' @export
#'
#' @examples
#'
#' # Getter for join_keys ----
#'
#' jk <- join_keys()
#' jk[["ds1"]][["ds2"]] <- "some_col"
#' jk[["ds1"]][["ds3"]] <- "some_col2"
#'
#' jk["ds1"]
#' jk[1:2]
#' jk[c("ds1", "ds2")]
`[.join_keys` <- function(x, i, keep_all_foreign_keys = FALSE) {
  if (missing(i)) {
    return(x)
  }

  if (is.null(i)) {
    return(new_join_keys()) # replicate base R
  }

  checkmate::assert(
    combine = "or",
    checkmate::check_integerish(i),
    checkmate::check_logical(i),
    checkmate::check_character(i)
  )
  checkmate::assert_logical(keep_all_foreign_keys, len = 1)

  # Convert integer/logical index to named index
  if (checkmate::test_integerish(i) || checkmate::test_logical(i)) {
    i <- names(x)[i]
  }

  # When retrieving a relationship pair, it will also return the symmetric key
  new_jk <- new_join_keys()
  queue <- i
  bin <- character(0)

  # Need to iterate on a mutating queue if subset of a dataset will also
  #  select its parent as that parent might have relationships with others
  #  already selected.
  while (length(queue) > 0) {
    ix <- queue[1]
    queue <- queue[-1]

    if (ix %in% bin) {
      next
    }
    bin <- c(bin, ix)

    ix_parent <- parent(x, ix)

    if (checkmate::test_string(ix_parent, min.chars = 1) && !ix_parent %in% c(queue, bin)) {
      queue <- c(queue, ix_parent)
    }

    ix_valid_names <- names(x[[ix]]) %in% c(queue, bin)
    if (keep_all_foreign_keys) {
      ix_valid_names <- rep(TRUE, length(names(x[[ix]])))
    }

    new_jk[[ix]] <- x[[ix]][ix_valid_names]

    # Add primary key of parent
    if (length(ix_parent) > 0) {
      new_jk[[ix_parent]][[ix_parent]] <- x[[ix_parent]][[ix_parent]]
    }
  }

  common_parents_ix <- names(parents(x)) %in% names(new_jk) &
    parents(x) %in% names(new_jk)

  if (any(common_parents_ix)) parents(new_jk) <- parents(x)[common_parents_ix]

  new_jk
}

#' @rdname join_keys
#' @details
#' Setter via index directly (bypassing the need to use `join_key()`).
#' When `dataset_2` is omitted, it will create a primary key with `dataset_2 = dataset_1`.
#'
#' @inheritParams base::`[<-`
#'
#' @export
`[<-.join_keys` <- function(x, i, value) {
  stop("Can't use `[<-` for object `join_keys`. Use [[<- instead.")
}

#' @rdname join_keys
#' @export
#' @examples
#'
#' jk <- join_keys()
#' jk[["ds1"]] <- list()
#' jk[["ds2"]][["ds3"]] <- "key"
#'
#' jk <- join_keys()
#' jk[["ds1"]] <- list()
#' jk[["ds2"]][["ds3"]] <- "key"
#' jk[["ds4"]] <- list(ds5 = "new")
#'
#' jk <- join_keys()
#' jk[["ds2"]][["ds3"]] <- "key"
#' jk[["ds2"]][["ds3"]] <- NULL
#'
#' jk
`[[<-.join_keys` <- function(x, i, value) {
  checkmate::assert(
    combine = "or",
    checkmate::check_string(i),
    checkmate::check_integerish(i, len = 1),
    checkmate::check_logical(i, len = 1)
  )
  checkmate::assert_list(value, names = "named", types = "character", null.ok = TRUE)
  if (checkmate::test_integerish(i) || checkmate::test_logical(i)) {
    i <- names(x)[[i]]
  }

  # Normalize values
  norm_value <- lapply(seq_along(value), function(.x) {
    join_key(i, names(value)[.x], value[[.x]])
  })
  names(norm_value) <- names(value)

  # Check if multiple modifications don't have a conflict
  repeated_value_ix <- names(value) %in% names(value)[duplicated(names(value))]
  repeated <- norm_value[repeated_value_ix]
  vapply(
    seq_along(repeated),
    function(.ix, .x_value = repeated[[.ix]], .x_name = names(.x_value[[1]])) {
      assert_compatible_keys2(
        .x_value,
        unlist(unname(
          repeated[-.ix][names(repeated[-.ix]) == .x_name]
        ), recursive = FALSE)
      )
    },
    logical(1)
  )

  norm_value <- lapply(norm_value, function(x) x[[1]][[1]])
  names(norm_value) <- names(value)

  # Safe to do as duplicated are the same
  norm_value[duplicated(names(norm_value))] <- NULL

  # Remove elements with length == 0L
  norm_value <- Filter(function(.x) length(.x) > 0, norm_value)

  # Remove classes to use list-based get/assign operations
  new_x <- unclass(x)

  # In case a pair is removed, also remove the symmetric pair
  removed_names <- setdiff(names(new_x[[i]]), names(norm_value))
  if (length(removed_names) > 0) {
    for (.x in removed_names) new_x[[.x]][[i]] <- NULL
  }

  new_x[[i]] <- norm_value

  # Iterate on all new values to create symmetrical pair
  for (ds2 in names(norm_value)) {
    if (ds2 == i) next

    keep_value <- new_x[[ds2]] %||% list()
    new_value <- norm_value[[ds2]]

    if (checkmate::test_character(new_value, min.len = 1, names = "unnamed")) {
      new_value <- setNames(new_value, new_value)
    } else if (checkmate::test_character(new_value, min.len = 1)) {
      # Invert key
      new_value <- setNames(names(new_value), new_value)
    }

    keep_value[[i]] <- new_value

    # Assign symmetrical
    new_x[[ds2]] <- keep_value
  }

  # Remove NULL or empty keys
  empty_ix <- vapply(
    new_x,
    function(.x) is.null(.x) || length(.x) == 0,
    logical(1)
  )
  preserve_attr <- attributes(new_x)[!names(attributes(new_x)) %in% "names"]
  new_x <- new_x[!empty_ix]
  attributes(new_x) <- utils::modifyList(attributes(new_x), preserve_attr)

  #
  # restore class
  class(new_x) <- class(x)
  new_x
}

#' Length of `join_keys` object.
#' @inheritParams base::length
#' @export
length.join_keys <- function(x) {
  if (NextMethod("length", x) == 0) {
    return(0)
  }
  sum(vapply(x, function(.x) length(.x) > 0, logical(1)))
}


#' @rdname join_keys
#' @export
format.join_keys <- function(x, ...) {
  check_ellipsis(...)
  if (length(x) > 0) {
    my_parents <- parents(x)
    names_sorted <- topological_sort(my_parents)
    names <- union(names_sorted, names(x))

    out <- lapply(names, function(i) {
      this_parent <- my_parents[[i]]
      out_i <- lapply(union(i, names(x[[i]])), function(j) {
        direction <- if (identical(my_parents[[j]], i)) {
          "  <-- "
        } else if (identical(my_parents[[i]], j)) {
          "  --> "
        } else if (!identical(i, j)) {
          "  <-> "
        } else {
          ""
        }

        keys <- x[[i]][[j]]
        sprintf(
          "%s%s: [%s]",
          direction, j,
          if (length(keys) == 0) "no primary keys" else toString(keys)
        )
      })
      paste(out_i, collapse = "\n")
    })
    paste(
      c(
        sprintf("A join_keys object containing foreign keys between %s datasets:", length(x)),
        out
      ),
      collapse = "\n"
    )
  } else {
    "An empty join_keys object."
  }
}

#' @rdname join_keys
#' @export
print.join_keys <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

# -----------------------------------------------------------------------------
#
#
# Helpers (non-exported)
#

#' Internal constructor
#'
#' @return an empty `join_keys` list
#'
#' @keywords internal
new_join_keys <- function() {
  structure(
    list(),
    class = c("join_keys", "list")
  )
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
        !identical(sort(keys_one), sort(setNames(names(keys_two), keys_two)))
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
