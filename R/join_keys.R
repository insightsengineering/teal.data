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
  x <- rlang::list2(...)
  # Constructor
  res <- new_join_keys()
  if (length(x) > 0) {
    join_keys(res) <- x
  }

  res
}

#' @rdname join_keys
#' @details
#' The setter assignment `join_keys(obj) <- ...` will merge obj and `...` if obj
#' is not empty.
#'
#' @param join_keys_obj (`join_keys`) empty object to set the new relationship pairs.
#' @param value (`join_key_set` or list of `join_key_set`) relationship pairs to add
#' to `join_keys` list.
#'
#' @export
`join_keys<-` <- function(join_keys_obj, value) {
  UseMethod("join_keys<-", join_keys_obj)
}

#' @rdname join_keys
#' @export
#' @examples
#'
#' # Using the setter (assignment) ----
#'
#' jk <- join_keys()
#' join_keys(jk) <- join_key("ds1", "ds2", "some_col")
#' join_keys(jk) <- c(join_keys(jk), join_key("ds3", "ds4", "some_col2"))
#'
#' join_keys(jk)[["ds1"]][["ds3"]] <- "some_col3"
#' jk
`join_keys<-.join_keys` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }

  # Assume assignment of join keys as a merge operation
  #  Needed to support join_keys(jk)[ds1, ds2] <- "key"
  if (checkmate::test_class(value, classes = c("join_keys", "list"))) {
    return(value)
  }

  if (inherits(value, "join_key_set")) value <- list(value)

  checkmate::assert_list(value, types = "join_key_set", min.len = 1)

  join_keys_obj <- new_join_keys()

  # check if any join_key_sets share the same datasets but different values
  for (idx_1 in seq_along(value)) {
    for (idx_2 in seq_along(value[idx_1])) {
      assert_compatible_keys(value[[idx_1]], value[[idx_2]])
    }
    dataset_1 <- get_dataset_1(value[[idx_1]])
    dataset_2 <- get_dataset_2(value[[idx_1]])
    keys <- get_keys(value[[idx_1]])

    join_keys_obj[[dataset_1]][[dataset_2]] <- keys
  }

  logger::log_trace("join_keys keys are set.")

  join_keys_obj
}

#' @rdname join_keys
#' @export
#'
#' @examples
#'
#' c(join_keys(join_key("a", "b", "c")), join_keys(join_key("a", "d2", "c")))
c.join_keys <- function(...) {
  x <- rlang::list2(...)

  if (!length(x)) {
    return(NULL)
  }
  checkmate::assert_list(x[-1], types = c("join_keys", "join_key_set"))

  merge_join_keys.join_keys(x[[1]], x[-1])
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
`join_keys<-.teal_data` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }

  join_keys(join_keys_obj@join_keys) <- value
  join_keys_obj
}

#' The Names of an `join_keys` Object
#' @inheritParams base::`names<-`
#' @export
`names<-.join_keys` <- function(x, value) {
  x <- unclass(x)
  # Update inner keys
  for (old_name in setdiff(names(x), value)) {
    old_entry <- x[[old_name]]
    new_name <- value[names(x) == old_name]

    # Change 2nd-tier first
    for (sub_name in names(old_entry)) {
      names(x[[sub_name]])[names(x[[sub_name]]) == old_name] <- new_name
    }

    # Change in first tier
    names(x)[names(x) == old_name] <- new_name
  }
  class(x) <- c("join_keys", "list")
  x
}

#' @rdname join_keys
#' @details
#' Getter for `join_keys` that returns the relationship between pairs of datasets.
#'
#' @param join_keys_obj (`join_keys`) object to extract the join keys
#' @param dataset_1 (`character`) name of first dataset.
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
`[.join_keys` <- function(join_keys_obj, dataset_1 = NULL, keep_all_foreign_keys = FALSE) {
  # Protection against missing being passed through functions
  if (missing(dataset_1)) dataset_1 <- NULL

  if (is.null(dataset_1)) {
    return(join_keys_obj)
  }

  if (checkmate::test_integerish(dataset_1) || checkmate::test_logical(dataset_1)) {
    dataset_1 <- names(join_keys_obj)[dataset_1]
  }

  # When retrieving a relationship pair, it will also return the symmetric key
  new_jk <- new_join_keys()
  queue <- dataset_1
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

    ix_parent <- parent(join_keys_obj, ix)

    if (checkmate::test_string(ix_parent, min.chars = 1) && !ix_parent %in% c(queue, bin)) {
      queue <- c(queue, ix_parent)
    }

    ix_valid_names <- names(join_keys_obj[[ix]]) %in% c(queue, bin)
    if (keep_all_foreign_keys) {
      ix_valid_names <- rep(TRUE, length(names(join_keys_obj[[ix]])))
    }

    new_jk[[ix]] <- join_keys_obj[[ix]][ix_valid_names]

    # Add primary key of parent
    if (length(ix_parent) > 0) {
      new_jk[[ix_parent]][[ix_parent]] <- join_keys_obj[[ix_parent]][[ix_parent]]
    }
  }

  common_parents_ix <- names(parents(join_keys_obj)) %in% names(new_jk) &
    parents(join_keys_obj) %in% names(new_jk)

  if (any(common_parents_ix)) parents(new_jk) <- parents(join_keys_obj)[common_parents_ix]

  new_jk
}

#' @rdname join_keys
#' @details
#' Setter via index directly (bypassing the need to use `join_key()`).
#' When `dataset_2` is omitted, it will create a primary key with `dataset_2 = dataset_1`.
#'
#' @param value (`character` vector) value to assign.
#'
#' @export
#'
#' @examples
#'
#' # Setter via index ----
#'
#' jk <- join_keys(
#'   join_key("ds1", "ds2", "col12"),
#'   join_key("ds3", "ds4", "col34")
#' )
#'
#' # overwrites previously defined key
#' jk["ds1"] <- list(ds2 = "(new)co12")
#' jk["ds1"] <- list(ds3 = "col13", ds4 = "col14")
#' jk
#'
#' jk[c("ds1", "ds2")] <- list(ds5 = "col*5")
#' jk[c(1, 2)] <- list(ds5 = "col**5")
#'
#' # Creates primary key by only defining `dataset_1`
#' jk["ds1"] <- "primary_key"
#' jk
`[<-.join_keys` <- function(join_keys_obj, dataset_1, value) {
  checkmate::assert(
    combine = "or",
    checkmate::check_character(dataset_1),
    checkmate::check_integerish(dataset_1)
  )

  if (checkmate::test_integerish(dataset_1)) {
    dataset_1 <- names(join_keys_obj)[dataset_1]
  }

  checkmate::assert(
    combine = "or",
    checkmate::check_character(value),
    checkmate::check_list(value, names = "named", types = "character", null.ok = TRUE)
  )

  # Assume characters as being primary keys
  if (checkmate::test_character(value)) {
    value <- lapply(dataset_1, function(dataset_ix) {
      value
    })
    names(value) <- dataset_1
  }

  original_value <- value
  for (dataset_ix in dataset_1) {
    if (is.null(value)) {
      inner_items <- names(join_keys_obj[[dataset_ix]])
      value <- structure(
        vector(mode = "list", length = length(inner_items)),
        names = inner_items
      )
    }

    for (new_ix in names(value)) {
      join_keys_obj[[dataset_ix]][[new_ix]] <- value[[new_ix]]
    }
    value <- original_value
  }

  join_keys_obj
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
#' jk
`[[<-.join_keys` <- function(join_keys_obj, dataset_1, value) {
  if (checkmate::test_integerish(dataset_1) || checkmate::test_logical(dataset_1)) {
    dataset_1 <- names(join_keys_obj)[[dataset_1]]
  }

  checkmate::assert_string(dataset_1)

  # Accepting 1 subscript with valid `value` formal
  checkmate::assert_list(value, names = "named", types = "character", null.ok = TRUE)

  # Normalize values
  norm_value <- lapply(names(value), function(.x) {
    get_keys(join_key(dataset_1, .x, value[[.x]]))
  })

  names(norm_value) <- names(value)
  value <- norm_value

  #
  # Remove classes to use list-based get/assign operations
  join_keys_obj <- unclass(join_keys_obj)

  # In case a pair is removed, also remove the symmetric pair
  removed_names <- setdiff(names(join_keys_obj[[dataset_1]]), names(value))
  if (length(removed_names) > 0) {
    for (.x in removed_names) join_keys_obj[[.x]][[dataset_1]] <- NULL
  }

  join_keys_obj[[dataset_1]] <- value

  # Iterate on all new values to create symmetrical pair
  for (ds2 in names(value)) {
    if (ds2 == dataset_1) next

    keep_value <- join_keys_obj[[ds2]] %||% list()
    new_value <- value[[ds2]]

    if (checkmate::test_character(new_value, min.len = 1, names = "unnamed")) {
      new_value <- setNames(new_value, new_value)
    } else if (
      checkmate::test_character(new_value, min.len = 1)
    ) {
      # Invert key
      new_value <- setNames(names(new_value), new_value)
    }

    keep_value[[dataset_1]] <- new_value

    # Assign symmetrical
    join_keys_obj[[ds2]] <- keep_value
  }

  # Remove NULL or empty keys
  empty_ix <- vapply(
    join_keys_obj,
    function(.x) is.null(.x) || length(.x) == 0,
    logical(1)
  )
  preserve_attr <- attributes(join_keys_obj)[!names(attributes(join_keys_obj)) %in% "names"]
  join_keys_obj <- join_keys_obj[!empty_ix]
  attributes(join_keys_obj) <- modifyList(attributes(join_keys_obj), preserve_attr)

  #
  # restore class
  class(join_keys_obj) <- c("join_keys", "list")
  join_keys_obj
}

#' @rdname merge_join_keys
#' @keywords internal
merge_join_keys <- function(join_keys_obj, new_join_keys) {
  UseMethod("merge_join_keys", join_keys_obj)
}

#' @rdname merge_join_keys
#' @keywords internal
merge_join_keys.default <- function(join_keys_obj, new_join_keys) {
  merge_join_keys(join_keys(join_keys_obj), new_join_keys)
}

#' Merging a list (or one) of `join_keys` objects into the current `join_keys` object
#'
#' @rdname merge_join_keys
#'
#' @param join_keys_obj (`join_keys`) object to merge the new_join_keys.
#' @param new_join_keys  `list` of `join_keys` objects or single `join_keys` object
#'
#' @return a new `join_keys` object with the resulting merge.
#'
#' @keywords internal
merge_join_keys.join_keys <- function(join_keys_obj, new_join_keys) {
  if (
    checkmate::test_class(new_join_keys, "join_key_set") ||
      checkmate::test_class(new_join_keys, "join_keys")
  ) {
    new_join_keys <- list(new_join_keys)
  }

  lapply(new_join_keys, assert_join_keys_alike)

  if (checkmate::test_list(new_join_keys, types = "join_key_set")) {
    jk_temp <- new_join_keys()
    join_keys(jk_temp) <- new_join_keys
    new_join_keys <- list(jk_temp)
  }

  checkmate::assert_list(new_join_keys, types = c("join_keys"), min.len = 1)

  for (el in new_join_keys) {
    join_keys_obj <- utils::modifyList(join_keys_obj, el)
  }

  logger::log_trace("join_keys keys merged.")
  return(join_keys_obj)
}

# S3 methods have to be exported, otherwise `.S3method` needs to be used
.S3method("merge_join_keys", "teal_data", merge_join_keys.default)
.S3method("merge_join_keys", "join_keys", merge_join_keys.join_keys)

#' Length of `join_keys` object.
#' @inheritParams base::length
#' @export
length.join_keys <- function(x) {
  if (NextMethod("length", x) == 0) {
    return(0)
  }
  sum(vapply(x, function(.x) !is.null(.x) && length(.x) > 0, logical(1)))
}

#' Prints `join_keys`.
#'
#' @inheritParams base::print
#' @return the `x` parameter
#'
#' @export
print.join_keys <- function(x, ...) {
  check_ellipsis(...)
  keys_list <- x
  my_parents <- parents(keys_list)
  class(keys_list) <- "list"
  if (length(keys_list) > 0) {
    cat(sprintf(
      "A join_keys object containing foreign keys between %s datasets:\n",
      length(x)
    ))
    # Hide parents
    attr(keys_list, "__parents__") <- NULL # nolint: object_name_linter
    non_empty_ix <- vapply(keys_list, function(.x) !is.null(.x) && length(.x) > 0, logical(1))
    print.default(keys_list[sort(names(keys_list))][non_empty_ix])
  } else {
    cat("An empty join_keys object.")
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
#' @return an empty `join_keys` list
#'
#' @keywords internal
new_join_keys <- function() {
  structure(
    list(),
    class = c("join_keys", "list")
  )
}

#' Assert the `join_keys` class membership of an argument
#' @inheritParams checkmate::assert_class
#'
#' @return `x` invisibly
#'
#' @keywords internal
assert_join_keys_alike <- function(x, .var.name = checkmate::vname(x), add = NULL) { # nolint: object_name_linter
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res <- check_join_keys_alike(x)

  checkmate::makeAssertion(x, res, var.name = .var.name, add)
}

#' @rdname assert_join_keys_alike
check_join_keys_alike <- function(x) {
  result <- checkmate::check_list(x, names = "named", types = "list")
  if (checkmate::test_string(result)) {
    return(result)
  }
  result <- all(
    vapply(
      x,
      function(el) {
        checkmate::test_list(el, types = "character", names = "named")
      },
      logical(1)
    )
  )
  if (isFALSE(all(result))) {
    return(
      paste(
        "Elements of list may only be named lists with a vector of  type `character`",
        "(that may be named or partially named)"
      )
    )
  }
  result
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

  dataset_1_one <- get_dataset_1(join_key_1)
  dataset_2_one <- get_dataset_2(join_key_1)
  keys_one <- get_keys(join_key_1)

  dataset_1_two <- get_dataset_1(join_key_2)
  dataset_2_two <- get_dataset_2(join_key_2)
  keys_two <- get_keys(join_key_2)


  # if first datasets and the second datasets match and keys
  # must contain the same named elements
  if (dataset_1_one == dataset_1_two && dataset_2_one == dataset_2_two) {
    if (!identical(sort(keys_one), sort(keys_two))) {
      error_message(dataset_1_one, dataset_2_one)
    }
  }

  # if first dataset of join_key_1 matches second dataset of join_key_2
  # and the first dataset of join_key_2 must match second dataset of join_key_1
  # and keys must contain the same elements but with names and values swapped
  if (dataset_1_one == dataset_2_two && dataset_2_one == dataset_1_two) {
    # have to handle empty case differently as names(character(0)) is NULL
    if (length(keys_one) == 0 && length(keys_two) == 0) {
      return(TRUE)
    }

    if (
      xor(length(keys_one) == 0, length(keys_two) == 0) ||
        !identical(sort(keys_one), sort(setNames(names(keys_two), keys_two)))
    ) {
      error_message(dataset_1_one, dataset_2_one)
    }
  }

  # otherwise they are compatible
  return(TRUE)
}

#' Helper function checks the parent-child relations are valid
#'
#' @param join_keys_obj (`join_keys`) object to assert validity of relations
#'
#' @return `join_keys_obj` invisibly
#'
#' @keywords internal
assert_parent_child <- function(join_keys_obj) {
  jk <- join_keys(join_keys_obj)
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
