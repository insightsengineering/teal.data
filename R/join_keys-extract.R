#' @rdname join_keys
#'
#' @details
#' - `[.join_keys` can be used to return a subset of relationship pairs. It will
#' retrieve the primary keys of the selected elements and its parents (along with)
#' the relationship keys between the selected elements and their parents.
#'
#' @param i index specifying elements to extract or replace. Index should be a
#' a character vector, but it can also take numeric, logical, `NULL` or missing.
#'
#' @param j index specifying elements to extract or replace. Index should be a
#' a character vector, but it can also take numeric, logical, `NULL` or missing.
#'
#' @export
#'
#' @examples
#'
#' # Getter for join_keys ----
#'
#' jk <- join_keys(
#'   join_key("ds1", "ds1", "primary-key-1"),
#'   join_key("ds2", "ds2", "primary-key-2"),
#'   join_key("ds3", "ds3", "primary-key-3"),
#'   join_key("ds2", "ds1", "foreign-key-2-1"),
#'   join_key("ds3", "ds1", "foregin-key-3-1")
#' )
#'
#' jk["ds1"]
#' jk[1:2]
#' jk[c("ds1", "ds2")]
#' jk["ds1", "ds2"]
`[.join_keys` <- function(x, i, j) {
  if (missing(i) && missing(j)) {
    # because:
    # - list(a = 1)[] returns list(a = 1)
    # - data.frame(a = 1)[] returns data.frame(a = 1)
    return(x)
  } else if (!missing(i) && is.null(i) || !missing(j) && is.null(j)) {
    # because list(a = 1)[NULL] returns NULL
    # data.frame(a = 1)[NULL, NULL] returns data.frame(
    return(join_keys())
  } else if (!missing(i) && !missing(j)) {
    if (
      !any(
        checkmate::test_string(i),
        checkmate::test_integerish(i, len = 1),
        checkmate::test_logical(i, len = length(x)) && sum(j) == 1
      ) ||
        !any(
          checkmate::test_string(j),
          checkmate::test_integerish(j, len = 1),
          checkmate::test_logical(j, len = length(x)) && sum(j) == 1
        )
    ) {
      stop(
        "join_keys[i, j] - Can't extract keys for multiple pairs.",
        "When specifying a pair [i, j], both indices must point to a single key pair.\n",
        call. = FALSE
      )
    }

    subset_x <- update_keys_given_parents(x[union(i, j)])
    return(subset_x[[i]][[j]])
  } else if (!missing(j)) {
    # ie. select all keys which have j as dataset_2
    # since list is symmetrical it is equivalent to selecting by i
    i <- j
  }

  checkmate::assert(
    combine = "or",
    checkmate::check_character(i),
    checkmate::check_integerish(i),
    checkmate::check_logical(i)
  )


  # Convert integer/logical index to named index
  if (checkmate::test_integerish(i) || checkmate::test_logical(i)) {
    i <- names(x)[i]
  }

  # When retrieving a relationship pair, it will also return the symmetric key
  new_jk <- new_join_keys()
  queue <- unique(i)
  bin <- character(0)

  # Need to iterate on a mutating queue if subset of a dataset will also
  #  select its parent as that parent might have relationships with others
  #  already selected.
  while (length(queue) > 0) {
    ix <- queue[1]
    queue <- queue[-1]
    bin <- c(bin, ix)

    ix_parent <- parent(x, ix)

    if (checkmate::test_string(ix_parent, min.chars = 1) && !ix_parent %in% c(queue, bin)) {
      queue <- c(queue, ix_parent)
    }

    ix_valid_names <- names(x[[ix]]) %in% c(queue, bin)

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
#'
#' @details
#' - `[<-` is not a supported operation for `join_keys`.
#'
#' @export
`[<-.join_keys` <- function(x, i, j, value) {
  if (missing(i) || missing(j)) {
    stop("join_keys[i, j] specify both indices to set a key pair.")
  } else if (!missing(i) && is.null(i) || !missing(j) && is.null(j)) {
    stop("join_keys[i, j] neither i nor j can be NULL.")
  } else if (
    !any(
      checkmate::test_string(i),
      checkmate::test_integerish(i, len = 1),
      checkmate::test_logical(i, len = length(x)) && sum(j) == 1
    ) ||
      !any(
        checkmate::test_string(j),
        checkmate::test_integerish(j, len = 1),
        checkmate::test_logical(j, len = length(x)) && sum(j) == 1
      )
  ) {
    stop(
      "join_keys[i, j] <- Can't set keys to specified indices.\n",
      "When setting pair [i, j], both indices must point to a single key pair.\n",
      call. = FALSE
    )
  }

  x[[i]][[j]] <- value
  x
}

#' @rdname join_keys
#'
#' @order 3
#'
#' @details
#' - `[[<-` is the preferred method to replace or assign new relationship pair to an
#' existing `join_keys` object.
#' - `join_keys(obj)[[dataset_1]] <- value` can also be used to assign a relationship
#' pair to an `obj` that contains a `join_keys`, such as itself or a `teal_data`
#' object.
#'
#' @export
#' @examples
#'
#' jk <- join_keys()
#' jk[["dataset_A"]][["dataset_B"]] <- "key"
#' jk[["dataset_C"]] <- list(dataset_A = "key_2", dataset_B = "key_3")
#' jk[["dataset_A"]][["dataset_C"]] <- NULL # removes key
#'
#' jk
`[[<-.join_keys` <- function(x, i, value) {
  checkmate::assert(
    combine = "or",
    checkmate::check_string(i),
    checkmate::check_integerish(i, len = 1),
    checkmate::check_logical(i, len = length(x))
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
    # Invert key
    new_value <- setNames(names(norm_value[[ds2]]), norm_value[[ds2]])
    keep_value[[i]] <- new_value

    # Assign symmetrical
    new_x[[ds2]] <- keep_value
  }

  preserve_attr <- attributes(new_x)[!names(attributes(new_x)) %in% "names"]
  # Remove NULL or empty keys
  new_x <- Filter(function(x) length(x) != 0L, new_x)
  attributes(new_x) <- utils::modifyList(attributes(new_x), preserve_attr)

  #
  # restore class
  class(new_x) <- class(x)
  new_x
}
