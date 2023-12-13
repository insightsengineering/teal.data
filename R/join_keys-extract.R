#' @rdname join_keys
#' @order 2
#'
#' @section Functions:
#' - `x[datanames]`: Returns a subset of the `join_keys` object for
#' given `datanames`, including parent `datanames` and symmetric mirror keys between
#' `datanames` in the result.
#' - `x[i, j]`: Returns join keys between datasets `i` and `j`,
#'   including implicit keys inferred from their relationship with a parent.
#'
#' @param i,j indices specifying elements to extract or replace. Index should be a
#' a character vector, but it can also take numeric, logical, `NULL` or missing.
#'
#' @export
#'
#' @examples
#'
#' # Getter for join_keys ---
#'
#' jk["ds1", "ds2"]
#'
#' # Subsetting join_keys ----
#'
#' jk["ds1"]
#' jk[1:2]
#' jk[c("ds1", "ds2")]
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
        checkmate::test_number(i),
        checkmate::test_logical(i, len = length(x)) && sum(j) == 1
      ) ||
        !any(
          checkmate::test_string(j),
          checkmate::test_number(j),
          checkmate::test_logical(j, len = length(x)) && sum(j) == 1
        )
    ) {
      stop(
        "join_keys[i, j] - Can't extract keys for multiple pairs.",
        "When specifying a pair [i, j], both indices must point to a single key pair.\n",
        call. = FALSE
      )
    }
    if (is.numeric(i)) i <- names(x)[i]
    if (is.numeric(j)) j <- names(x)[j]

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
    checkmate::check_numeric(i),
    checkmate::check_logical(i)
  )


  # Convert integer/logical index to named index
  if (checkmate::test_numeric(i) || checkmate::test_logical(i)) {
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
#' @order 2
#'
#' @param parent (`character(1)`) indicates which dataset is the parent in the
#' relationship or `none` if it is an undirected relationship. One of `i`,
#' `j` or `none`.
#'
#' @section Functions:
#' - `x[i, j] <- value`: Assignment of a key to pair `(i, j)`.
#' - `x[i] <- value`: This (without `j` parameter) **is not** a supported
#' operation for `join_keys`.
#' - `join_keys(x)[i, j] <- value`: Assignment to `join_keys` object stored in `x`,
#' such as a `teal_data` or `join_keys` itself.
#'
#' @export
#' @examples
#'
#' # Setting a new primary key ---
#'
#' jk["ds4", "ds4"] <- "pk4"
#' jk["ds5", "ds5"] <- "pk5"
#'
#' # Setting a single relationship pair ---
#'
#' jk["ds1", "ds4"] <- c("pk1" = "pk4")
#'
#' # Removing a key ---
#'
#' jk["ds5", "ds5"] <- NULL
`[<-.join_keys` <- function(x, i, j, parent = c("i", "j", "none"), value) {
  parent <- match.arg(parent)
  if (missing(i) || missing(j)) {
    stop("join_keys[i, j] specify both indices to set a key pair.")
  } else if (!missing(i) && is.null(i) || !missing(j) && is.null(j)) {
    stop("join_keys[i, j] neither i nor j can be NULL.")
  } else if (
    !any(
      checkmate::test_string(i),
      checkmate::test_number(i),
      checkmate::test_logical(i, len = length(x)) && sum(j) == 1
    ) ||
      !any(
        checkmate::test_string(j),
        checkmate::test_number(j),
        checkmate::test_logical(j, len = length(x)) && sum(j) == 1
      )
  ) {
    stop(
      "join_keys[i, j] <- Can't set keys to specified indices.\n",
      "When setting pair [i, j], both indices must point to a single key pair.\n",
      call. = FALSE
    )
  }

  # Handle join key removal separately
  if (is.null(value)) {
    x[[i]][[j]] <- NULL
    return(x)
  }

  parent_conversion <- switch(parent,
    i = "dataset_1",
    j = "dataset_2",
    "none"
  )

  c(x, join_key(i, j, value, parent_conversion))
}

#' @noRd
#' @rdname join_keys
#'
#' @order 1000
#' @usage ## Prefered method is x[i, j] <- value
#' x[[i]][[j]] <- value
#'
#' @section Functions:
#' - `x[[i]][[j]] <- value`: It is equivalent as  `x[i, j] <- value`.
#'
#' @export
#' @examples
#'
#' # Setting via x[[i]] <- value ---
#'
#' jk <- join_keys()
#' jk[["ds6"]][["ds6"]] <- "pk6"
#' jk[["ds7"]] <- list(ds7 = "pk7", ds6 = c(pk7 = "pk6"))
#' jk[["ds7"]][["ds7"]] <- NULL # removes key
#'
#' jk
`[[<-.join_keys` <- function(x, i, value) {
  checkmate::assert(
    combine = "or",
    checkmate::check_string(i),
    checkmate::check_number(i),
    checkmate::check_logical(i, len = length(x))
  )
  checkmate::assert_list(value, names = "named", types = "character", null.ok = TRUE)
  if (checkmate::test_numeric(i) || checkmate::test_logical(i)) {
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

  # Keep only elements with length > 0L
  norm_value <- Filter(length, norm_value)

  # Remove classes to use list-based get/assign operations
  new_x <- unclass(x)

  # In case a pair is removed, also remove the symmetric pair and update parents
  removed_names <- setdiff(names(new_x[[i]]), names(norm_value))
  for (.x in removed_names) {
    if (identical(parent(x, .x), i)) attr(new_x, "parents")[[.x]] <- NULL
    if (identical(parent(x, i), .x)) attr(new_x, "parents")[[i]] <- NULL

    new_x[[.x]][[i]] <- NULL
  }

  new_x[[i]] <- norm_value

  # Iterate on all new values to create symmetrical pair
  for (ds2 in names(norm_value)) {
    if (ds2 == i) next

    keep_value <- if (is.null(x)) list() else new_x[[ds2]]

    # Invert key
    new_value <- stats::setNames(names(norm_value[[ds2]]), norm_value[[ds2]])
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
