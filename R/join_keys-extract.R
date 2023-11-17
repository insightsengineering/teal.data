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
  if (missing(i)) {
    return(x)
  }

  if (is.null(i)) {
    return(join_keys()) # replicate base R
  }

  if (!missing(j)) {
    checkmate::assert(
      combine = "or",
      checkmate::check_string(i),
      checkmate::check_integerish(i, len = 1),
      checkmate::check_logical(i, len = length(x))
    )
    checkmate::assert(
      combine = "or",
      checkmate::check_string(j),
      checkmate::check_integerish(j, len = 1),
      checkmate::check_logical(j, len = length(x))
    )

    subset_x <- update_keys_given_parents(x)[union(i, j)]
    return(subset_x[[i]][[j]])
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
