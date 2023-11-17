#' Manage relationships between datasets using `join_keys`
#'
#' @description
#' `join_keys()` facilitates the creation and retrieval of relationships between datasets.
#' `join_keys` class extends a list and contains keys connecting pairs of datasets. Each element
#' of the list contains keys for specific dataset. Each dataset can have a relationship with
#' itself (primary key) and with other datasets.
#'
#' Note that `join_keys` list is symmetrical, that is, when keys are set between `dat1` and `dat2` it
#' is automatically mirrored between `dat2` and `dat1`.
#'
#' @details
#'
#' - `join_keys()`: Returns an empty `join_keys` object when called without arguments.
#' - `join_keys(x)`: Returns the `join_keys` object contained in `x` (if it contains one).
#' - `join_keys(...)`: Creates a new object with one or more `join_key_set` parameters.
#' - `join_keys[datanames]`: Returns a subset of the `join_keys` object for given datanames,
#'   including their symmetric mirror keys.
#' - `join_keys[i, j]`: Returns join keys between datasets `i` and `j`,
#'   including implicit keys inferred from their relationship with a parent.
#'
#'
#' @order 1
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
#'   join_key("ds1", "ds1", "pk1"),
#'   join_key("ds2", "ds2", "pk2"),
#'   join_key("ds3", "ds3", "pk3"),
#'   join_key("ds2", "ds1", c(pk2 = "pk1")),
#'   join_key("ds3", "ds1", c(pk3 = "pk1"))
#' )
#'
#' jk
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
join_keys.default <- function(...) {
  c(new_join_keys(), ...)
}

#' @rdname join_keys
#' @export
join_keys.join_keys <- function(...) {
  x <- rlang::list2(...)
  x[[1]]
}

#' @rdname join_keys
#' @order 1000
#' @export
#' @examples
#' # Using a `join_keys` with `teal_data`
#'
#' td <- teal_data()
#' join_keys(td) <- join_keys(
#'   join_key("ds1", "ds1", "pk1"),
#'   join_key("ds2", "ds2", "pk2"),
#'   join_key("ds3", "ds3", "pk3"),
#'   join_key("ds2", "ds1", c(pk2 = "pk1")),
#'   join_key("ds3", "ds1", c(pk3 = "pk1"))
#' )
#' join_keys(td)
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
#'
#' @details
#' - "`join_keys(obj) <- value`" will set the `join_keys` in object with `value`.
#' `value` needs to be an object of class `join_keys` or `join_key_set`.
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
#' jk <- join_keys()
#' jk["ds1", "ds1"] <- "pk1"
#' jk["ds2", "ds2"] <- "pk2"
#' jk["ds3", "ds3"] <- "pk3"
#' jk["ds2", "ds1"] <- c(pk2 = "pk1")
#' jk["ds3", "ds1"] <- c(pk3 = "pk1")
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
#' join_keys(td)["ds1", "ds2"] <- "key1"
#' join_keys(td) <- c(join_keys(td), join_keys(join_key("ds3", "ds2", "key3")))
#' join_keys(td)
`join_keys<-.teal_data` <- function(x, value) {
  join_keys(x@join_keys) <- value
  x
}


#' @rdname join_keys
#'
#' @details
#' - `[<-` is not a supported operation for `join_keys`.
#'
#' @export
`[<-.join_keys` <- function(x, i, j, value) {
  if (missing(j)) {
    stop("Can't use `[<-` for object `join_keys` with only i. Use [[<- instead.")
  }

  checkmate::assert_string(i)
  checkmate::assert_string(j)

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
