#' Manage relationships between datasets using `join_keys`
#' @order 1
#' @name join_keys
#'
#' @usage ## Constructor, getter and setter
#' join_keys(...)
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
#' @seealso [join_key()] for creating `join_keys_set`,
#' [parents()] for parent operations,
#' [teal_data()] for `teal_data` constructor _and_
#' [default_cdisc_join_keys] for default `CDISC` keys.
#'
#' @examples
#' # Creating a new join keys ----
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
#' @order 1
#' @export
join_keys.default <- function(...) {
  c(new_join_keys(), ...)
}

#' @rdname join_keys
#' @order 1
#' @export
join_keys.join_keys <- function(...) {
  x <- rlang::list2(...)
  x[[1]]
}

#' @rdname join_keys
#' @order 1
#' @export
join_keys.teal_data <- function(...) {
  x <- rlang::list2(...)
  x[[1]]@join_keys
}

#' @rdname join_keys
#' @order 1
#' @export
join_keys.TealData <- function(...) {
  x <- rlang::list2(...)
  x[[1]]$get_join_keys()
}

#' @rdname join_keys
#' @order 5
#'
#' @details
#' - `join_keys(x) <- value`: Assignment of the `join_keys` in object with `value`.
#' `value` needs to be an object of class `join_keys` or `join_key_set`.
#'
#' @param x (`join_keys`) empty object to set the new relationship pairs.
#' `x` is typically an object of `join_keys` class. When called with the `join_keys(x)`
#' or `join_keys(x) <- value` then it can also take a supported class (`teal_data`, `join_keys`)
#' @param value (`join_key_set` or list of `join_key_set`) relationship pairs to add
#' to `join_keys` list.
#'
#' @export
`join_keys<-` <- function(x, value) {
  checkmate::assert_class(value, classes = c("join_keys", "list"))
  UseMethod("join_keys<-", x)
}

#' @rdname join_keys
#' @order 5
#' @export
#' @examples
#'
#' # Assigning keys via join_keys(x)[i, j] <- value ----
#'
#' obj <- join_keys()
#' # or
#' obj <- teal_data()
#'
#' join_keys(obj)["ds1", "ds1"] <- "pk1"
#' join_keys(obj)["ds2", "ds2"] <- "pk2"
#' join_keys(obj)["ds3", "ds3"] <- "pk3"
#' join_keys(obj)["ds2", "ds1"] <- c(pk2 = "pk1")
#' join_keys(obj)["ds3", "ds1"] <- c(pk3 = "pk1")
#'
#' identical(jk, join_keys(obj))
`join_keys<-.join_keys` <- function(x, value) {
  value
}

#' @rdname join_keys
#' @order 5
#' @export
#' @examples
#'
#' # Setter for join_keys within teal_data ----
#'
#' td <- teal_data()
#' join_keys(td) <- jk
#'
#' join_keys(td)["ds1", "ds2"] <- "new_key"
#' join_keys(td) <- c(join_keys(td), join_keys(join_key("ds3", "ds2", "key3")))
#' join_keys(td)
`join_keys<-.teal_data` <- function(x, value) {
  join_keys(x@join_keys) <- value
  x
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
