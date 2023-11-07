#' Create a relationship between a pair of datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details `join_key()` will create a relationship for the variables on a pair
#' of datasets.
#'
#' @inheritParams mutate_join_keys
#' @param dataset_2 (optional `character`) other dataset name. In case it is omitted, then it
#' will create a primary key for `dataset_1`.
#' @param keys (optionally named `character`) where `names(keys)` are columns in `dataset_1`
#' with relationship to columns of `dataset_2` given by the elements in `keys`.
#' If `names(keys)` is `NULL` then the same column names are used for both `dataset_1`
#' and `dataset_2`.
#'
#' @return object of class `JoinKeySet` to be passed into `join_keys` function.
#'
#' @seealso [join_keys()]
#'
#' @export
#'
#' @examples
#' join_key("d1", "d2", c("A"))
#' join_key("d1", "d2", c("A" = "B"))
#' join_key("d1", "d2", c("A" = "B", "C"))
#' join_key("d1", "d2", c("A" = "B", "C" = ""))
join_key <- function(dataset_1, dataset_2 = dataset_1, keys) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  checkmate::assert_character(keys, any.missing = FALSE)

  if (length(keys) > 0) {
    if (is.null(names(keys))) {
      names(keys) <- keys
    }

    keys <- trimws(keys)
    names(keys) <- trimws(names(keys))

    # Remove keys with empty value and without name
    if (any(keys == "" & names(keys) == "")) {
      message("Key with an empty value and name are ignored.")
      keys <- keys[keys != "" & names(keys) != ""]
    }

    # Set name of keys without one: c("A") -> c("A" = "A")
    if (any(names(keys) == "")) {
      names(keys)[names(keys) == "" & keys != ""] <- keys[names(keys) == "" & keys != ""]
    }

    # Set value of keys with empty string, but non-empty name: c("A" = "") -> c("A" = "A")
    if (any(keys == "" & names(keys) != "")) {
      keys[keys == ""] <- names(keys[keys == ""])
    }

    stopifnot(!is.null(names(keys)))
    stopifnot(!anyDuplicated(keys))
    stopifnot(!anyDuplicated(names(keys)))
  }

  if (dataset_1 == dataset_2 && any(names(keys) != keys)) {
    stop("Keys within a dataset must match exactly: keys = c('A' = 'B') are not allowed")
  }

  structure(
    setNames(list(setNames(list(keys), dataset_2)), dataset_1),
    class = "JoinKeySet"
  )
}

#' Getter for attributes in `JoinKeySet` object
#'
#' Internal methods for `JoinKeySet` operations
#'
#' @param join_key_object (`JoinKeySet`) object to retrieve attribute from.
#' @return `dataset_1`, `dataset_2` or `key` as `character(1)`
#'
#' @keywords internal
get_dataset_1 <- function(join_key_object) {
  names(join_key_object)
}

#' @rdname get_dataset_1
#' @keywords internal
get_dataset_2 <- function(join_key_object) {
  names(join_key_object[[1]])
}

#' @rdname get_dataset_1
#' @keywords internal
get_keys.JoinKeySet <- function(join_key_object) {
  join_key_object[[1]][[1]]
}
# Remove this once generic `get_keys` is removed (and rename non-exported function to `get_keys`)
.S3method("get_keys", "JoinKeySet", get_keys.JoinKeySet)
