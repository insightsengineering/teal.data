#' Create a relationship between a pair of datasets
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details `join_key()` will create a relationship for the variables on a pair
#' of datasets.
#'
#' @param dataset_1,dataset_2 (`character(1)`) dataset names. If `dataset_2` is omitted,
#'  a primary key for `dataset_1` is created.
#' @param keys (optionally named `character`) where `names(keys)` are columns in `dataset_1`
#' corresponding to columns of `dataset_2` given by the elements of `keys`.
#'
#' If unnamed, the same column names are used for both datasets.
#'
#' If any element of the `keys` vector is empty with a non-empty name, then the name is
#' used for both datasets.
#'
#' @return object of class `join_key_set` to be passed into `join_keys` function.
#'
#' @seealso [join_keys()]
#'
#' @export
#'
#' @examples
#' join_key("d1", "d2", c("A"))
#' join_key("d1", "d2", c("A" = "B"))
#' join_key("d1", "d2", c("A" = "B", "C"))
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
      names(keys)[names(keys) == ""] <- keys[names(keys) == ""]
    }

    # Set value of keys with empty string, but non-empty name: c("A" = "") -> c("A" = "A")
    if (any(keys == "")) {
      keys[keys == ""] <- names(keys[keys == ""])
    }

    stopifnot(!is.null(names(keys)))
    stopifnot(!anyDuplicated(keys))
    stopifnot(!anyDuplicated(names(keys)))

    if (dataset_1 == dataset_2 && any(names(keys) != keys)) {
      stop("Keys within a dataset must match exactly: keys = c('A' = 'B') are not allowed")
    }
  } else {
    keys <- NULL
  }

  structure(
    list(
      structure(
        list(keys),
        names = dataset_2
      )
    ),
    names = dataset_1,
    class = "join_key_set"
  )
}
