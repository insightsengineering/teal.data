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
join_key <- function(dataset_1, dataset_2 = dataset_1, keys) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  checkmate::assert_character(keys, any.missing = FALSE)

  if (length(keys) > 0) {
    if (is.null(names(keys))) {
      names(keys) <- keys
    }

    if (any(names(keys) == "")) {
      names(keys)[names(keys) == "" & keys != ""] <- keys[names(keys) == "" & keys != ""]
    }

    stopifnot(!is.null(names(keys)))
    stopifnot(!anyDuplicated(keys))
    stopifnot(!anyDuplicated(names(keys)))
  }

  if (dataset_1 == dataset_2 && any(names(keys) != keys)) {
    stop("Keys within a dataset must match exactly: keys = c('A' = 'B') are not allowed")
  }

  structure(
    list(
      dataset_1 = dataset_1,
      dataset_2 = dataset_2,
      keys = keys
    ),
    class = "JoinKeySet"
  )
}
