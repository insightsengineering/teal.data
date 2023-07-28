#' S3 generic for creating an information summary about the duplicate key values in a dataset
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details The information summary provides row numbers and number of duplicates
#' for each duplicated key value.
#'
#' @param dataset `TealDataset` or `data.frame` a dataset, which will be tested
#' @param keys `character` vector of variable names in `dataset` consisting the key
#' or `keys` object, which does have a `primary` element with a vector of variable
#' names in `dataset` consisting the key. Optional, default: NULL
#'
#' @return a `tibble` with variables consisting the key and `row_no` and `duplicates_count` columns
#'
#' @note Raises an exception when this function cannot determine the primary key columns of the tested object.
#'
#' @examples
#'
#' adsl <- teal.data::example_cdisc_data("ADSL")
#' # create a TealDataset with default keys
#' rel_adsl <- cdisc_dataset("ADSL", adsl)
#' get_key_duplicates(rel_adsl)
#'
#' df <- as.data.frame(
#'   list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5))
#' )
#' res <- get_key_duplicates(df, keys = c("a", "b")) # duplicated keys are in rows 3 and 4
#' print(res) # prints a tibble
#' \dontrun{
#' get_key_duplicates(df) # raises an exception, because keys are missing with no default
#' }
#'
#' @export
get_key_duplicates <- function(dataset, keys = NULL) {
  UseMethod("get_key_duplicates", dataset)
}

#' @rdname get_key_duplicates
#' @export
get_key_duplicates.TealDataset <- function(dataset, keys = NULL) { # nolint
  df <- get_raw_data(dataset)
  if (is.null(keys)) {
    keys_ds <- get_keys(dataset)
    keys <- if (is.null(keys_ds)) character(0) else keys_ds
  }

  get_key_duplicates_util(df, keys)
}

#' @rdname get_key_duplicates
#' @export
get_key_duplicates.data.frame <- function(dataset, keys = NULL) { # nolint
  if (is.null(keys)) {
    attr_key <- attr(dataset, "primary_key")
    keys <- if (is.null(attr_key)) character(0) else attr
  }
  get_key_duplicates_util(dataset, keys)
}

#' Creates a duplicate keys information summary.
#'
#' @details
#' Accepts a list of variable names - `keys`, which are treated as the
#' key to the `data.frame` argument. An instance of duplicated key is
#' defined as two rows, which have the same values in columns defined by `keys`.
#' Per each key value with duplicates returns a row in a `tibble`. The return table
#' has columns corresponding to the variable names passed in `keys` and
#' two additional columns: `rows` and `n`, which provide
#' information about row numbers of the original dataframe, which contain duplicated keys
#' and total duplicates counts.
#'
#' @param dataframe dataframe
#' @param keys `character` vector of variable names consisting the key to the `data.frame`
#'
#' @return `data.frame` with a duplicate keys information summary
#'
#' @keywords internal
#'
#' @examples
#' df <- data.frame(
#'   a = c("a", "a", "b", "b", "c"),
#'   b = c(1, 2, 3, 3, 4),
#'   c = c(1, 2, 3, 4, 5)
#' )
#' res <- teal.data:::get_key_duplicates_util(df, keys = c("a", "b"))
#' print(res) # duplicated keys are in rows 3 and 4
#' @seealso [get_key_duplicates]
get_key_duplicates_util <- function(dataframe, keys) {
  checkmate::assert_data_frame(dataframe)
  checkmate::assert_character(keys)
  stopifnot(
    all(
      vapply(keys, FUN.VALUE = logical(1), FUN = function(key) key %in% colnames(dataframe))
    )
  )

  # The goal is to print values of duplicated primary keys with number of duplicates and row numbers
  duplicates <- dataframe[, keys, drop = FALSE]
  duplicates$dups <- duplicated(duplicates, fromLast = FALSE) | duplicated(duplicates, fromLast = TRUE)
  duplicates$row_number <- seq_len(nrow(duplicates))
  duplicates <- duplicates[duplicates$dups, ]
  duplicates$dups <- NULL

  if (nrow(duplicates) == 0) {
    duplicates$rows <- character(0)
    duplicates$row_number <- NULL
    duplicates$n <- integer(0)
    return(duplicates)
  }

  groups <- split(duplicates, duplicates[, keys, drop = FALSE], drop = TRUE)
  summary_list <- lapply(groups, function(group) {
    ans <- group[1, keys, drop = FALSE]
    ans$rows <- paste(group[, "row_number"], collapse = ",")
    ans$n <- nrow(group)
    ans
  })
  summary <- do.call(rbind, summary_list)
  rownames(summary) <- NULL
  summary
}
