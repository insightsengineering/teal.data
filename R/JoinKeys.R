# Constructors ====

#' Create a `JoinKeys` out of a list of `JoinKeySet` objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @details Note that join keys are symmetric although the relationship only needs
#' to be specified once.
#
#' @param ... optional, a `JoinKeySet` objects created using the `join_key` function.
#'
#' @return `JoinKeys`
#'
#' @export
#'
#' @examples
#' # setting join keys
#'
#' jk <- join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' jk
#'
#' # or
#' jk <- join_keys()
#' jk["dataset_A", "dataset_B"] <- c("col_1" = "col_a")
#' jk["dataset_A", "dataset_C"] <- c("col_2" = "col_x", "col_3" = "col_y")
#' jk
join_keys <- function(...) {
  x <- rlang::list2(...)

  # Getter
  if (checkmate::test_list(x, len = 1, types = c("JoinKeys"))) {
    return(x[[1]])
  } else if (checkmate::test_list(x, len = 1, types = c("teal_data"))) {
    return(x[[1]]@join_keys)
  } else if (checkmate::test_list(x, len = 1, types = c("TealData"))) {
    return(x[[1]]$get_join_keys())
  }

  # Constructor
  res <- new_join_keys()
  if (length(x) > 0) {
    join_keys(res) <- x
  }

  res
}

#' @rdname join_keys
#'
#' @details
#' `cdisc_join_keys` is a wrapper around `join_keys` that sets the default
#' join keys for CDISC datasets. It is used internally by `cdisc_data` to
#' set the default join keys for CDISC datasets.
#'
#' @export
#'
#' @examples
#'
#' # Default CDISC join keys
#'
#' cdisc_join_keys(join_key("dataset_A", "dataset_B", c("col_1" = "col_a")), "ADTTE")
cdisc_join_keys <- function(...) {
  data_objects <- rlang::list2(...)

  jk <- join_keys()
  for (ix in seq_along(data_objects)) {
    item <- data_objects[[ix]]
    name <- names(data_objects)[ix]

    if (checkmate::test_class(item, "JoinKeySet")) {
      jk[item$dataset_1, item$dataset_2] <- item$keys
    } else if (
      checkmate::test_multi_class(item, c("TealDataConnector", "TealDataset", "TealDatasetConnector"))
    ) {
    } else {
      if ((is.null(name) || identical(trimws(name), "")) && is.character(item)) {
        name <- item
      }
      if (name %in% names(default_cdisc_keys)) {
        # Set default primary keys
        keys_list <- default_cdisc_keys[[name]]
        jk[name] <- keys_list$primary

        if (!is.null(keys_list$parent) && !is.null(keys_list$foreign)) {
          jk[name, keys_list$parent] <- keys_list$foreign
        }
      }
    }
  }

  jk
}

# wrappers ====
#' Mutate `JoinKeys` with a new values
#'
#' @description `r lifecycle::badge("experimental")`
#' Mutate `JoinKeys` with a new values
#'
#' @param x (`JoinKeys`) object to be modified
#' @param dataset_1 (`character`) one dataset name
#' @param dataset_2 (`character`) other dataset name
#' @param value (named `character`) column names used to join
#'
#' @return modified `JoinKeys` object
#'
#' @export
mutate_join_keys <- function(x, dataset_1, dataset_2, value) {
  UseMethod("mutate_join_keys")
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' # TealData ----
#'
#' ADSL <- teal.data::example_cdisc_data("ADSL")
#' ADRS <- teal.data::example_cdisc_data("ADRS")
#'
#' x <- cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADRS", ADRS)
#' )
#' join_keys(x)$get("ADSL", "ADRS")
#'
#' mutate_join_keys(x, "ADSL", "ADRS", c("COLUMN1" = "COLUMN2"))
#' join_keys(x)$get("ADSL", "ADRS")
mutate_join_keys.TealData <- function(x, dataset_1, dataset_2, value) { # nolint
  x@join_keys <- mutate_join_keys(x@join_keys, dataset_1, dataset_2, value)
  x@join_keys
}
