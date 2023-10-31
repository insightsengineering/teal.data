## JoinKeys ====
#'
#'
#' @title R6 Class to store relationships for joining datasets
#'
#' @description `r lifecycle::badge("stable")`
#' This class stores symmetric links between pairs of key-values
#' (e.g. column A of dataset X can be joined with column B of dataset Y). This relationship
#' is more general than the SQL foreign key relationship which also imposes constraints on the values
#' of these columns.
#' @param dataset_1 (`character`) one dataset name
#' @param dataset_2 (`character`) other dataset name
#'
#' @examples
#' x <- teal.data:::JoinKeys$new()
#' x$set(
#'   list(
#'     join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'     join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#'   )
#' )
#' x$get()
#' x$mutate("dataset_A", "dataset_B", c("col1" = "col10"))
#' x$get("dataset_A", "dataset_B")
JoinKeys <- R6::R6Class( # nolint
  classname = "JoinKeys",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Create a new object of `JoinKeys`
    #' @return empty (`JoinKeys`)
    initialize = function() {
      logger::log_trace("JoinKeys initialized.")
      class(private$.keys) <- class(new_join_keys())
      return(invisible(self))
    },
    #' @description
    #' Split the current `JoinKeys` object into a named list of join keys objects with an element for each dataset
    #' @return (`list`) a list of `JoinKeys` object
    split = function() {
      split_join_keys(self$get())
    },
    #' @description
    #' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
    #' @param x  `list` of `JoinKeys` objects or single `JoinKeys` object
    #' @return (`self`) invisibly for chaining
    merge = function(x) {
      result <- merge_join_keys(self, x)
      private$.keys <- result
    },
    #' @description
    #' Get join keys between two datasets.
    #' @return (`character`) named character vector x with names(x) the
    #' columns of `dataset_1` and the values of `(x)` the corresponding join
    #' keys in `dataset_2` or `character(0)` if no relationship
    #' @details if one or both of `dataset_1` and `dataset_2` are missing then
    #' underlying keys structure is returned for further processing
    get = function(dataset_1, dataset_2) {
      get_join_key(private$.keys, dataset_1, dataset_2)
    },
    #' @description
    #' Change join_keys for a given pair of dataset names (or
    #' add join_keys for given pair if it does not exist)
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate = function(dataset_1, dataset_2, val) {
      private$.keys <- mutate_join_keys(private$.keys, dataset_1, dataset_2, val)
      return(invisible(self))
    },
    #' @description
    #' Set up join keys basing on list of `JoinKeySet` objects.
    #' @param x  `list` of `JoinKeySet` objects (which are created using the `join_key` function)
    #' or single `JoinKeySet` objects
    #' @details Note that join keys are symmetric although the relationship only needs
    #' to be specified once
    #' @return (`self`) invisibly for chaining
    set = function(x) {
      join_keys(private$.keys) <- x
      return(invisible(self))
    },
    #' @description
    #' Prints this `JoinKeys`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      print.Placeholder(private$.keys)
    },
    #' @description
    #' Sets the parents of the datasets.
    #'
    #' @param named_list Named (`list`) of the parents datasets.
    #'
    #' @return (`self`) invisibly for chaining
    set_parents = function(named_list) {
      parents(private$.keys) <- named_list
      invisible(self)
    },
    #' @description
    #' Gets the parent of the desired dataset.
    #'
    #' @param dataname (`character`) name of the dataset.
    #' @return (`character`) the parent of the desired dataset
    get_parent = function(dataname) {
      if (missing(dataname)) {
        return(NULL)
      }
      parents(private$.keys)[[dataname]]
    },
    #' @description
    #' Gets the parents of the datasets.
    #'
    #' @return (`list`) A named list of the parents of all datasets
    get_parents = function() {
      parents(private$.keys)
    },
    #' @description
    #' Updates the keys of the datasets based on the parents.
    #'
    #' @return (`self`) invisibly for chaining
    update_keys_given_parents = function() {
      private$.keys <- update_keys_given_parents(private$.keys)

      invisible(self)
    }
  ),
  ## __Private Fields ====
  private = list(
    .keys = list(),
    check_parent_child = function() {
      # Needed for a single test
      assert_parent_child(private$.keys)
    }
  )
)

# constructors ====

#' Create a `JoinKeys` out of a list of `JoinKeySet` objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param ... optional, a `JoinKeySet` objects created using the `join_key` function.
#' @details Note that join keys are symmetric although the relationship only needs
#' to be specified once.
#'
#' @return `JoinKeys`
#'
#' @export
#'
#' @examples
#' # setting join keys
#' join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' # or
#' jk <- join_keys()
#' jk["dataset_A", "dataset_B"] <- c("col_1" = "col_a")
#' jk["dataset_A", "dataset_C"] <- c("col_2" = "col_x", "col_3" = "col_y")
#'
join_keys <- function(...) {
  x <- rlang::list2(...)

  # Getter
  if (checkmate::test_list(x, len = 1, types = c("Placeholder", "JoinKeys"))) {
    return(x[[1]])
  } else if (checkmate::test_list(x, len = 1, types = c("teal_data"))) {
    return(x[[1]]@join_keys)
  } else if (checkmate::test_list(x, len = 1, types = c("TealData"))) {
    return(x[[1]]$get_join_keys())
  }

  # Constructor
  res <- JoinKeys$new()
  if (length(x) > 0) {
    res$set(x)
  }

  res
}

#' @title Getter for JoinKeys that returns the relationship between pairs of datasets
#' @param x JoinKeys object to extract the join keys
#' @param dataset_1 (`character`) name of first dataset.
#' @param dataset_2 (`character`) name of second dataset.
#' @export
#' @keywords internal
`[.JoinKeys` <- function(x, dataset_1, dataset_2 = dataset_1) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  x$get(dataset_1, dataset_2)
}

#' @rdname sub-.JoinKeys
#' @param value value to assign
#' @export
#' @keywords internal
`[<-.JoinKeys` <- function(x, dataset_1, dataset_2 = dataset_1, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2)
  x$mutate(dataset_1, dataset_2, value)
  x
}

#' @rdname join_keys
#' @details
#' `cdisc_join_keys` is a wrapper around `join_keys` that sets the default
#' join keys for CDISC datasets. It is used internally by `cdisc_data` to
#' set the default join keys for CDISC datasets.
#'
#' @export
#' @examples
#'
#' # default CDISC join keys
#' cdisc_join_keys(join_key("dataset_A", "dataset_B", c("col_1" = "col_a")), "ADTTE")
#'
cdisc_join_keys <- function(...) {
  data_objects <- rlang::list2(...)

  join_keys <- join_keys()
  lapply(seq_along(data_objects), function(ix) {
    item <- data_objects[[ix]]
    name <- names(data_objects)[ix]

    if (checkmate::test_class(item, "JoinKeySet")) {
      join_keys$set(item)
      return(NULL)
    } else if (
      checkmate::test_multi_class(item, c("TealDataConnector", "TealDataset", "TealDatasetConnector"))
    ) {
      return(NULL)
    } else {
      if ((is.null(name) || identical(trimws(name), "")) && is.character(item)) {
        name <- item
      }
      if (name %in% names(default_cdisc_keys)) {
        # Set default primary keys
        keys_list <- default_cdisc_keys[[name]]
        join_keys[name] <- keys_list$primary

        if (!is.null(keys_list$parent) && !is.null(keys_list$foreign)) {
          join_keys[name, keys_list$parent] <- keys_list$foreign
        }
      }
    }
  })

  join_keys
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
#' @param val (named `character`) column names used to join
#'
#' @return modified `JoinKeys` object
#'
#' @export
mutate_join_keys <- function(x, dataset_1, dataset_2, val) {
  UseMethod("mutate_join_keys")
}

#' @rdname mutate_join_keys
#' @export
#' @examples
#' # JoinKeys ----
#'
#' x <- join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' x$get("dataset_A", "dataset_B")
#'
#' mutate_join_keys(x, "dataset_A", "dataset_B", c("col_1" = "col_10"))
#' x$get("dataset_A", "dataset_B")
mutate_join_keys.JoinKeys <- function(x, dataset_1, dataset_2, val) {
  x$mutate(dataset_1, dataset_2, val)
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
#' x$get_join_keys()$get("ADSL", "ADRS")
#'
#' mutate_join_keys(x, "ADSL", "ADRS", c("COLUMN1" = "COLUMN2"))
#' x$get_join_keys()$get("ADSL", "ADRS")
mutate_join_keys.TealData <- function(x, dataset_1, dataset_2, val) { # nolint
  x$mutate_join_keys(dataset_1, dataset_2, val)
}

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
