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
      return(invisible(self))
    },
    #' @description
    #' Split the current `JoinKeys` object into a named list of join keys objects with an element for each dataset
    #' @return (`list`) a list of `JoinKeys` object
    split = function() {
      split_join_keys(self)
    },
    #' @description
    #' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
    #' @param x  `list` of `JoinKeys` objects or single `JoinKeys` object
    #' @return (`self`) invisibly for chaining
    merge = function(x) {
      result <- merge_join_keys(self, x)
      class(result) <- "list"
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
      new_keys <- private$.keys
      class(new_keys) <- "Placeholder"
      res <- get_join_key(new_keys, dataset_1, dataset_2)
      if (checkmate::test_class(res, "Placeholder")) class(res) <- "list"
      res
    },
    #' @description
    #' Change join_keys for a given pair of dataset names (or
    #' add join_keys for given pair if it does not exist)
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate = function(dataset_1, dataset_2, val) {
      new_keys <- private$.keys
      class(new_keys) <- "Placeholder"
      res <- mutate_join_keys(new_keys, dataset_1, dataset_2, val)
      class(res) <- "list"

      private$.keys <- res
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
      jk <- private$.keys
      class(jk) <- c("Placeholder", "list")
      join_keys(jk) <- x
      class(jk) <- "list"
      private$.keys <- jk
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
      for (dataset in names(named_list)) {
        checkmate::assert(
          checkmate::check_null(self$get_parent(dataset)),
          checkmate::check_true(
            length(self$get_parent(dataset)) == 0 &&
              length(named_list[[dataset]]) == 0
          ),
          checkmate::check_true(self$get_parent(dataset) == named_list[[dataset]]),
          "Please check the difference between provided datasets parents and provided join_keys parents."
        )
        if (is.null(self$get_parent(dataset))) {
          private$parents[[dataset]] <- named_list[[dataset]]
        }
      }
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
      private$parents[[dataname]]
    },
    #' @description
    #' Gets the parents of the datasets.
    #'
    #' @return (`list`) A named list of the parents of all datasets
    get_parents = function() {
      private$parents
    },
    #' @description
    #' Updates the keys of the datasets based on the parents.
    #'
    #' @return (`self`) invisibly for chaining
    update_keys_given_parents = function() {
      datanames <- names(self$get())
      duplicate_pairs <- list()
      for (d1 in datanames) {
        d1_pk <- self$get(d1, d1)
        d1_parent <- self$get_parent(d1)
        for (d2 in datanames) {
          if (paste(d2, d1) %in% duplicate_pairs) {
            next
          }
          if (length(self$get(d1, d2)) == 0) {
            d2_parent <- self$get_parent(d2)
            d2_pk <- self$get(d2, d2)

            fk <- if (identical(d1, d2_parent)) {
              # first is parent of second -> parent keys -> first keys
              d1_pk
            } else if (identical(d1_parent, d2)) {
              # second is parent of first -> parent keys -> second keys
              d2_pk
            } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
              # both has the same parent -> parent keys
              self$get(d1_parent, d1_parent)
            } else {
              # cant find connection - leave empty
              next
            }
            self$mutate(d1, d2, fk)
            duplicate_pairs <- append(duplicate_pairs, paste(d1, d2))
          }
        }
      }
      # check parent child relation
      private$check_parent_child()

      invisible(self)
    }
  ),
  ## __Private Fields ====
  private = list(
    .keys = list(),
    parents = list(),
    join_pair = function(join_key) {
      res <- join_pair(self, join_key)
      class(res) <- "list"
      private$.keys <- res
    },
    # checks the parent child relations are valid
    check_parent_child = function() {
      if (!is.null(self$get_parents())) {
        parents <- self$get_parents()
        for (idx1 in seq_along(parents)) {
          name_from <- names(parents)[[idx1]]
          for (idx2 in seq_along(parents[[idx1]])) {
            name_to <- parents[[idx1]][[idx2]]
            keys_from <- self$get(name_from, name_to)
            keys_to <- self$get(name_to, name_from)
            if (length(keys_from) == 0 && length(keys_to) == 0) {
              stop(sprintf("No join keys from %s to its parent (%s) and vice versa", name_from, name_to))
            }
            if (length(keys_from) == 0) {
              stop(sprintf("No join keys from %s to its parent (%s)", name_from, name_to))
            }
            if (length(keys_to) == 0) {
              stop(sprintf("No join keys from %s parent name (%s) to %s", name_from, name_to, name_from))
            }
          }
        }
      }
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
