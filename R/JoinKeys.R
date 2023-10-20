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
      list_of_list_of_join_key_set <- lapply(
        names(self$get()),
        function(dataset_1) {
          lapply(
            names(self$get()[[dataset_1]]),
            function(dataset_2) join_key(dataset_1, dataset_2, self$get()[[dataset_1]][[dataset_2]])
          )
        }
      )
      res <- lapply(
        list_of_list_of_join_key_set,
        function(x) {
          y <- JoinKeys$new()
          y$set(x)
        }
      )
      names(res) <- names(self$get())

      logger::log_trace("JoinKeys$split keys split.")
      return(res)
    },
    #' @description
    #' Merging a list (or one) of `JoinKeys` objects into the current `JoinKeys` object
    #' @param x  `list` of `JoinKeys` objects or single `JoinKeys` object
    #' @return (`self`) invisibly for chaining
    merge = function(x) {
      if (inherits(x, "JoinKeys")) x <- list(x)
      checkmate::assert_list(x, types = "JoinKeys", min.len = 1)
      for (jk in x) {
        for (dataset_1 in names(jk$get())) {
          for (dataset_2 in names(jk$get()[[dataset_1]])) {
            self$mutate(dataset_1, dataset_2, jk$get()[[dataset_1]][[dataset_2]])
          }
        }
      }
      logger::log_trace("JoinKeys$merge keys merged.")
      return(invisible(self))
    },
    #' @description
    #' Get join keys between two datasets.
    #' @return (`character`) named character vector x with names(x) the
    #' columns of `dataset_1` and the values of `(x)` the corresponding join
    #' keys in `dataset_2` or `character(0)` if no relationship
    #' @details if one or both of `dataset_1` and `dataset_2` are missing then
    #' underlying keys structure is returned for further processing
    get = function(dataset_1, dataset_2) {
      if (missing(dataset_1) && missing(dataset_2)) {
        return(private$.keys)
      }
      if (missing(dataset_2)) {
        return(private$.keys[[dataset_1]])
      }
      if (missing(dataset_1)) {
        return(private$.keys[[dataset_2]])
      }
      if (is.null(private$.keys[[dataset_1]][[dataset_2]])) {
        return(character(0))
      }
      return(private$.keys[[dataset_1]][[dataset_2]])
    },
    #' @description
    #' Change join_keys for a given pair of dataset names (or
    #' add join_keys for given pair if it does not exist)
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate = function(dataset_1, dataset_2, val) {
      checkmate::assert_string(dataset_1)
      checkmate::assert_string(dataset_2)
      checkmate::assert_character(val, any.missing = FALSE)

      private$join_pair(join_key(dataset_1, dataset_2, val))

      logger::log_trace(
        sprintf(
          "JoinKeys$mutate updated the keys between %s and %s to %s",
          dataset_1,
          dataset_2,
          paste(val, collapse = ", ")
        )
      )
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
      if (length(private$.keys) > 0) {
        stop("Keys already set, please use JoinKeys$mutate() to change them")
      }
      if (inherits(x, "JoinKeySet")) {
        x <- list(x)
      }

      # check if any JoinKeySets share the same datasets but different values
      for (idx_1 in seq_along(x)) {
        for (idx_2 in seq_len(idx_1)) {
          private$check_compatible_keys(x[[idx_1]], x[[idx_2]])
        }
      }

      checkmate::assert_list(x, types = "JoinKeySet", min.len = 1)
      lapply(x, private$join_pair)

      logger::log_trace("JoinKeys$set keys are set.")
      return(invisible(self))
    },
    #' @description
    #' Prints this `JoinKeys`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)
      keys_list <- self$get()
      if (length(keys_list) > 0) {
        cat(sprintf(
          "A JoinKeys object containing foreign keys between %s datasets:\n",
          length(keys_list)
        ))
        print(keys_list)
      } else {
        cat("An empty JoinKeys object.")
      }
      invisible(self)
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
      dataset_1 <- join_key$dataset_1
      dataset_2 <- join_key$dataset_2
      keys <- join_key$keys

      if (is.null(private$.keys[[dataset_1]])) {
        private$.keys[[dataset_1]] <- list()
      }
      private$.keys[[dataset_1]][[dataset_2]] <- keys

      if (dataset_2 != dataset_1) {
        if (is.null(private$.keys[[dataset_2]])) {
          private$.keys[[dataset_2]] <- list()
        }

        if (length(keys) > 0) {
          keys <- setNames(names(keys), keys)
        }
        private$.keys[[dataset_2]][[dataset_1]] <- keys
      }
    },
    # helper function to deterimine if two key sets contain incompatible keys
    # return TRUE if compatible, throw error otherwise
    check_compatible_keys = function(join_key_1, join_key_2) {
      error_message <- function(dataset_1, dataset_2) {
        stop(
          paste("cannot specify multiple different join keys between datasets:", dataset_1, "and", dataset_2)
        )
      }


      # if first datasets and the second datasets match and keys
      # must contain the same named elements
      if (join_key_1$dataset_1 == join_key_2$dataset_1 && join_key_1$dataset_2 == join_key_2$dataset_2) {
        if (!identical(sort(join_key_1$keys), sort(join_key_2$keys))) {
          error_message(join_key_1$dataset_1, join_key_1$dataset_2)
        }
      }

      # if first dataset of join_key_1 matches second dataset of join_key_2
      # and the first dataset of join_key_2 must match second dataset of join_key_1
      # and keys must contain the same elements but with names and values swapped
      if (join_key_1$dataset_1 == join_key_2$dataset_2 && join_key_1$dataset_2 == join_key_2$dataset_1) {
        # have to handle empty case differently as names(character(0)) is NULL
        if (length(join_key_1$keys) == 0 && length(join_key_2$keys) == 0) {
          return(TRUE)
        }

        if (xor(length(join_key_1$keys) == 0, length(join_key_2$keys) == 0) ||
          !identical(sort(join_key_1$keys), sort(setNames(names(join_key_2$keys), join_key_2$keys)))) {
          error_message(join_key_1$dataset_1, join_key_1$dataset_2)
        }
      }

      # otherwise they are compatible
      return(TRUE)
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
#' join_keys()
#' join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a")),
#'   join_key("dataset_A", "dataset_C", c("col_2" = "col_x", "col_3" = "col_y"))
#' )
#' join_keys(
#'   join_key("dataset_A", "dataset_B", c("col_1" = "col_a"))
#' )
join_keys <- function(...) {
  x <- list(...)
  res <- JoinKeys$new()
  if (length(x) > 0) {
    res$set(x)
  }

  res
}

#' @title Getter for JoinKeys that returns the relationship between pairs of datasets
#' @export
`[.JoinKeys` <- function(x, dataset_1, dataset_2 = NULL) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  dataset_2 <- dataset_2 %||% dataset_1
  x$get(dataset_1, dataset_2)
}

#' @rdname sub-.JoinKeys
#' @export
`[<-.JoinKeys` <- function(x, dataset_1, dataset_2 = NULL, value) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)

  dataset_2 <- dataset_2 %||% dataset_1
  x$mutate(dataset_1, dataset_2, value)
  x
}

#' @rdname join_keys
#' @details
#' `join_keys_cdisc` treat non-`JoinKeySet` arguments as possible CDISC datasets.
#' The `dataname` is extrapolated from the name  (or fallback to the value itself if
#' it's a `character(1)`).
#'
#' @export
#' @examples
#' join_keys_cdisc(join_key("dataset_A", "dataset_B", c("col_1" = "col_a")), "ADTTE")
#'
join_keys_cdisc <- function(...) {
  x <- list(...)

  x_parsed <- lapply(seq_along(x), function(ix) {
    item <- x[[ix]]

    name <- names(x)[ix] %||% item # fallback to value if names are not set
    if (
      checkmate::test_class(item, "JoinKeySet") ||
        !checkmate::test_string(name, min.chars = 1) ||
        !name %in% names(default_cdisc_keys)) {
      return(list(item))
    }

    # Add primary key
    result <- list(primary_key(name, keys = get_cdisc_keys(name)))
    keys_list <- default_cdisc_keys[[name]]

    if (is.null(keys_list) || is.null(keys_list$parent) || is.null(keys_list$foreign)) {
      return(result)
    }
    # Add JoinKey with parent dataset (if exists)
    append(result, list(join_key(name, keys_list$parent, keys = keys_list$foreign)))
  })
  x_parsed <- do.call(c, x_parsed)

  do.call(join_keys, x_parsed)
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
#' @param dataset_2 (`character`) other dataset name. In case it is omitted, then it
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
join_key <- function(dataset_1, dataset_2 = NULL, keys) {
  checkmate::assert_string(dataset_1)
  checkmate::assert_string(dataset_2, null.ok = TRUE)
  checkmate::assert_character(keys, any.missing = FALSE)

  dataset_2 <- dataset_2 %||% dataset_1

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

#' @rdname join_key
#' @export
#'
#' @details
#' `primary_key()` will create a primary key for a dataset. It is equivalent to
#' `join_key(...)` and omitting `dataset_2` argument or giving it the same name
#' `dataset_1`.
#'
primary_key <- function(dataset_1, keys) {
  if (checkmate::test_character(keys) &&
    !checkmate::test_names(names(keys), type = "unnamed")) {
    stop("Primary keys parameter must be a unamed character vector: keys = c('A' = 'A') are not allowed")
  }
  join_key(dataset_1 = dataset_1, dataset_2 = dataset_1, keys = keys)
}
