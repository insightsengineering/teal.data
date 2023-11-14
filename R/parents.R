#' Getter and setter for specific parent
#'
#' @param x (`join_keys`) object to retrieve.
#' @param dataset_name (`character(1)`)
#'
#' @export
#'
#' @examples
#' jk <- join_keys(join_key("ds1", "ds2", "key"))
#' parent(jk, "ds2")
#' parents(jk) <- list("ds2" = "ds1")
#' parent(jk, "ds2")
parent <- function(x, dataset_name) {
  checkmate::assert_string(dataset_name)
  # assert x is performed by parents()
  parents(x)[[dataset_name]]
}

#' Getter and setter functions for parents attribute of `join_keys`
#'
#' @param x (`join_keys`) object to retrieve or manipulate.
#' @return a list of `character` representing the parents.
#'
#' @export
parents <- function(x) {
  UseMethod("parents", x)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- default_cdisc_join_keys["ADEX"]
#' parents(jk)
parents.join_keys <- function(x) {
  attr(x, "__parents__") %||% list()
}

#' @rdname parents
#' @export
#' @examples
#'
#' td <- cdisc_data(
#'   ADSL = teal.data::rADSL,
#'   ADTTE = teal.data::rADTTE
#' )
#' parents(td)
parents.teal_data <- function(x) {
  attr(x@join_keys, "__parents__") %||% list()
}

#' @rdname parents
#'
#' @param value (`list`) named list of character values
#'
#' @export
`parents<-` <- function(x, value) {
  UseMethod("parents<-", x)
}

#' @rdname parents
#' @export
#' @examples
#'
#' jk <- join_keys(
#'   join_key("ds1", "ds2", "id"),
#'   join_key("ds5", "ds6", "id"),
#'   join_key("ds7", "ds6", "id")
#' )
#' parents(jk) <- list()
#' parents(jk) <- list(ds1 = "ds2")
#' parents(jk)["ds5"] <- "ds6"
#' parents(jk)["ds6"] <- "ds7"
`parents<-.join_keys` <- function(x, value) {
  checkmate::assert_list(value, types = "character", names = "named")

  new_parents <- list()

  for (dataset in names(value)) {
    parent <- new_parents[[dataset]]
    checkmate::assert(
      checkmate::check_null(parent),
      checkmate::check_true(
        length(parent) == 0 &&
          length(value[[dataset]]) == 0
      ),
      checkmate::check_true(parent == value[[dataset]]),
      "Please check the difference between provided datasets parents and provided join_keys parents."
    )
    if (is.null(parent)) {
      new_parents[[dataset]] <- value[[dataset]]
    }
  }

  if (is_dag(new_parents)) {
    stop("Cycle detected in a parent and child dataset graph.")
  }

  attr(x, "__parents__") <- new_parents # nolint: object_name_linter

  assert_parent_child(x)
  x
}

#' @rdname parents
#' @export
#' @examples
#'
#' td <- cdisc_data(
#'   ADSL = teal.data::rADSL,
#'   ADTTE = teal.data::rADTTE,
#'   ADRS = teal.data::rADRS
#' )
#' parents(td) <- list("ADTTE" = "ADSL") # replace existing
#' parents(td)["ADRS"] <- "ADSL" # add new parent
`parents<-.teal_data` <- function(x, value) {
  parents(x@join_keys) <- value
  x
}

#' Updates the keys of the datasets based on the parents.
#'
#' @param x (`join_keys`) object to update the keys.
#'
#' @return (`self`) invisibly for chaining
#'
#' @keywords internal
update_keys_given_parents <- function(x) {
  jk <- join_keys(x)

  checkmate::assert_class(jk, "join_keys", .var.name = checkmate::vname(x))

  datanames <- names(jk)
  duplicate_pairs <- list()
  for (d1 in datanames) {
    d1_pk <- jk[[d1]][[d1]]
    d1_parent <- parents(jk)[[d1]]
    for (d2 in datanames) {
      if (paste(d2, d1) %in% duplicate_pairs) {
        next
      }
      if (length(jk[[d1]][[d2]]) == 0) {
        d2_parent <- parent(jk, d2)
        d2_pk <- jk[[d2]][[d2]]

        fk <- if (identical(d1, d2_parent)) {
          # first is parent of second -> parent keys -> first keys
          d1_pk
        } else if (identical(d1_parent, d2)) {
          # second is parent of first -> parent keys -> second keys
          d2_pk
        } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
          # both has the same parent -> parent keys
          jk[[d1_parent]][[d1_parent]]
        } else {
          # cant find connection - leave empty
          next
        }
        jk[[d1]][[d2]] <- fk # mutate join key
        duplicate_pairs <- append(duplicate_pairs, paste(d1, d2))
      }
    }
  }
  # check parent child relation
  assert_parent_child(x = jk)

  jk
}
