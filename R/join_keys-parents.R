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
#'
#' parents(td) <- list("ADTTE" = "ADSL") # replace existing
#' parents(td)["ADRS"] <- "ADSL" # add new parent
`parents<-.teal_data` <- function(x, value) {
  parents(x@join_keys) <- value
  x
}
