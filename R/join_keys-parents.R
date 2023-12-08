#' Getter and setter functions for parents attribute of `join_keys`
#'
#' @description
#' `parents()` facilitates the creation of dependencies between datasets by
#' assigning a parent-child relationship.
#'
#' Each element is defined by a list element, where `list("child" = "parent")`.
#'
#' @param x (`join_keys` or `teal_data`) object that contains "parents" information
#' to retrieve or manipulate.
#'
#' @return a list of `character` representing the parents.
#'
#' @export
#' @seealso [join_keys()]
parents <- function(x) {
  UseMethod("parents", x)
}

#' @describeIn parents Retrieves parents of `join_keys` object.
#' @export
#' @examples
#' # Get parents of join_keys ---
#'
#' jk <- default_cdisc_join_keys["ADEX"]
#' parents(jk)
parents.join_keys <- function(x) {
  if (is.null(attr(x, "parents"))) list() else attr(x, "parents")
}

#' @describeIn parents Retrieves parents of `join_keys` inside `teal_data` object.
#' @export
#' @examples
#'
#' # Get parents of join_keys inside teal_data object ---
#'
#' td <- teal_data(
#'   ADSL = teal.data::rADSL,
#'   ADTTE = teal.data::rADTTE,
#'   ADRS = teal.data::rADRS,
#'   join_keys = default_cdisc_join_keys[c("ADSL", "ADTTE", "ADRS")]
#' )
#' parents(td)
parents.teal_data <- function(x) {
  parents(x@join_keys)
}

#' @describeIn parents Assignment of parents in `join_keys` object.
#'
#' @param value (`named list`) of `character` vectors.
#'
#' @export
`parents<-` <- function(x, value) {
  UseMethod("parents<-", x)
}

#' @describeIn parents Assignment of parents of `join_keys` object.
#' @export
#' @examples
#'
#' # Assigment of parents ---
#'
#' jk <- join_keys(
#'   join_key("ds1", "ds2", "id"),
#'   join_key("ds5", "ds6", "id"),
#'   join_key("ds7", "ds6", "id")
#' )
#'
#' parents(jk) <- list(ds2 = "ds1")
#'
#' # Setting individual parent-child relationship
#'
#' parents(jk)["ds6"] <- "ds5"
#' parents(jk)["ds7"] <- "ds6"
`parents<-.join_keys` <- function(x, value) {
  checkmate::assert_list(value, types = c("character"), names = "named")

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

  attr(x, "parents") <- new_parents

  assert_parent_child(x)
  x
}

#' @describeIn parents Assignment of parents of `join_keys` inside `teal_data` object.
#' @export
#' @examples
#'
#' # Assigment of parents of join_keys inside teal_data object ---
#'
#' parents(td) <- list("ADTTE" = "ADSL") # replace existing
#' parents(td)["ADRS"] <- "ADSL" # add new parent
`parents<-.teal_data` <- function(x, value) {
  parents(x@join_keys) <- value
  x
}

#' @describeIn parents Getter for individual parent
#'
#' @param dataset_name (`character(1)`) Name of dataset to query on their parent.
#'
#' @return For `parent(x, dataset_name)` returns `NULL` if parent does not exist.
#'
#' @export
#'
#' @examples
#'
#' # Get individual parent ---
#'
#' parent(jk, "ds2")
#' parent(td, "ADTTE")
parent <- function(x, dataset_name) {
  checkmate::assert_string(dataset_name)
  # assert x is performed by parents()
  parents(x)[[dataset_name]]
}
