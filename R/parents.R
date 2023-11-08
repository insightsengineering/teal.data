#' Getter and setter for specific parent
#'
#' @param join_keys_obj (`join_keys`) object to retrieve.
#' @param dataset_name (`character(1)`)
#'
#' @export
#'
#' @examples
#' jk <- join_keys()
#' parent(jk, "ds1")
#' parents(jk) <- list("ds2" = "ds3")
#' parent(jk, "ds2")
parent <- function(join_keys_obj, dataset_name) {
  if (missing(dataset_name)) {
    return(NULL)
  }
  checkmate::assert_string(dataset_name)
  # assert join_keys_obj is performed by parents()
  parents(join_keys_obj)[[dataset_name]]
}

#' Getter and setter functions for parents attribute of `join_keys`
#'
#' @param join_keys_obj (`join_keys`) object to retrieve or manipulate.
#' @return a list of `character` representing the parents.
#'
#' @export
parents <- function(join_keys_obj) {
  UseMethod("parents", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- join_keys()
#' parents(jk)
parents.join_keys <- function(join_keys_obj) {
  attr(join_keys_obj, "__parents__") %||% list()
}

#' @rdname parents
#'
#' @param value (`list`) named list of character values
#'
#' @export
#'
#' @examples
#' jk <- join_keys()
#' parents(jk) <- list(ADSL = "ADTTE")
`parents<-` <- function(join_keys_obj, value) {
  UseMethod("parents<-", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- join_keys()
#' parents(jk) <- list(ds1 = "ds2", "ds3" = "ds4")
#' parents(jk)["ds5"] <- "ds6"
#' parents(jk)["ds6"] <- "ds7"
`parents<-.join_keys` <- function(join_keys_obj, value) {
  if (missing(value)) {
    return(join_keys_obj)
  }
  checkmate::assert_list(value, types = "character", names = "named", min.len = 1)
  old_parents <- attr(join_keys_obj, "__parents__")

  for (dataset in names(value)) {
    parent <- old_parents[[dataset]]
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
      old_parents[[dataset]] <- value[[dataset]]
    }
  }
  attr(join_keys_obj, "__parents__") <- old_parents # nolint: object_name_linter
  join_keys_obj
}

#' Updates the keys of the datasets based on the parents.
#'
#' @param join_keys_obj (`join_keys`) object to update the keys.
#'
#' @return (`self`) invisibly for chaining
#'
#' @export
#'
#' @examples
#' jk <- join_keys()
#' join_keys(jk) <- list(
#'   join_key("df1", "df1", c("id", "id2")),
#'   join_key("df1", "df2", c("id" = "id")),
#'   join_key("df1", "df3", c("id" = "id"))
#' )
#' parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
#' jk2 <- update_keys_given_parents(jk)
#'
#' jk[["df2"]]
#' jk2[["df2"]]
update_keys_given_parents <- function(join_keys_obj) {
  jk <- join_keys(join_keys_obj)

  checkmate::assert_class(jk, "join_keys", .var.name = checkmate::vname(join_keys_obj))

  datanames <- names(jk)
  duplicate_pairs <- list()
  for (d1 in datanames) {
    d1_pk <- jk[d1, d1]
    d1_parent <- parents(jk)[[d1]]
    for (d2 in datanames) {
      if (paste(d2, d1) %in% duplicate_pairs) {
        next
      }
      if (length(jk[d1, d2]) == 0) {
        d2_parent <- parents(jk)[[d2]]
        d2_pk <- jk[d2, d2]

        fk <- if (identical(d1, d2_parent)) {
          # first is parent of second -> parent keys -> first keys
          d1_pk
        } else if (identical(d1_parent, d2)) {
          # second is parent of first -> parent keys -> second keys
          d2_pk
        } else if (identical(d1_parent, d2_parent) && length(d1_parent) > 0) {
          # both has the same parent -> parent keys
          jk[d1_parent, d1_parent]
        } else {
          # cant find connection - leave empty
          next
        }
        jk <- mutate_join_keys(jk, d1, d2, fk)
        duplicate_pairs <- append(duplicate_pairs, paste(d1, d2))
      }
    }
  }
  # check parent child relation
  assert_parent_child(join_keys_obj = jk)

  jk
}
