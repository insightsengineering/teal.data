#' A name
#' @export
parents <- function(join_keys_obj) {
  UseMethod("parents", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk)
parents.Placeholder <- function(join_keys_obj) {
  rlang::`%||%`(attr(join_keys_obj, "__parents__"), list())
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk) <- list(ADSL = "ADTTE")
`parents<-` <- function(join_keys_obj, value) {
  UseMethod("parents<-", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk)["ADTTE"] <- "ADSL"
`parents<-.Placeholder` <- function(join_keys_obj, value) {
  checkmate::assert_list(value, types = "character", names = "named", min.len = 1)
  attr(join_keys_obj, "__parents__") <- value
  join_keys_obj
}


#' @rdname parents
#' @export
#' @examples
`parents[` <- function(join_keys_obj, dataset) {
  UseMethod("parents[", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk)["ADTTE"] <- "ADSL"
#' parents(jk)["YADA"]
`parents[.Placeholder` <- function(join_keys_obj, dataset) {
  checkmate::assert_list(dataset, min.len = 1, names = "named")
  res <- attr(join_keys_obj, "__parents__")[[dataset]]
  if (is.null(res)) {
    return(NULL)
  }
  res
}

#' @rdname parents
#' @export
`parents[[<-` <- function(join_keys_obj, dataset, value) {
  UseMethod("parent[<-", join_keys_obj)
}

#' @rdname parents
#' @export
`parents[<-` <- function(join_keys_obj, dataset, value) {
  UseMethod("parent[<-", join_keys_obj)
}

#' @rdname parents
#' @export
#' @examples
#' jk <- new_join_keys()
#' parents(jk)["ADTTE"] <- "ADSL"
`parents[<-.Placeholder` <- function(join_keys_obj, dataset, value) {
  checkmate::assert_character(dataset, min.len = 1)
  if (is.null(attr(join_keys_obj, "__parents__"))) {
    attr(join_keys_obj, "__parents__") <- list()
  }
  attr(join_keys_obj, "__parents__") <- dataset
  join_keys_obj
}
