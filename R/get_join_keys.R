#' Function to get join keys from a `tdata` object
#' @param data `tdata` - object to extract the join keys
#' @return Either `JoinKeys` object or `NULL` if no join keys
#' @export
get_join_keys <- function(data) {
  UseMethod("get_join_keys", data)
}


#' @rdname get_join_keys
#' @export
get_join_keys.tdata <- function(data) {
  data@join_keys
}

#' @rdname get_join_keys
#' @export
get_join_keys.ddl <- function(data) {
  attr(data, "join_keys")
}


#' @rdname get_join_keys
#' @export
get_join_keys.TealData <- function(data) {
  data$get_join_keys()
}
