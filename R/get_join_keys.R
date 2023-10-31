#' Function to get join keys from a `` object
#' @param data `` - object to extract the join keys
#' @return Either `JoinKeys` object or `NULL` if no join keys
#' @export
get_join_keys <- function(data) {
  lifecycle::deprecate_stop(
    when = " 0.3.1",
    what = "get_join_keys(data)",
    details = "Use `join_keys(data)` instead."
  )
}

#' @rdname get_join_keys
#' @inheritParams mutate_join_keys
#' @param value value to assign
#' @export
`get_join_keys<-` <- function(data, dataset_1, dataset_2 = NULL, value) {
  lifecycle::deprecate_stop(
    when = " 0.3.1",
    what = "get_join_keys(data) <- ...",
    details = "Use `join_keys(data) <- ...`"
  )
}
