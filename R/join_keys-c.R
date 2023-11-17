#' @rdname join_keys
#' @order 4
#' @export
#'
#' @examples
#'
#' # Merging multiple `join_keys` ---
#'
#' jk_merged <- c(
#'   jk,
#'   join_keys(
#'     join_key("ds4", keys = c("pk4", "pk4_2")),
#'     join_key("ds4", "ds3", c(pk4_2 = "pk3"))
#'   )
#' )
c.join_keys <- function(...) {
  join_keys_obj <- rlang::list2(...)[[1]]
  x <- rlang::list2(...)[-1]
  checkmate::assert_multi_class(join_keys_obj, classes = c("join_keys", "join_key_set"))
  checkmate::assert_list(x, types = c("join_keys", "join_key_set"))

  x_merged <- Reduce(
    init = join_keys(),
    x = x,
    f = function(.x, .y) {
      assert_compatible_keys2(.x, .y)
      utils::modifyList(.x, .y, keep.null = FALSE)
    }
  )

  utils::modifyList(join_keys_obj, x_merged, keep.null = FALSE)
}

#' @rdname join_keys
#' @order 4
#'
#' @export
#'
#' @examples
#'
#' # note: merge can be performed with both join_keys and join_key_set
#'
#' jk_merged <- c(
#'   jk_merged,
#'   join_key("ds5", keys = "pk5"),
#'   join_key("ds5", "ds1", c(pk5 = "pk1"))
#' )
c.join_key_set <- function(...) {
  c.join_keys(...)
}
