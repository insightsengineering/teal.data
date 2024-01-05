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
#'     join_key("ds3", "ds4", c(pk3 = "pk4_2"))
#'   )
#' )
c.join_keys <- function(...) {
  x <- rlang::list2(...)
  checkmate::assert_list(x, types = c("join_keys", "join_key_set"))

  Reduce(
    init = join_keys(),
    x = x,
    f = function(.x, .y) {
      out <- utils::modifyList(.x, .y, keep.null = FALSE)
      parents(out) <- utils::modifyList(attr(.x, "parents"), attr(.y, "parents"), keep.null = FALSE)
      out
    }
  )
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
#'   join_key("ds1", "ds5", c(pk1 = "pk5"))
#' )
c.join_key_set <- function(...) {
  c.join_keys(...)
}
