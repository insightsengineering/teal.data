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

  # Ensure base object has correct class when called from c.join_key_set
  join_keys_obj <- join_keys(join_keys_obj)

  x_merged <- Reduce(
    init = join_keys(),
    x = x,
    f = function(.x, .y) {
      assert_compatible_keys2(.x, .y)
      out <- utils::modifyList(.x, .y, keep.null = FALSE)
      attr(out, "__parents__") <- .merge_parents(.x, .y)
      out
    }
  )

  out <- utils::modifyList(join_keys_obj, x_merged, keep.null = FALSE)
  attr(out, "__parents__") <- .merge_parents(join_keys_obj, x_merged)
  out
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

#' Merge parents for 2 `join_keys` object
#'
#' @param x,y (`join_keys`) objects to merge their parents
#'
#' @return a list with parents merged from 2 `join_keys`. Not the object itself.
#' @keywords internal
.merge_parents <- function(x, y) {
  x_parent <- list()
  y_parent <- list()
  if (length(attr(x, "__parents__"))) {
    x_parent <- attr(x, "__parents__")
  }
  if (length(attr(y, "__parents__"))) {
    y_parent <- attr(y, "__parents__")
  }
  utils::modifyList(x_parent, y_parent, keep.null = FALSE)
}
