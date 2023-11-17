#' @rdname join_keys
#' @export
#'
#' @examples
#'
#' # Merging multiple `join_keys`
#'
#' jk_merged <- c(jk, join_keys(join_key("dataset_D", "dataset_E", "col_2")))
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
