#' Compare 2 join keys by ordering names and removing NULL elements
#'
#' Code borrowed from waldo. This should be removed in favor of using testthat 3e
#' that has an option to compare lists as maps.
#'
#' `expect_identical(x, y, list_as_map = TRUE)`
#'
#'
#' @inheritParams testthat::compare
#'
#' @keywords internal
compare.join_keys <- function(x, y, ...) {
  as_map <- function(x) {
    attr(x, "extra_class") <- class(x)
    attr(x, "class") <- "list"

    # Remove nulls
    is_null <- vapply(x, is.null, logical(1))
    x <- x[!is_null]

    # Sort named components, preserving positions of unnamed
    nx <- rlang::names2(x)
    is_named <- nx != ""
    if (any(is_named)) {
      idx <- seq_along(x)
      idx[is_named] <- idx[is_named][order(nx[is_named])]
      x <- x[idx]
    }

    x
  }

  compare(as_map(x), as_map(y))
}

.S3method("compare", "join_keys", compare.join_keys)
