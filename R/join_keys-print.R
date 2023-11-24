#' @rdname join_keys
#' @order 7
#' @export
format.join_keys <- function(x, ...) {
  if (length(x) > 0) {
    my_parents <- parents(x)
    names_sorted <- topological_sort(my_parents)
    names <- union(names_sorted, names(x))
    x_implicit <- update_keys_given_parents(x)
    out <- lapply(names, function(i) {
      this_parent <- my_parents[[i]]
      out_i <- lapply(union(i, names(x[[i]])), function(j) {
        direction <- if (identical(my_parents[[j]], i)) {
          "  <-- "
        } else if (identical(my_parents[[i]], j)) {
          "  --> "
        } else if (!identical(i, j)) {
          "  <-> "
        } else {
          ""
        }

        keys <- x[[i]][[j]]
        sprintf(
          "%s%s: [%s]",
          direction, j,
          if (length(keys) == 0) "no primary keys" else toString(keys)
        )
      })

      implicit_datasets <- setdiff(names(x_implicit[[i]]), names(x[[i]]))
      if (length(implicit_datasets) > 0) {
        out_i <- c(
          out_i,
          paste0(
            "  --* (implicit via parent with): ",
            paste(implicit_datasets, collapse = ", ")
          )
        )
      }

      paste(out_i, collapse = "\n")
    })
    paste(
      c(
        sprintf("A join_keys object containing foreign keys between %s datasets:", length(x)),
        out
      ),
      collapse = "\n"
    )
  } else {
    "An empty join_keys object."
  }
}

#' @rdname join_keys
#' @order 7
#' @export
print.join_keys <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}
