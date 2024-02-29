#' Variable labels
#'
#' Get or set variable labels in a `data.frame`.
#'
#' @details Variable labels can be stored as a `label` attribute set on individual variables.
#' These functions get or set this attribute, either on all (`col_labels`) or some variables (`col_relabel`).
#'
#' @param x (`data.frame` or `DataFrame`) data object
#' @param fill (`logical(1)`) specifying what to return if variable has no label
#' @param value (`character`) vector of variable labels of length equal to number of columns in `x`;
#'  if named, names must match variable names in `x` and will be used as key to set labels;
#'  use `NA` to remove label from variable
#' @param ... name-value pairs, where name corresponds to a variable name in `x`
#'  and value is the new variable label
#'
#' @return
#' For `col_labels`, named character vector of variable labels, the names being the corresponding variable names.
#' If the `label` attribute is missing, the vector elements will be
#' the variable names themselves if `fill = TRUE` and `NA` if `fill = FALSE`.
#'
#' For `col_labels<-` and `col_relabel`, copy of `x` with variable labels modified.
#'
#' @examples
#' x <- iris
#' col_labels(x)
#' col_labels(x) <- paste("label for", names(iris))
#' col_labels(x)
#' y <- col_relabel(x, Sepal.Length = "Sepal Length of iris flower")
#' col_labels(y)
#'
#' @source These functions were taken from
#' [formatters](https://cran.r-project.org/package=formatters) package, to reduce the complexity of
#' the dependency tree and rewritten.
#'
#' @rdname col_labels
#' @export
#'
col_labels <- function(x, fill = FALSE) {
  checkmate::test_multi_class(x, c("data.frame", "DataFrame"))
  checkmate::assert_flag(fill)

  if (ncol(x) == 0L) {
    return(character(0L))
  }

  labels <- sapply(x, function(i) as.vector(attr(i, "label", exact = TRUE)), simplify = FALSE, USE.NAMES = TRUE)
  mapply(
    function(name, label) {
      checkmate::assert_string(
        label,
        .var.name = sprintf("\"label\" attribute of column \"%s\"", name),
        null.ok = TRUE
      )
    },
    name = names(x),
    label = labels
  )

  nulls <- vapply(labels, is.null, logical(1L))
  if (any(nulls)) {
    labels[nulls] <-
      if (fill) {
        colnames(x)[nulls]
      } else {
        NA_character_
      }
  }

  unlist(labels)
}

#' @rdname col_labels
#' @export
`col_labels<-` <- function(x, value) {
  checkmate::test_multi_class(x, c("data.frame", "DataFrame"))
  checkmate::assert_character(value)
  checkmate::assert_true(
    ncol(x) == length(value),
    .var.name = "Length of value is equal to the number of columns"
  )

  varnames <-
    if (is.null(names(value))) {
      names(x)
    } else if (any(names(value) == "")) {
      specified_cols <- names(value)[names(value) != ""]
      checkmate::assert_subset(specified_cols, names(x), .var.name = "names of value")
      res <- names(value)
      res[res == ""] <- setdiff(names(x), specified_cols)
      res
    } else {
      checkmate::assert_set_equal(names(value), names(x), .var.name = "names of value")
      names(value)
    }

  for (i in seq_along(value)) {
    if (is.na(value[i])) {
      attr(x[[varnames[i]]], "label") <- NULL
    } else {
      attr(x[[varnames[i]]], "label") <- value[[i]]
    }
  }
  x
}

#' @rdname col_labels
#' @export
col_relabel <- function(x, ...) {
  checkmate::test_multi_class(x, c("data.frame", "DataFrame"))
  if (missing(...)) {
    return(x)
  }
  value <- list(...)
  varnames <- names(value)

  checkmate::assert_subset(varnames, names(x), .var.name = "names of ...")
  lapply(value, checkmate::assert_string, .var.name = "element of ...", na.ok = TRUE)

  for (i in seq_along(value)) {
    if (is.na(value[i])) {
      attr(x[[varnames[i]]], "label") <- NULL
    } else {
      attr(x[[varnames[i]]], "label") <- value[[i]]
    }
  }
  x
}
