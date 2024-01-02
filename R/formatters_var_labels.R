#' Get Label Attributes of Variables in a `data.frame`
#'
#' Variable labels can be stored as a `label` attribute for each variable.
#' This functions returns a named character vector with the variable labels
#' (empty sting if not specified)
#'
#' @param x a `data.frame` object
#' @param fill (`logical(1)`) boolean in case the `label` attribute does not exist if
#'   `TRUE` the variable names is returned, otherwise `NA`.Default is `FALSE`.
#'
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @seealso [col_relabel()] [`col_labels<-`]
#'
#' @return a named character vector with the variable labels, the names
#'   correspond to the variable names
#'
#' @export
#'
#' @examples
#' x <- iris
#' col_labels(x)
#' col_labels(x) <- paste("label for", names(iris))
#' col_labels(x)
col_labels <- function(x, fill = FALSE) {
  checkmate::assert_character(colnames(x), any.missing = FALSE)
  if (NCOL(x) == 0) {
    return(character())
  }

  y <- Map(function(col, colname) {
    label <- attr(col, "label")

    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!is.character(label) && !(length(label) == 1)) {
        stop("label for variable ", colname, "is not a character string")
      }
      as.vector(label)
    }
  }, x, colnames(x))

  labels <- unlist(y, recursive = FALSE, use.names = TRUE)

  if (!is.character(labels)) {
    stop("label extraction failed")
  }

  labels
}

#' Set Label Attributes of All Variables in a `data.frame`
#'
#' Variable labels can be stored as a `label` attribute for each variable.
#' This function sets all non-missing (non-NA) variable labels in a `data.frame`.
#'
#' @inheritParams col_labels
#' @param value new variable labels, `NA` removes the variable label
#'
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @seealso [col_labels()] [col_relabel()]
#'
#' @return modifies the variable labels of `x`
#'
#' @export
#'
#' @examples
#' x <- iris
#' col_labels(x)
#' col_labels(x) <- paste("label for", names(iris))
#' col_labels(x)
#'
#' if (interactive()) {
#'   View(x) # in RStudio data viewer labels are displayed
#' }
`col_labels<-` <- function(x, value) {
  checkmate::assert_character(colnames(x), any.missing = FALSE)
  checkmate::assert_character(value, any.missing = FALSE)
  checkmate::assert_true(ncol(x) == length(value))

  theseq <- if (!is.null(names(value))) names(value) else seq_along(x)
  # across columns of x
  for (j in theseq) {
    attr(x[[j]], "label") <- if (!is.na(value[j])) {
      value[j]
    } else {
      NULL
    }
  }

  x
}

#' Copy and Change Variable Labels of a `data.frame`
#'
#' Relabel a subset of the variables
#'
#' @inheritParams col_labels<-
#' @param ... name-value pairs, where name corresponds to a variable name in
#'   `x` and the value to the new variable label
#'
#' @return a copy of `x` with labels changed according to `...`
#'
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @seealso [col_labels()] [`col_labels<-`]
#'
#' @export
#'
#' @examples
#' x <- col_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' col_labels(x)
#'
col_relabel <- function(x, ...) {
  checkmate::assert_character(colnames(x), any.missing = FALSE)
  if (missing(...)) {
    return(x)
  }
  dots <- list(...)
  varnames <- names(dots)
  checkmate::assert_character(varnames, null.ok = FALSE)

  map_varnames <- match(varnames, colnames(x))

  if (any(is.na(map_varnames))) {
    stop("variables: ", paste(varnames[is.na(map_varnames)], collapse = ", "), " not found")
  }

  if (any(vapply(dots, Negate(is.character), logical(1)))) {
    stop("all variable labels must be of type character")
  }

  for (i in seq_along(map_varnames)) {
    attr(x[[map_varnames[[i]]]], "label") <- dots[[i]]
  }

  x
}
