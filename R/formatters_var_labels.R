#' Function Manages Label Attributes of Variables in a `data.frame`
#'
#' Variable labels can be stored as a `label` attribute for each variable.
#' This functions either get named character vector with the variable labels
#' (empty sting if not specified) or sets all non-missing (non-NA) variable labels in a `data.frame`.
#'
#' @param x `data.frame`
#' @param fill (`logical(1)`) specifying what to return if `label` attribute does not exist
#'
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @seealso [col_relabel()]
#'
#' @return For `col_labels`, named character vector of variable labels, the names being the corresponding variable names.
#' If the `label` attribute is missing, the vector elements will be the variable names themselves
#' if `fill = TRUE` and `NA` if `fill = FALSE`.
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

#' @rdname col_labels
#' @param value (`character`) new variable labels; setting `NA` removes the variable label
#' @return For `col_labels<-`, modifies the variable labels of `data.frame`.(Note that the value of
#' col_labels(x) <- value is that of the assignment, value, not the return value from the left-hand side.)
#'
#' @export
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
#'   `data.frame` and the value to the new variable label
#'
#' @return a copy of `data.frame` with labels changed according to `...`
#'
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @seealso [col_labels()]
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
