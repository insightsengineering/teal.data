#' Variable Labels
#'
#' Get or set variable labels in a `data.frame`
#'
#' Variable labels can be stored as a `label` attribute set on individual variables.
#' These functions get or set this attribute, either on all (`col_labels`) or some variables (`col_relabel`).
#'
#' @param x `data.frame`
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
#' the variable names themselves if `fill = TRUE` and `NA` if `fill = FALSE`.\cr
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
#' @source This function was taken 1-1 from
#' \href{https://cran.r-project.org/package=formatters}{formatters} package, to reduce the complexity of
#' the dependency tree.
#'
#' @rdname col_labels
#' @export
#'
col_labels <- function(x, fill = FALSE) {
  checkmate::assert_data_frame(x)
  checkmate::assert_flag(fill)

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
#' @export
`col_labels<-` <- function(x, value) {
  checkmate::assert_data_frame(x)
  checkmate::assert_character(value)
  checkmate::assert_true(
    ncol(x) == length(value),
    .var.name = "Length of value is equal to the number of columns"
  )

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

#' @rdname col_labels
#' @export
#'
col_relabel <- function(x, ...) {
  checkmate::assert_data_frame(x)
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
