#' Get dataset label attribute
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param data \code{data.frame} from which attribute is extracted
#'
#' @return (\code{character}) label or \code{NULL} if it is missing
#'
#' @export
#'
#' @examples
#' data_label(example_cdisc_data("ADSL"))
data_label <- function(data) {
  attr(data, "label")
}

#' Set dataset label attribute
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x \code{data.frame} for which attribute is set
#' @param value (\code{character}) label
#'
#' @return modified \code{x} object
#'
#' @export
#'
#' @examples
#' x <- example_cdisc_data("ADSL")
#' data_label(x) <- "My custom label"
#' data_label(x)
`data_label<-` <- function(x, value) { # nolint
  stopifnot(is.data.frame(x))
  checkmate::assert_string(value)

  attr(x, "label") <- value
  x
}

#' Function that returns the default keys for a `CDISC` dataset by name
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param dataname name of the `CDISC` dataset
#'
#' @return \code{keys} object
#'
#' @export
#'
#' @examples
#' get_cdisc_keys("ADSL")
get_cdisc_keys <- function(dataname) {
  checkmate::assert_string(dataname)

  if (!(dataname %in% names(default_cdisc_keys))) {
    stop(paste(sprintf("get_cdisc_keys does not support datasets called %s", dataname),
      "  Please specify the keys directly, for example:",
      sprintf(
        "  cdisc_dataset(dataname = \"%s\", keys = c(\"STUDYID\", \"USUBJID\", ...), parent = \"ADSL\", ...)",
        dataname
      ),
      sep = "\n"
    ))
  } else {
    cdisc_keys <- default_cdisc_keys[[dataname]]$primary

    return(cdisc_keys)
  }
}

#' Extracts dataset and variable labels from a dataset.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param data (`data.frame`) table to extract the labels from
#' @param fill (`logical(1)`) if `TRUE`, the function will return variable names for columns with non-existent labels;
#'   otherwise will return `NA` for them
#'
#' @return `list` with two keys: `dataset_labels` and `column_labels`
#'
#' @export
#'
#' @examples
#' iris_with_labels <- iris
#' attr(iris_with_labels, "label") <- "Custom iris dataset with labels"
#' attr(iris_with_labels["Sepal.Length"], "label") <- c(`Sepal.Length` = "Sepal Length")
#' get_labels(iris_with_labels, fill = TRUE)
#' get_labels(iris_with_labels, fill = FALSE)
get_labels <- function(data, fill = TRUE) {
  stopifnot(is.data.frame(data))
  checkmate::assert_flag(fill)

  column_labels <- Map(function(col, colname) {
    label <- attr(col, "label")
    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!checkmate::test_string(label, na.ok = TRUE)) {
        stop("label for variable ", colname, " is not a character string")
      }
      as.vector(label) # because label might be a named vector
    }
  }, data, colnames(data))
  column_labels <- unlist(column_labels, recursive = FALSE, use.names = TRUE)

  list("dataset_label" = data_label(data), "column_labels" = column_labels)
}
