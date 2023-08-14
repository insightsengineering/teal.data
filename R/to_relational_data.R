#' S3 generic for `to_relational_data` function.
#'
#' This function takes an object and converts into a `TealData` object, the primary data
#' object for use in teal applications.
#'
#' @param data `TealDataset`, `TealDatasetConnector`, `data.frame`, `MultiAssayExperiment`,  `list`
#' or `function` returning a named list.
#'
#' @details Passing a `TealData` into this function leaves the object unchanged.
#'
#' @return `TealData` object
#'
#' @examples
#'
#' to_relational_data(head(iris))
#' to_relational_data(dataset("IRIS", head(iris)))
#' to_relational_data(list(iris = head(iris), mtcars = head(mtcars)))
#'
#' d_connector <- dataset_connector("iris", callable_function(function() head(iris)))
#' d_connector$pull()
#' to_relational_data(d_connector)
#'
#' @keywords internal
#' @export
to_relational_data <- function(data) {
  UseMethod("to_relational_data")
}

#' @keywords internal
#' @export
to_relational_data.data.frame <- function(data) { # nolint
  dataname <- deparse(substitute(data, parent.frame()), width.cutoff = 500L)

  if (grepl("\\)$", dataname) && inherits(data, "data.frame")) {
    stop("Single data.frame shouldn't be provided as a result of a function call. Please name
         the object first or use a named list.")
  }

  if (dataname %in% names(default_cdisc_keys)) {
    cdisc_data(cdisc_dataset(dataname, data))
  } else {
    teal_data(dataset(dataname, data))
  }
}

#' @keywords internal
#' @export
to_relational_data.TealDataset <- function(data) {
  dataname <- get_dataname(data)

  if (dataname %in% names(default_cdisc_keys)) {
    cdisc_data(data)
  } else {
    teal_data(data)
  }
}

#' @keywords internal
#' @export
to_relational_data.TealDatasetConnector <- function(data) { # nolint
  to_relational_data.TealDataset(data)
}

#' @keywords internal
#' @export
to_relational_data.list <- function(data) {
  checkmate::assert_list(
    data,
    types = c("dataset", "data.frame", "MultiAssayExperiment", "TealDataset", "TealDatasetConnector")
  )

  call <- substitute(data, parent.frame())
  list_names <- names(data)
  parsed_names <- as.character(call)[-1]

  if (
    (
      length(list_names) == 0 &&
        length(parsed_names) == 0 &&
        any(sapply(data, inherits, c("dataset", "data.frame", "MultiAssayExperiment")))
    ) ||
      (any(list_names == "") && length(parsed_names) == 0) ||
      (any(is.na(list_names)))
  ) {
    stop("Unnamed lists shouldn't be provided as input for data. Please use a named list.")
  }

  datasets_list <- lapply(
    seq_along(data),
    function(idx) {
      if (is.data.frame(data[[idx]]) || inherits(data[[idx]], "MultiAssayExperiment")) {
        dataname <- if (length(list_names) == 0 || list_names[[idx]] == "") {
          parsed_names[[idx]]
        } else {
          list_names[[idx]]
        }

        if (dataname %in% names(default_cdisc_keys)) {
          cdisc_dataset(dataname, data[[idx]])
        } else {
          dataset(dataname, data[[idx]])
        }
      } else if (inherits(data[[idx]], "TealDataset") || inherits(data[[idx]], "TealDatasetConnector")) {
        data[[idx]]
      } else {
        stop("Unknown class to create TealDataset from.")
      }
    }
  )

  if (any(sapply(datasets_list, function(x) inherits(x, "CDISCTealDataset")))) {
    do.call("cdisc_data", args = datasets_list)
  } else {
    do.call("teal_data", args = datasets_list)
  }
}

#' @keywords internal
#' @export
to_relational_data.MultiAssayExperiment <- function(data) { # nolint
  teal_data(dataset("MAE", data))
}

#' @keywords internal
#' @export
to_relational_data.TealData <- function(data) { # nolint
  data
}

#' @keywords internal
#' @export
to_relational_data.tdata <- function(data) {
  data
}
