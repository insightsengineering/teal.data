#' @rdname join_keys
#'
#' @details
#' `cdisc_join_keys` is a wrapper around `join_keys` that sets the default
#' join keys for CDISC datasets. It is used internally by `cdisc_data` to
#' set the default join keys for CDISC datasets.
#'
#' @export
#'
#' @examples
#'
#' # Default CDISC join keys
#'
#' cdisc_join_keys(join_key("dataset_A", "dataset_B", c("col_1" = "col_a")), "ADTTE")
cdisc_join_keys <- function(...) {
  data_objects <- rlang::list2(...)

  jk <- join_keys()
  for (ix in seq_along(data_objects)) {
    item <- data_objects[[ix]]
    name <- names(data_objects)[ix]

    if (checkmate::test_class(item, "JoinKeySet")) {
      jk[item$dataset_1, item$dataset_2] <- item$keys
    } else if (
      checkmate::test_multi_class(item, c("TealDataConnector", "TealDataset", "TealDatasetConnector"))
    ) {
    } else {
      if ((is.null(name) || identical(trimws(name), "")) && is.character(item)) {
        name <- item
      }
      if (name %in% names(default_cdisc_keys)) {
        # Set default primary keys
        keys_list <- default_cdisc_keys[[name]]
        jk[name] <- keys_list$primary

        if (!is.null(keys_list$parent) && !is.null(keys_list$foreign)) {
          jk[name, keys_list$parent] <- keys_list$foreign
        }
      }
    }
  }

  jk
}
