#' List containing the default `CDISC` datasets `join_keys` objects
#'
#' @details
#' This data object is created at loading time from `cdisc_datasets/cdisc_datasets.yaml`.
#'
#' @name default_cdisc_join_keys
#' @docType data
#' @export
NULL

#' Helper method to build `default_cdisc_join_keys`
#' @param default_cdisc_keys (`list`) default definition of primary and foreign
#' keys for `CDISC` datasets
#'
#' @keywords internal
build_cdisc_join_keys <- function(default_cdisc_keys) {
  checkmate::assert_list(default_cdisc_keys, types = "list")

  jk <- join_keys()
  for (name in names(default_cdisc_keys)) {
    # Set default primary keys
    keys_list <- default_cdisc_keys[[name]]

    if (!is.null(keys_list[["primary"]])) {
      jk[[name]][[name]] <- keys_list[["primary"]]
    }

    if (!is.null(keys_list[["parent"]])) {
      if (!is.null(keys_list[["foreign"]])) {
        jk[[name]][[keys_list[["parent"]]]] <- keys_list[["foreign"]]
      }
      parents(jk)[[name]] <- keys_list[["parent"]]
    }
  }
  jk
}
