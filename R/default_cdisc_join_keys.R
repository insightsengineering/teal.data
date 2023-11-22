#' List containing the default `CDISC` join keys
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
