#' Data input for teal app
#'
#' @description `r lifecycle::badge("stable")`
#' Function takes datasets and creates `CDISCTealData` object which can be used in `teal` applications.
#'
#' @note This function does not automatically assign keys to `TealDataset`
#' and `TealDatasetConnector` objects passed to it. If the keys are needed
#' they should be assigned before calling `cdisc_data`. See example:
#' ```
# library(scda)
# test_dataset <- dataset("ADAE", synthetic_cdisc_data("latest")$adae) # does not have keys
# test_adsl <- cdisc_dataset("ADSL", synthetic_cdisc_data("latest")$adsl)
# test_data <- cdisc_data(test_dataset, test_adsl)
# get_keys(test_data, "ADAE") # returns character(0)
#
# test_dataset <- cdisc_dataset("ADAE", synthetic_cdisc_data("latest")$adae)
# test_data <- cdisc_data(test_dataset, test_adsl)
# get_keys(test_data, "ADAE") # returns [1] "STUDYID" "USUBJID" "ASTDTM"  "AETERM"  "AESEQ"
#' ```
#' @inheritParams teal_data
#' @param ... (`TealDataConnector`, `TealDataset` or
#'   `TealDatasetConnector`) elements to include where `ADSL` data is mandatory.
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with datasets column names used for joining.
#'   If empty then it would be automatically derived basing on intersection of datasets primary keys
#'
#' @return a `CDISCTealData` object
#'
#' @details This function checks if there were keys added to all data sets
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' latest_data <- synthetic_cdisc_data("latest")
#' ADSL <- latest_data$adsl
#' ADTTE <- latest_data$adtte
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte',
#'   check = TRUE
#' )
#'
#' # Example with keys
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL,
#'     keys = c("STUDYID", "USUBJID")
#'   ),
#'   cdisc_dataset("ADTTE", ADTTE,
#'     keys = c("STUDYID", "USUBJID", "PARAMCD"),
#'     parent = "ADSL"
#'   ),
#'   join_keys = join_keys(
#'     join_key(
#'       "ADSL",
#'       "ADTTE",
#'       c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID")
#'     )
#'   ),
#'   code = 'ADSL <- synthetic_cdisc_data("latest")$adsl
#'           ADTTE <- synthetic_cdisc_data("latest")$adtte',
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       join_keys = teal.data::join_keys(),
                       code = "",
                       check = FALSE) {
  data_objects <- list(...)
  checkmate::assert_list(
    data_objects,
    types = c("CDISCTealDataset", "CDISCTealDatasetConnector", "CDISCTealDataConnector")
  )
  if (inherits(join_keys, "JoinKeySet")) {
    join_keys <- teal.data::join_keys(join_keys)
  }
# browser()
#   # set join_keys if they are not passed by the user
#   join_keys_sets <- lapply(data_objects, function(obj) {
#     join_key(
#       obj$get_dataname(),
#       obj$get_dataname(),
#       obj$get_keys()
#     )
#   })

    # join_keys_sets <- lapply(data_objects, function(obj) {
    #   load_datasets(obj)
    #   lapply(obj$get_datanames(), function(sub_obj) {
    #
    #     print(obj$get_dataset(sub_obj))
    #     join_key(
    #       sub_obj,
    #       sub_obj,
    #       obj$get_dataset[[sub_obj]]$get_keys()
    #     )
    #   })
    # })

  #same check in set
  if (length(join_keys$get()) == 0) join_keys$set(join_keys_sets)

  # set parents
  retrieve_parents <- function(x) {
    tryCatch(
      parents <- x$get_parent(),
    error = function(cond) rep(character(0), length(x$get_datanames()))
  )}
  new_parents <- lapply(data_objects, retrieve_parents)
  names(new_parents) <- sapply(data_objects, function(x) x$get_datanames())

  if (is_dag(new_parents)) {
    stop("Cycle detected in a parent and child dataset graph.")
  }
  join_keys$set_parents(new_parents)
  join_keys$update_keys_given_parents()

  # initialize TealData
  x <- TealData$new(..., check = check, join_keys = join_keys)

  # set parents if they are not passed by the user
  # if (is.null(join_keys$get_parents())) {
  #   new_parent <- create_parents(data_objects)
  #   if (is_dag(new_parent)) {
  #     stop("Cycle detected in a parent and child dataset graph.")
  #   }
  #   join_keys$set_parents(new_parent)
  #
  #   # set up join keys as parent keys
  #   #join_keys$update_keys_given_parents(x)
  # }

  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }

  x$check_reproducibility()
  x$check_metadata()
  return(x)
}

#' Load `CDISCTealData` object from a file
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @inheritParams teal_data_file
#'
#' @return `CDISCTealData` object
#'
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(scda)
#'
#'      # code>
#'      ADSL <- synthetic_cdisc_data('latest')$adsl
#'      ADTTE <- synthetic_cdisc_data('latest')$adtte
#'
#'      cdisc_data(
#'           cdisc_dataset(\"ADSL\", ADSL), cdisc_dataset(\"ADTTE\", ADTTE),
#'           code = \"ADSL <- synthetic_cdisc_data('latest')$adsl
#'                   ADTTE <- synthetic_cdisc_data('latest')$adtte\",
#'           check = FALSE
#'      )
#'      # <code"
#'   ),
#'   con = file_example
#' )
#'
#' cdisc_data_file(file_example)
cdisc_data_file <- function(path, code = get_code(path)) {
  object <- object_file(path, "CDISCTealData")
  object$mutate(code)
  return(object)
}

##
# create_parents = function(data_objects) {
#   new_parent <- list()
#   for (x in data_objects) {
#     if (inherits(x, "TealDataset") ||
#         inherits(x, "TealDatasetConnector")) {
#       x_dataname <- x$get_dataname()
#       new_parent[[x_dataname]] <-
#         if (inherits(x, "CDISCTealDataset") ||
#             inherits(x, "CDISCTealDatasetConnector")) {
#           x$get_parent()
#         } else {
#           character(0L)
#         }
#     } else if (inherits(x, "TealDataConnector")) {
#       added_parent <- if (inherits(x, "CDISCTealDataConnector")) {
#         x$get_parent()
#       } else {
#         sapply(x$get_datanames(), function(i)
#           character(0), USE.NAMES = TRUE, simplify = FALSE)
#       }
#       new_parent <- c(new_parent, added_parent)
#     } else {
#       stop(
#         paste(
#           "The child elements of CDISCTealData should be only of TealDataset or TealDatasetConnector or",
#           "TealDataConnector class."
#         )
#       )
#     }
#   }
#   new_parent
# }
