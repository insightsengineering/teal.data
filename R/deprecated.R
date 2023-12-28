#' Deprecated `TealData` class and related functions
#'
#' @description `r lifecycle::badge("deprecated")`\cr
#' This function has been deprecated. Please see [teal_data()] and [cdisc_data()] instead.
#' Find more information on \href{https://github.com/insightsengineering/teal/discussions/945}{Migration guide}.
#'
#' @name TealData
#'
#' @param ... any argument supported in `TealData` related functions.
#'
#' @return nothing
#'
NULL

.deprecate_teal_data <- function(what) {
  lifecycle::deprecate_stop(
    when = "0.3.1",
    what = what,
    details = "Find more information on https://github.com/insightsengineering/teal/discussions/945"
  )
}

#' @rdname TealData
#' @export
as_cdisc <- function(...) {
  .deprecate_teal_data("as_cdisc()")
}

#' @rdname TealData
#' @export
callable_code <- function(...) {
  .deprecate_teal_data("callable_code()")
}

#' @rdname TealData
#' @export
callable_function <- function(...) {
  .deprecate_teal_data("callable_function()")
}

#' @rdname TealData
#' @export
code_dataset_connector <- function(...) {
  .deprecate_teal_data("code_dataset_connector()")
}

#' @rdname TealData
#' @export
code_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("code_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
csv_dataset_connector <- function(...) {
  .deprecate_teal_data("csv_dataset_connector()")
}

#' @rdname TealData
#' @export
csv_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("csv_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
python_code <- function(...) {
  .deprecate_teal_data("python_code()")
}

#' @rdname TealData
#' @export
python_dataset_connector <- function(...) {
  .deprecate_teal_data("python_dataset_connector()")
}

#' @rdname TealData
#' @export
python_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("python_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
cdisc_data_connector <- function(...) {
  .deprecate_teal_data("cdisc_data_connector()")
}

#' @rdname TealData
#' @export
cdisc_dataset <- function(...) {
  .deprecate_teal_data("cdisc_dataset()")
}

#' @rdname TealData
#' @export
cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
cdisc_dataset_connector_file <- function(...) {
  .deprecate_teal_data("cdisc_dataset_connector_file()")
}

#' @rdname TealData
#' @export
cdisc_dataset_file <- function(...) {
  .deprecate_teal_data("cdisc_dataset_file()")
}

#' @rdname TealData
#' @export
dataset <- function(...) {
  .deprecate_teal_data("dataset()")
}

#' @rdname TealData
#' @export
dataset_connector <- function(...) {
  .deprecate_teal_data("dataset_connector()")
}

#' @rdname TealData
#' @export
dataset_connector_file <- function(...) {
  .deprecate_teal_data("dataset_connector_file()")
}

#' @rdname TealData
#' @export
dataset_file <- function(...) {
  .deprecate_teal_data("dataset_file()")
}

#' @rdname TealData
#' @export
data_connection <- function(...) {
  .deprecate_teal_data("data_connection()")
}

#' @rdname TealData
#' @export
fun_dataset_connector <- function(...) {
  .deprecate_teal_data("fun_dataset_connector()")
}

#' @rdname TealData
#' @export
fun_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("fun_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
relational_data_connector <- function(...) {
  .deprecate_teal_data("relational_data_connector()")
}

#' @rdname TealData
#' @export
mae_dataset <- function(...) {
  .deprecate_teal_data("mae_dataset()")
}

#' @rdname TealData
#' @export
get_attrs <- function(...) {
  .deprecate_teal_data("get_attrs()")
}

#' @rdname TealData
#' @export
get_dataset_label <- function(...) {
  .deprecate_teal_data("get_dataset_label()")
}

#' @rdname TealData
#' @export
get_dataset <- function(...) {
  .deprecate_teal_data("get_dataset()")
}

#' @rdname TealData
#' @export
get_datasets <- function(...) {
  .deprecate_teal_data("get_datasets()")
}

#' @rdname TealData
#' @export
get_dataname <- function(...) {
  .deprecate_teal_data("get_dataname()")
}

#' @rdname TealData
#' @export
get_key_duplicates <- function(...) {
  .deprecate_teal_data("get_key_duplicates()")
}

#' @rdname TealData
#' @export
get_keys <- function(...) {
  .deprecate_teal_data("get_keys()")
}

#' @rdname TealData
#' @export
get_raw_data <- function(...) {
  .deprecate_teal_data("get_raw_data()")
}

#' @rdname TealData
#' @export
is_pulled <- function(...) {
  .deprecate_teal_data("is_pulled()")
}

#' @rdname TealData
#' @export
load_dataset <- function(...) {
  .deprecate_teal_data("load_dataset()")
}

#' @rdname TealData
#' @export
load_datasets <- function(...) {
  .deprecate_teal_data("load_datasets()")
}

#' @rdname TealData
#' @export
mutate_data <- function(...) {
  .deprecate_teal_data("mutate_data()")
}

#' @rdname TealData
#' @export
mutate_dataset <- function(...) {
  .deprecate_teal_data("mutate_dataset()")
}

#' @rdname TealData
#' @export
set_args <- function(...) {
  .deprecate_teal_data("set_args()")
}

#' @rdname TealData
#' @export
rds_dataset_connector <- function(...) {
  .deprecate_teal_data("rds_dataset_connector()")
}

#' @rdname TealData
#' @export
rds_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("rds_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
script_dataset_connector <- function(...) {
  .deprecate_teal_data("script_dataset_connector()")
}

#' @rdname TealData
#' @export
script_cdisc_dataset_connector <- function(...) {
  .deprecate_teal_data("script_cdisc_dataset_connector()")
}

#' @rdname TealData
#' @export
set_keys <- function(...) {
  .deprecate_teal_data("set_keys()")
}

#' @rdname TealData
#' @export
read_script <- function(...) {
  .deprecate_teal_data("read_script()")
}

#' @rdname TealData
#' @export
to_relational_data <- function(...) {
  .deprecate_teal_data("to_relational_data()")
}

#' @rdname TealData
#' @export
validate_metadata <- function(...) {
  .deprecate_teal_data("validate_metadata()")
}

#' @rdname TealData
#' @export
get_cdisc_keys <- function(...) {
  .deprecate_teal_data("get_cdisc_keys()")
}

#' @rdname TealData
#' @export
cdisc_data_file <- function(...) {
  .deprecate_teal_data("cdisc_data_file()")
}

#' @rdname TealData
#' @export
teal_data_file <- function(...) {
  .deprecate_teal_data("teal_data_file()")
}

#' Function to get join keys from a `` object
#' @param data `` - object to extract the join keys
#' @return Either `join_keys` object or `NULL` if no join keys
#' @export
get_join_keys <- function(data) {
  lifecycle::deprecate_stop(
    when = " 0.3.1",
    what = "get_join_keys(data)",
    details = "Use `join_keys(data)` instead."
  )
}

#' @rdname get_join_keys
#' @inheritParams join_key
#' @param value value to assign
#' @export
`get_join_keys<-` <- function(data, dataset_1, dataset_2 = NULL, value) {
  lifecycle::deprecate_stop(
    when = " 0.3.1",
    what = "`get_join_keys<-`()",
    details = "Use `join_keys(x) <- ...`"
  )
}

#' @rdname col_labels
#' @param data a `data.frame` object
#' @param fill (`logical(1)`) boolean in case the `label` attribute does not exist if
#'   `TRUE` the variable names is returned, otherwise `NA`.Default is `FALSE`.
#' @export
get_labels <- function(data, fill = TRUE) {
  lifecycle::deprecate_stop(
    when = " 0.3.1",
    what = "get_labels(data)",
    details = "Use col_labels(data)"
  )
}
