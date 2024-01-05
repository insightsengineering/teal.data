#' Deprecated `TealData` class and related functions
#'
#' @description `r lifecycle::badge("deprecated")`\cr
#' The `TealData` class and associated functions have been deprecated. Use [teal_data()] instead.
#' See the \href{https://github.com/insightsengineering/teal/discussions/945}{Migration guide} for details.
#'
#' @name TealData
#'
#' @param ... any argument supported in `TealData` related functions.
#'
#' @return nothing
#' @seealso  [cdisc_data()] , [join_keys()]
#'
NULL

.deprecate_function <- function(what, details) {
  lifecycle::deprecate_stop(
    when = "0.3.1",
    what = what,
    details = details
  )
}

deprecation_detail <- "Find more information on https://github.com/insightsengineering/teal/discussions/945"

#' @rdname TealData
#' @export
as_cdisc <- function(...) {
  .deprecate_function("as_cdisc()", deprecation_detail)
}

#' @rdname TealData
#' @export
callable_code <- function(...) {
  .deprecate_function("callable_code()", deprecation_detail)
}

#' @rdname TealData
#' @export
callable_function <- function(...) {
  .deprecate_function("callable_function()", deprecation_detail)
}

#' @rdname TealData
#' @export
code_dataset_connector <- function(...) {
  .deprecate_function("code_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
code_cdisc_dataset_connector <- function(...) {
  .deprecate_function("code_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
csv_dataset_connector <- function(...) {
  .deprecate_function("csv_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
csv_cdisc_dataset_connector <- function(...) {
  .deprecate_function("csv_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
python_code <- function(...) {
  .deprecate_function("python_code()", deprecation_detail)
}

#' @rdname TealData
#' @export
python_dataset_connector <- function(...) {
  .deprecate_function("python_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
python_cdisc_dataset_connector <- function(...) {
  .deprecate_function("python_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_data_connector <- function(...) {
  .deprecate_function("cdisc_data_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_dataset <- function(...) {
  .deprecate_function("cdisc_dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_dataset_connector <- function(...) {
  .deprecate_function("cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_dataset_connector_file <- function(...) {
  .deprecate_function("cdisc_dataset_connector_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_dataset_file <- function(...) {
  .deprecate_function("cdisc_dataset_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
dataset <- function(...) {
  .deprecate_function("dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
dataset_connector <- function(...) {
  .deprecate_function("dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
dataset_connector_file <- function(...) {
  .deprecate_function("dataset_connector_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
dataset_file <- function(...) {
  .deprecate_function("dataset_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
data_connection <- function(...) {
  .deprecate_function("data_connection()", deprecation_detail)
}

#' @rdname TealData
#' @export
fun_dataset_connector <- function(...) {
  .deprecate_function("fun_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
fun_cdisc_dataset_connector <- function(...) {
  .deprecate_function("fun_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
relational_data_connector <- function(...) {
  .deprecate_function("relational_data_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
mae_dataset <- function(...) {
  .deprecate_function("mae_dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_attrs <- function(...) {
  .deprecate_function("get_attrs()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_dataset_label <- function(...) {
  .deprecate_function("get_dataset_label()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_dataset <- function(...) {
  .deprecate_function("get_dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_datasets <- function(...) {
  .deprecate_function("get_datasets()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_dataname <- function(...) {
  .deprecate_function("get_dataname()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_key_duplicates <- function(...) {
  .deprecate_function("get_key_duplicates()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_keys <- function(...) {
  .deprecate_function("get_keys()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_raw_data <- function(...) {
  .deprecate_function("get_raw_data()", deprecation_detail)
}

#' @rdname TealData
#' @export
is_pulled <- function(...) {
  .deprecate_function("is_pulled()", deprecation_detail)
}

#' @rdname TealData
#' @export
load_dataset <- function(...) {
  .deprecate_function("load_dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
load_datasets <- function(...) {
  .deprecate_function("load_datasets()", deprecation_detail)
}

#' @rdname TealData
#' @export
mutate_data <- function(...) {
  .deprecate_function("mutate_data()", deprecation_detail)
}

#' @rdname TealData
#' @export
mutate_dataset <- function(...) {
  .deprecate_function("mutate_dataset()", deprecation_detail)
}

#' @rdname TealData
#' @export
set_args <- function(...) {
  .deprecate_function("set_args()", deprecation_detail)
}

#' @rdname TealData
#' @export
rds_dataset_connector <- function(...) {
  .deprecate_function("rds_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
rds_cdisc_dataset_connector <- function(...) {
  .deprecate_function("rds_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
script_dataset_connector <- function(...) {
  .deprecate_function("script_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
script_cdisc_dataset_connector <- function(...) {
  .deprecate_function("script_cdisc_dataset_connector()", deprecation_detail)
}

#' @rdname TealData
#' @export
set_keys <- function(...) {
  .deprecate_function("set_keys()", deprecation_detail)
}

#' @rdname TealData
#' @export
read_script <- function(...) {
  .deprecate_function("read_script()", deprecation_detail)
}

#' @rdname TealData
#' @export
to_relational_data <- function(...) {
  .deprecate_function("to_relational_data()", deprecation_detail)
}

#' @rdname TealData
#' @export
validate_metadata <- function(...) {
  .deprecate_function("validate_metadata()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_cdisc_keys <- function(...) {
  .deprecate_function("get_cdisc_keys()", deprecation_detail)
}

#' @rdname TealData
#' @export
cdisc_data_file <- function(...) {
  .deprecate_function("cdisc_data_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
teal_data_file <- function(...) {
  .deprecate_function("teal_data_file()", deprecation_detail)
}

#' @rdname TealData
#' @export
get_join_keys <- function(...) {
  .deprecate_function("get_join_keys()", "Use `join_keys(data)` instead.")
}

#' @rdname TealData
#' @param value value to assign
#' @export
`get_join_keys<-` <- function(..., value) {
  .deprecate_function("`get_join_keys<-`()", "Use `join_keys(x) <- ...`")
}

#' @rdname col_labels
#' @include formatters_var_labels.R
#' @details
#' `r lifecycle::badge("deprecated")`\cr
#' In previous versions fo `teal.gata` labels were managed with `get_labels()`.
#' This function is deprecated as of `0.3.1`, use `col_labels` instead.
#' @export
get_labels <- function(...) {
  .deprecate_function("get_labels()", "Use col_labels(data)")
}
