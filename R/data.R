#' Simulated `CDISC` Data for Examples
#'
#' @description
#' This package contains random datasets that are used for testing purposes in the `teal.data` package:
#'
#' - `rADSL` Random subject-level analysis dataset
#'
#' @name random_cdisc_data
#' @docType data
#' @keywords datasets
#' @source - `rADSL`: [random.cdisc.data::cadsl]
# nolint start: line_length.
#' @format - `rADSL`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADSL)` rows and `r ncol(rADSL)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADSL"

#' @description - `rADAE`: Random adverse events analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADAE`: [random.cdisc.data::cadae]
# nolint start: line_length.
#' @format - `rADAE`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADAE)` rows and `r ncol(rADAE)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADAE"

#' @description - `rADCM`: Random concomitant medications analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADCM`: [random.cdisc.data::cadcm]
# nolint start: line_length.
#' @format - `rADCM`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADCM)` rows and `r ncol(rADCM)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADCM"

#' @description - `rADEX`: Random response analysis dataset
#' @docType data
#' @keywords  datasets
#' @source - `rADEX`: [random.cdisc.data::cadex]
# nolint start: line_length.
#' @format - `rADEX`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADEX)` rows and `r ncol(rADEX)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADEX"

#' @description - `rADLB`: Random laboratory data analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADLB`: [random.cdisc.data::cadlb]
# nolint start: line_length.
#' @format - `rADLB`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADLB)` rows and `r ncol(rADLB)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADLB"

#' @description - `rADRS`: Random response analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADRS`: [random.cdisc.data::cadrs]
# nolint start: line_length.
#' @format - `rADRS`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADRS)` rows and `r ncol(rADRS)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADRS"

#' @description - `rADTR`: Random tumor response analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADTR`: [random.cdisc.data::cadtr]
# nolint start: line_length.
#' @format - `rADTR`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADTR)` rows and `r ncol(rADTR)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADTR"

#' @description - `rADTTE`: Random time to event analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADTTE`: [random.cdisc.data::cadtte]
# nolint start: line_length.
#' @format - `rADTTE`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADTTE)` rows and `r ncol(rADTTE)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADTTE"

#' @description - `rADVS`: Random vital signs analysis dataset
#' @docType data
#' @keywords datasets
#' @source - `rADVS`: [random.cdisc.data::cadvs]
# nolint start: line_length.
#' @format - `rADVS`: An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with `r nrow(rADVS)` rows and `r ncol(rADVS)` columns.
# nolint end: line_length.
#' @rdname random_cdisc_data
"rADVS"

#' List containing default joining keys for `CDISC` datasets
#'
#' These represent primary and foreign keys for joining `CDISC` datasets.
#'
#' @docType data
#' @source internal. See `data-raw/cdisc_datasets.yaml` and `data-raw/data.R` for details.
#' @seealso [join_keys()]
#' @examples
#' default_cdisc_join_keys
#' default_cdisc_join_keys[c("ADSL", "ADAE")] # Subset for ADSL and ADAE
"default_cdisc_join_keys"
