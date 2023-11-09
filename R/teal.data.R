#' teal.data: Reproducible data model for `teal` applications
#'
#' This package extends `teal` applications by module which
#' stores the data with they relationships (keys) and reproducible
#' code. Package offers also to load data from files, databases and
#' it's easily extendable by another data sources.
#'
#'
#' @keywords internal
"_PACKAGE"

# Fix R CMD check notes
#' @import shiny
#' @import teal.code
#' @importFrom digest digest
#' @importFrom stats setNames
#' @importFrom shinyjs useShinyjs
#' @importFrom logger log_trace
NULL
