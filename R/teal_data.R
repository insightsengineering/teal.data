#' Teal data
#'
#' @description `r lifecycle::badge("stable")`
#' Universal function to pass data to teal application
#'
#' @param ... (`TealDataConnector`, `TealDataset`, `TealDatasetConnector`)\cr
#'   objects
#' @param join_keys (`JoinKeys`) or a single (`JoinKeySet`)\cr
#'   (optional) object with dataset column relationships used for joining.
#'   If empty then no joins between pairs of objects
#' @param code (`character`) code to reproduce the datasets.
#' @param check (`logical`) reproducibility check - whether to perform a check that the pre-processing
#'  code included in the object definitions actually produces those objects.
#'  If `check` is true and preprocessing code is empty an error will be thrown.
#'
#' @return (`TealData`)
#'
#' @export
#'
#' @examples
#' x1 <- dataset(
#'   "x1",
#'   iris,
#'   code = "x1 <- iris"
#' )
#'
#' x2 <- dataset(
#'   "x2",
#'   mtcars,
#'   code = "x2 <- mtcars"
#' )
#'
#' teal_data(x1, x2)
teal_data <- function(...,
                      join_keys = teal.data::join_keys(),
                      code = "",
                      check = FALSE) {
                         x$check_reproducibility()
  # check_reproducibility()
  # check_metadata
  new_tdata(
    env = list(...),
    code = code,
    join_keys = join_keys
  )
}

