#' (test helper) Create test data for `CDISC` data
#'
#' @inheritParams cdisc_data
#'
#' @return a `CDISC` data set with the following tables: `ADSL`, `ADTTE` and `ADAE`
#'
#' @keywords internal
local_cdisc_data_mixed_call <- function(check = TRUE, join_keys1 = join_keys()) {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adae_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))

  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
  adae_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
    }
  )
  adae_cdc <- cdisc_dataset_connector("ADAE", adae_cf, keys = get_cdisc_keys("ADAE"))
  adae_rdc <- cdisc_data_connector(
    connection = data_connection(open_fun = callable_function(function() "open function")),
    connectors = list(adae_cdc)
  )

  load_dataset(adsl)
  load_dataset(adtte)
  load_dataset(adae_cdc)

  cdisc_data(adsl, adtte, adae_rdc, check = check, join_keys = join_keys1)
}
