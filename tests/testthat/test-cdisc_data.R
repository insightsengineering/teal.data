testthat::test_that("cdisc_data returns teal_data object for objects different than old api", {
  adsl_raw <- as.data.frame(as.list(stats::setNames(nm = default_cdisc_join_keys["ADSL", "ADSL"])))
  testthat::expect_s4_class(
    cdisc_data(ADSL = adsl_raw),
    "teal_data"
  )
})
