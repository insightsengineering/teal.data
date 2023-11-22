testthat::test_that("cdisc_data returns teal_data object for objects different than old api", {
  adsl_raw <- as.data.frame(as.list(stats::setNames(nm = get_cdisc_keys("ADSL"))))
  testthat::expect_s4_class(
    teal_data(adsl = adsl_raw, check = TRUE),
    "teal_data"
  )
})
