testthat::test_that("cdisc_data returns teal_data object for objects different than old api", {
  adsl_raw <- as.data.frame(as.list(stats::setNames(nm = default_cdisc_join_keys["ADSL", "ADSL"])))
  testthat::expect_s4_class(
    cdisc_data(adsl = adsl_raw, check = FALSE),
    "teal_data"
  )
  testthat::expect_s4_class(
    cdisc_data(i = iris, code = "i <- iris", check = TRUE),
    "teal_data"
  )
})
