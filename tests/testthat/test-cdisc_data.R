testthat::test_that("cdisc_data returns teal_data object with default join_keys for given dataset", {
  data <- cdisc_data(ADSL = data.frame())
  testthat::expect_s4_class(data, "teal_data")
  testthat::expect_identical(
    join_keys(data),
    default_cdisc_join_keys["ADSL"]
  )
})
