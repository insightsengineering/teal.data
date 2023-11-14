test_that("default_cdisc_join_keys is assigned in package environment", {
  expect_true(exists("default_cdisc_join_keys"))
  expect_gt(length(default_cdisc_join_keys), 0)
})

test_that("default_cdisc_join_keys subsetting of datasets with parent is valid", {
  # indirect test to build_cdisc_join_keys
  ds <- c("ADTTE", "ADSL")
  result <- default_cdisc_join_keys[ds]
  expect_length(result, 2)
  expect_length(parents(result), 1)
})

test_that("default_cdisc_join_keys subsetting of dataset without parent contains parent", {
  # indirect test to build_cdisc_join_keys
  ds <- c("ADTTE", "ADEX", "ADRS")
  result <- default_cdisc_join_keys[ds]
  expect_length(result, 4)
  expect_length(parents(result), 3)
})
