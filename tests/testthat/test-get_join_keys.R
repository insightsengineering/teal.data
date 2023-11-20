testthat::test_that("get_join_keys is deprecated", {
  lifecycle::expect_defunct(get_join_keys(join_keys()))
})

testthat::test_that("get_join_keys<- is deprecated", {
  lifecycle::expect_defunct(`get_join_keys<-`(NULL, NULL, NULL, NULL))
})
