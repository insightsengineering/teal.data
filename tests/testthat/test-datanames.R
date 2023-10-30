# get ----
testthat::test_that("datanames returns contents of @datanames slot", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_identical(datanames(td), c("i", "m"))
})

testthat::test_that("variables not in @datanames are omitted", {
  td <- teal_data(i = iris, m = mtcars)
  td <- within(td, f <- faithful)
  testthat::expect_identical(datanames(td), c("i", "m"))
})

# set ----
testthat::test_that("datanames can set value of @datanames", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_identical(datanames(td), c("i", "m"))
  datanames(td) <- "i"
  testthat::expect_identical(datanames(td), "i")
})

testthat::test_that("only names of existing variables are accepted", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_no_error(datanames(td) <- "i")
  testthat::expect_error(datanames(td) <- "f", "invalid name")
})
