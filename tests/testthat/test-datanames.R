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

# qenv.error support ----
testthat::test_that("datanames supports qenv.error class" ,{
  qe <- within(teal_data(), stop())
  testthat::expect_no_error(datanames(qe))
  testthat::expect_no_error(datanames(qe) <- "name")
})

testthat::test_that("datanames called on qenv.error returns NULL", {
  qe <- within(teal_data(), stop())
  testthat::expect_null(datanames(qe))
})

testthat::test_that("datanames<- called on qenv.error does not change qenv.error", {
  qe <- within(teal_data(), stop())
  qec <- qe
  testthat::expect_identical(qe, qec)
})
