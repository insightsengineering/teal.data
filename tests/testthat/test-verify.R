testthat::test_that("verify returns the same object for empty teal_data", {
  tdata1 <- teal_data()
  testthat::expect_identical(tdata1, verify(tdata1))
  testthat::expect_identical(tdata1@verified, TRUE)
})

testthat::test_that("verify returns the same object for teal_data where code was run in within/eval_code", {
  tdata1 <- teal_data()
  tdata1 <- within(tdata1, {
    a <- 1
    b <- a^5
    c <- list(x = 2)
  })
  testthat::expect_identical(verify(tdata1)@verified, TRUE)
  testthat::expect_identical(verify(tdata1)@code, tdata1@code)
  testthat::expect_identical(verify(tdata1)@env, tdata1@env)
})

testthat::test_that("verify returns the same object with changed @verified field for properly executed code", {
  tdata2 <- teal_data(x1 = iris, code = "x1 = iris")
  tdata2_ver <- verify(tdata2)
  testthat::expect_identical(tdata2@verified, FALSE)
  testthat::expect_identical(tdata2_ver@verified, TRUE)
  testthat::expect_identical(tdata2_ver@code, tdata2@code)
  testthat::expect_identical(tdata2_ver@env, tdata2@env)
})

testthat::test_that("verify raises error if @code does not restore objects in @env", {
  tdata3 <- teal_data(x1 = iris, code = "x1 = mtcars")

  testthat::expect_error(verify(tdata3))
})

testthat::test_that("verify returns error for qenv.error input", {
  tdata4 <- teal_data()
  tdata4 <- within(tdata4, {
    stop("error")
  })

  testthat::expect_error(verify(tdata4))
})
