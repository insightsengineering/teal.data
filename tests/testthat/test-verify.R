testthat::test_that("verify returns the same object for @verified=TRUE", {
  tdata <- teal_data()
  testthat::expect_identical(tdata, verify(tdata))

  tdata1 <- teal_data()
  tdata1 <- within(tdata1, {
    a <- 1
    b <- a^5
    c <- list(x = 2)
  })
  testthat::expect_identical(verify(tdata1), tdata1)
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

  testthat::expect_error(verify(tdata3), "Code verification failed.")
})

testthat::test_that("verify returns error for qenv.error input", {
  tdata4 <- teal_data()
  tdata4 <- within(tdata4, {
    stop("error")
  })

  testthat::expect_error(verify(tdata4))
})


testthat::test_that("not verified warning is not added if it already exists in @code", {
  x <- teal_data(iris = iris)
  testthat::expect_identical(
    get_code(x),
    "warning('Code was not verified for reproducibility.')"
  )
  x@code <- get_code(x)
  testthat::expect_identical(
    get_code(x),
    "warning('Code was not verified for reproducibility.')"
  )
})
