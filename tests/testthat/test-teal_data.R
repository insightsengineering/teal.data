testthat::test_that("teal_data allows to initialize empty teal_data object", {
  testthat::expect_s4_class(teal_data(), "teal_data")
})

testthat::test_that("empty teal_data returns empty code and verified=TRUE", {
  testthat::expect_identical(teal_data()@code, list(character(0)))
  testthat::expect_identical(teal_data()@verified, TRUE)
})

testthat::test_that("non-empty data in teal_data returns verified=FALSE", {
  testthat::expect_identical(teal_data(IRIS = iris)@verified, FALSE)
})

testthat::test_that("teal_data allows to initialize empty teal_data with join_keys", {
  testthat::expect_equal(
    teal_data(join_keys = join_keys(join_key("data1", "data2", "id")))@join_keys,
    join_keys(join_key("data1", "data2", "id"))
  )
})

testthat::test_that(
  "teal_data initializes teal_data object without names taken from join_keys if objects did not exist in env",
  {
    testthat::expect_identical(
      names(teal_data(join_keys = join_keys(join_key("parent", "child", "id")))),
      character(0)
    )
  }
)

testthat::test_that(
  "teal_data initializes teal_data object with names taken only from passed objects and not join_keys",
  {
    testthat::expect_identical(
      names(teal_data(iris = iris, join_keys = join_keys(join_key("parent", "child", "id")))),
      "iris"
    )
  }
)

testthat::test_that("teal_data returns teal_data when data passed as named list", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  testthat::expect_s4_class(teal_data(df1 = df1), "teal_data")
})

testthat::test_that("teal_data accepts any data provided as named list", {
  df1 <- structure(1L, class = "anyclass")
  testthat::expect_no_error(teal_data(df1 = df1))
})

testthat::test_that("teal_data accepts code as character", {
  testthat::expect_no_error(
    teal_data(
      iris1 = iris,
      code = "iris1 <- iris"
    )
  )
})

testthat::test_that("teal_data accepts code as language", {
  testthat::expect_no_error(
    teal_data(
      iris1 = iris,
      code = quote(iris1 <- iris)
    )
  )
})

testthat::test_that("teal_data code unfolds code-block wrapped in '{'", {
  testthat::expect_identical(
    get_code(teal_data(iris1 = iris, code = quote({
      iris1 <- iris
    }))),
    "iris1 <- iris"
  )
})

testthat::test_that("teal_data code is concatenated into single string", {
  testthat::expect_identical(
    get_code(teal_data(iris1 = iris, code = c("iris1 <- iris", "iris2 <- iris1"))),
    "iris1 <- iris\niris2 <- iris1"
  )
})

testthat::test_that("teal_data@.xData is locked. Not able to modify, add or remove bindings", {
  data <- teal_data(iris = iris)
  testthat::expect_error(data@.xData$iris <- iris, "cannot change value of locked binding for 'iris'")
  testthat::expect_error(data@.xData$iris2 <- iris, "cannot add bindings to a locked environment")
  testthat::expect_error(rm("iris", envir = data@.xData), "cannot remove bindings from a locked environment")
})
