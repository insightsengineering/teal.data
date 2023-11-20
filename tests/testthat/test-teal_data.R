testthat::test_that("teal_data allows to initialize empty teal_data object", {
  testthat::expect_s4_class(teal_data(), "teal_data")
})

testthat::test_that("empty teal_data returns empty code, id, wartnings and messages", {
  testthat::expect_identical(teal_data()@code, character(0))
  testthat::expect_identical(teal_data()@id, integer(0))
  testthat::expect_identical(teal_data()@messages, character(0))
  testthat::expect_identical(teal_data()@warnings, character(0))
})

testthat::test_that("teal_data allows to initialize empty teal_data with join_keys", {
  testthat::expect_equal(
    teal_data(join_keys = join_keys(join_key("data1", "data2", "id")))@join_keys,
    join_keys(join_key("data1", "data2", "id"))
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from passed objects", {
  testthat::expect_identical(
    teal_data(iris = iris, mtcars = mtcars)@datanames,
    c("iris", "mtcars")
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from passed join_keys", {
  testthat::expect_identical(
    teal_data(join_keys = join_keys(join_key("parent", "child", "id")))@datanames,
    c("parent", "child")
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from join_keys and passed objects", {
  testthat::expect_identical(
    teal_data(iris = iris, join_keys = join_keys(join_key("parent", "child", "id")))@datanames,
    c("iris", "parent", "child")
  )
})


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
    teal_data(iris1 = iris, code = quote({
      iris1 <- iris
    }))@code,
    "iris1 <- iris"
  )
})

testthat::test_that("teal_data code is concatenated into single string", {
  testthat::expect_identical(
    teal_data(iris1 = iris, code = c("iris1 <- iris", "iris2 <- iris1"))@code,
    "iris1 <- iris\niris2 <- iris1"
  )
})

testthat::test_that("teal_data@env is locked. Not able to modify, add or remove bindings", {
  data <- teal_data(iris = iris)
  testthat::expect_error(data@env$iris <- iris, "cannot change value of locked binding for 'iris'")
  testthat::expect_error(data@env$iris2 <- iris, "cannot add bindings to a locked environment")
  testthat::expect_error(rm("iris", envir = data@env), "cannot remove bindings from a locked environment")
})
