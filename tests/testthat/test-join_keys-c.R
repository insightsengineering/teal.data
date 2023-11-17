testthat::test_that("c.join_keys joins join_keys object with join_key objects", {
  obj <- join_keys()
  obj <- c(obj, join_key("a", "a", "aa"), join_key("b", "b", "bb"))
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys duplicated keys are ignored", {
  obj <- join_keys()
  obj <- c(obj, join_key("a", "a", "aa"), join_key("a", "a", "aa"))
  testthat::expect_identical(
    obj,
    join_keys(join_key("a", "a", "aa"))
  )
})

testthat::test_that("c.join_keys joins join_keys object with join_keys objects", {
  obj <- join_keys()
  obj <- c(
    obj,
    join_keys(join_key("a", "a", "aa")),
    join_keys(join_key("b", "b", "bb"))
  )
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys joins join_keys object with join_keys and join_key objects", {
  obj <- join_keys()
  obj <- c(
    obj,
    join_keys(join_key("a", "a", "aa")),
    join_key("b", "b", "bb")
  )
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys throws when joining with a list", {
  obj <- join_keys()
  testthat::expect_error(c(
    obj,
    list(c = list(c = "cc"))
  ))
})

testthat::test_that("c.join_keys doesn't throw when second object is empty join_keys", {
  x <- join_keys(join_key("a", "a", "aa"))
  y <- join_keys()
  testthat::expect_no_error(c(x, y))
})

testthat::test_that("c.join_keys throws on conflicting join_keys_set objects", {
  obj <- join_keys()
  testthat::expect_error(
    c(
      obj,
      join_keys(join_key("a", "b", "aa")),
      join_keys(join_key("b", "a", "bb"))
    ),
    "cannot specify multiple different join keys between datasets"
  )

  testthat::expect_error(
    c(
      obj,
      join_key("a", "b", "aa"),
      join_key("b", "a", "bb")
    ),
    "cannot specify multiple different join keys between datasets"
  )
})

testthat::test_that("c.join_key_set throws on conflicting join_keys_set objects", {
  testthat::expect_error(
    c(
      join_key("a", "b", "aa"),
      join_key("a", "b", "ca"),
      join_key("a", "b", "cc")
    ),
    "cannot specify multiple different join keys between datasets"
  )
})
