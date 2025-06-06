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

testthat::test_that("c.join_key_set joins join_key_set object with join_keys objects", {
  obj <- join_key("a", "a", "aa")
  obj <- c(obj, join_key("b", "b", "bb"), join_key("c", "c", "cc"))
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb"),
      join_key("c", "c", "cc")
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

testthat::test_that("c.join_key_set merges with empty and non-empty parents", {
  jk1 <- join_keys(
    join_key("d1", "d1", "a")
  )

  jk2 <- join_keys(
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d3", "d4", "cd")
  )

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d3", "d4", "cd", directed = FALSE)
  )
  parents(expected) <- list(d4 = "d3")

  testthat::expect_equal(
    c(jk1, jk2),
    expected
  )
})

testthat::test_that("c.join_key_set merges parents also", {
  jk1 <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab")
  )
  jk2 <- join_key("d3", "d3", "c")

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab", directed = FALSE),
    join_key("d3", "d3", "c")
  )
  parents(expected) <- list(d2 = "d1")

  testthat::expect_equal(
    c(jk1, jk2),
    expected
  )
})

testthat::test_that("c.join_keys merges parents also", {
  jk1 <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab")
  )

  jk2 <- join_keys(
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d3", "d4", "cd")
  )

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab", directed = FALSE),
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d3", "d4", "cd", directed = FALSE)
  )
  parents(expected) <- list(d2 = "d1", d4 = "d3")

  testthat::expect_identical(
    c(jk1, jk2),
    expected
  )
})

testthat::test_that("c.join_keys merges existing parents are overwritten", {
  jk1 <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d1", "d2", "ab"),
    join_key("d3", "d4", "cd")
  )

  jk2 <- join_keys(
    join_key("d2", "d2", "b"),
    join_key("d2", "d3", "cb")
  )

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d4", "d4", "d"),
    join_key("d1", "d2", "ab", directed = FALSE),
    join_key("d3", "d4", "cd", directed = FALSE),
    join_key("d2", "d3", "cb", directed = FALSE)
  )
  parents(expected) <- list(d2 = "d1", d4 = "d3", d3 = "d2")
  testthat::expect_identical(c(jk1, jk2), expected)
})

testthat::test_that("c.join_keys throws error when merge produces acyclical graph", {
  jk1 <- join_keys(join_key("d1", "d2", "a"))
  jk2 <- join_keys(join_key("d2", "d1", "a"))
  testthat::expect_error(c(jk1, jk2), "Cycle detected in a parent and child dataset graph")
})
