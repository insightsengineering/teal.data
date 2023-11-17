test_that("all.equal.join_keys identical join_keys without parents are not identical, but treated as equal", {
  jk1 <- join_keys(
    join_key("d1", "d2", c(a = "b"))
  )
  jk2 <- join_keys(
    join_key("d2", "d1", c(b = "a"))
  )

  testthat::expect_failure(
    testthat::expect_identical(
      jk1,
      jk2
    )
  )

  testthat::expect_true(
    all.equal.join_keys(
      jk1,
      jk2
    )
  )
})

test_that("all.equal.join_keys identical join_keys with parents are treated as equal", {
  jk1 <- join_keys(
    join_key("d1", "d2", c(a = "b"))
  )
  parents(jk1) <- list("d2" = "d1")
  jk2 <- join_keys(
    join_key("d2", "d1", c(b = "a"))
  )
  parents(jk2) <- list("d2" = "d1")

  testthat::expect_true(
    all.equal.join_keys(
      jk1,
      jk2
    )
  )
})

test_that("all.equal.join_keys 2 objects with different parents return error", {
  jk1 <- join_keys(
    join_key("d1", "d2", c(a = "b"))
  )
  parents(jk1) <- list("d2" = "d1")
  jk2 <- join_keys(
    join_key("d2", "d1", c(b = "a"))
  )

  testthat::expect_type(
    all.equal.join_keys(
      jk1,
      jk2
    ),
    "character"
  )
})

test_that("all.equal.join_keys 2 objects with empty parents (one NULL and another empty list) are treated as equal", {
  jk1 <- join_keys(
    join_key("d1", "d2", c(a = "b"))
  )

  jk2 <- join_keys(
    join_key("d2", "d1", c(b = "a"))
  )
  attr(jk2, "__parents__") <- NULL

  testthat::expect_true(
    all.equal.join_keys(
      jk1,
      jk2
    )
  )
})
