testthat::test_that("join_key throws error with NULL keys", {
  testthat::expect_error(join_key("d1", "d2", keys = NULL))
})

testthat::test_that("join_key throws error with NA keys", {
  testthat::expect_error(join_key("d1", "d2", keys = NA))
  testthat::expect_error(join_key("d1", "d2", keys = c("X" = "A", "B", NA)))
})

testthat::test_that("join_key throws error with numeric keys", {
  testthat::expect_error(join_key("d1", "d2", keys = 1:10))
})

testthat::test_that("join_key throws error with data keys", {
  testthat::expect_error(join_key("d1", "d2", keys = Sys.time() + seq(1, 5)))
})

testthat::test_that("join_key throws error with invalid duplicate names in keys/values", {
  testthat::expect_error(join_key("d1", "d2", keys = c("A" = "A", "A" = "B")))
  testthat::expect_error(join_key("d1", "d2", keys = c("C" = "A", "D" = "A")))
})

testthat::test_that("join_key throws error with invalid primary keys (names != keys)", {
  testthat::expect_error(join_key("d1", "d1", keys = c("B" = "A", "A" = "B")))
  testthat::expect_error(join_key("d1", keys = c("B" = "A", "A" = "B")))
})

testthat::test_that("join_key throws error with invalid dataset arguments", {
  # missing
  testthat::expect_error(join_key("d1", as.character(NA), keys = c("A" = "B", "C" = "D")))
  # invalid type
  testthat::expect_error(join_key("d1", 5, keys = c("A" = "B", "C" = "D")))
  # invalid length
  testthat::expect_error(join_key("d1", c("d1", "d2"), keys = c("A" = "B", "C" = "D")))
})

testthat::test_that("join_key does not throw error with valid arguments", {
  # keys of length 0
  testthat::expect_silent(join_key("d1", "d2", keys = character(0)))
  # keys of length 1
  testthat::expect_silent(join_key("d1", "d2", keys = c("A" = "B")))
  # keys of length > 1
  testthat::expect_silent(join_key("d1", "d2", keys = c("A" = "B", "C" = "D")))
  # dataset_1 and dataset_2 can be the same if keys match
  testthat::expect_silent(join_key("d1", "d1", keys = c("A" = "A", "B" = "B")))

  testthat::expect_silent(join_key("d1", keys = c("A" = "A", "B" = "B")))
})

testthat::test_that("join_key will fill empty names with value", {
  # keys of length 0
  jk <- join_key("d1", "d2", keys = c("A" = "B", "C"))
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B", "C"), c("A", "C")))

  jk <- join_key("d1", "d2", keys = c("B", "C"))
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B", "C"), c("B", "C")))
})

testthat::test_that("join_key will fill empty values with name", {
  # keys of length 0
  jk <- join_key("d1", "d2", keys = c("A" = "B", "C" = ""))
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B", "C"), c("A", "C")))

  jk <- join_key("d1", "d2", keys = c("B", "C" = ""))
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B", "C"), c("B", "C")))
})

testthat::test_that("join_key ignores empty name/value on keys if it has other keys", {
  testthat::expect_message(jk <- join_key("d1", "d2", keys = c("A" = "B", "")), "are ignored")
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B"), c("A")))

  testthat::expect_message(jk <- join_key("d1", "d2", keys = c("B", "")), "are ignored")
  testthat::expect_identical(jk[[1]][[1]], stats::setNames(c("B"), c("B")))
})

testthat::test_that("join_key sets key as character(0) when keys are all all empty strings", {
  # invalid types
  jk <- join_key("d1", "d2", keys = c("", "", "", ""))
  testthat::expect_length(jk[[1]][[1]], 0)

  jk2 <- join_key("d1", "d2", keys = c(" ", " ", "    ", " "))
  testthat::expect_length(jk2[[1]][[1]], 0)
})
