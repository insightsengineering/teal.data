test_that("join_key throws error with NULL keys", {
  expect_error(join_key("d1", "d2", keys = NULL))
})

test_that("join_key throws error with NA keys", {
  expect_error(join_key("d1", "d2", keys = NA))
  expect_error(join_key("d1", "d2", keys = c("X" = "A", "B", NA)))
})

test_that("join_key throws error with numeric keys", {
  expect_error(join_key("d1", "d2", keys = 1:10))
})

test_that("join_key throws error with data keys", {
  expect_error(join_key("d1", "d2", keys = Sys.time() + seq(1, 5)))
})

test_that("join_key throws error with invalid duplicate names in keys/values", {
  expect_error(join_key("d1", "d2", keys = c("A" = "A", "A" = "B")))
  expect_error(join_key("d1", "d2", keys = c("C" = "A", "D" = "A")))
})

test_that("join_key throws error with invalid primary keys (names != keys)", {
  expect_error(join_key("d1", "d1", keys = c("B" = "A", "A" = "B")))
  expect_error(join_key("d1", keys = c("B" = "A", "A" = "B")))
})

test_that("join_key throws error with invalid dataset arguments", {
  # missing
  expect_error(join_key("d1", as.character(NA), keys = c("A" = "B", "C" = "D")))
  # invalid type
  expect_error(join_key("d1", 5, keys = c("A" = "B", "C" = "D")))
  # invalid length
  expect_error(join_key("d1", c("d1", "d2"), keys = c("A" = "B", "C" = "D")))
})

test_that("join_key does not throw error with valid arguments", {
  # keys of length 0
  expect_silent(join_key("d1", "d2", keys = character(0)))
  # keys of length 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B")))
  # keys of length > 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B", "C" = "D")))
  # dataset_1 and dataset_2 can be the same if keys match
  expect_silent(join_key("d1", "d1", keys = c("A" = "A", "B" = "B")))

  expect_silent(join_key("d1", keys = c("A" = "A", "B" = "B")))
})

test_that("join_key will fill empty names with value", {
  # keys of length 0
  jk <- join_key("d1", "d2", keys = c("A" = "B", "C"))
  expect_identical(jk[[1]][[1]], setNames(c("B", "C"), c("A", "C")))

  jk <- join_key("d1", "d2", keys = c("B", "C"))
  expect_identical(jk[[1]][[1]], setNames(c("B", "C"), c("B", "C")))
})

test_that("join_key will fill empty values with name", {
  # keys of length 0
  jk <- join_key("d1", "d2", keys = c("A" = "B", "C" = ""))
  expect_identical(jk[[1]][[1]], setNames(c("B", "C"), c("A", "C")))

  jk <- join_key("d1", "d2", keys = c("B", "C" = ""))
  expect_identical(jk[[1]][[1]], setNames(c("B", "C"), c("B", "C")))
})

test_that("join_key ignores empty name/value on keys if it has other keys", {
  expect_message(jk <- join_key("d1", "d2", keys = c("A" = "B", "")), "are ignored")
  expect_identical(jk[[1]][[1]], setNames(c("B"), c("A")))

  expect_message(jk <- join_key("d1", "d2", keys = c("B", "")), "are ignored")
  expect_identical(jk[[1]][[1]], setNames(c("B"), c("B")))
})

test_that("join_key sets key as character(0) when keys are all all empty strings", {
  # invalid types
  jk <- join_key("d1", "d2", keys = c("", "", "", ""))
  expect_length(jk[[1]][[1]], 0)

  jk2 <- join_key("d1", "d2", keys = c(" ", " ", "    ", " "))
  expect_length(jk2[[1]][[1]], 0)
})
