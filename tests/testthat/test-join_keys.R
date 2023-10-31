test_that("join_keys.teal_data will successfully obtain object from teal_data", {
  obj <- helper_generator_teal_data()

  expect_identical(obj@join_keys, join_keys(obj))
  helper_test_getter_join_keys(obj, "ds1")
})

test_that("join_keys.JoinKeys will return itself", {
  obj <- helper_generator_JoinKeys()

  expect_identical(obj, join_keys(obj))
  helper_test_getter_join_keys(obj, "ds1")
})

test_that("join_keys<-.teal_data", {
  obj <- helper_generator_teal_data()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

test_that("join_keys<-.JoinKeys", {
  obj <- helper_generator_JoinKeys()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

# -----------------------------------------------------------------------------
#
# mutate_join_keys (empty value name)
#

test_that("mutate_join_keys with empty name is changed to the key value", {
  jk <- new_join_keys()

  # set empty key name
  jk <- mutate_join_keys(jk, "d1", "d2", c("A" = "B", "C"))
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- mutate_join_keys(jk, "d1", "d2", c("A" = "B", "C" = ""))
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- mutate_join_keys(jk, "d1", "d2", c("A" = "B", ""))
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("[<-.JoinKeys with empty name is changed to the key value", {
  jk <- new_join_keys()

  # set empty key name
  jk["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  jk["d1", "d2"] <- c("A" = "B", "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("join_keys()[]<-.Placeholder with empty name is changed to the key value", {
  jk <- new_join_keys()

  # set empty key name
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("join_keys()[]<-.teal_data with empty name is changed to the key value", {
  td <- teal_data()

  # set empty key name
  join_keys(td)["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  join_keys(td)["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  join_keys(td)["d1", "d2"] <- c("A" = "B", "")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", ""), c("A", "")))
})
