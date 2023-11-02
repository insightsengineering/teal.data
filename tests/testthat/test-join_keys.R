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

test_that("join_keys<-.teal_data shared test to getter and setter", {
  obj <- helper_generator_teal_data()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

test_that("join_keys<-.JoinKeys shared test to getter and setter", {
  obj <- helper_generator_JoinKeys()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

test_that("join_keys<-.JoinKeys to set via a JoinKeySet object", {
  obj <- join_keys()
  join_keys(obj) <- join_key("ds1", "ds2", "id")
  expect_equal(obj$ds1, list("ds2" = c("id" = "id")))
  expect_equal(obj$ds2, list("ds1" = c("id" = "id")))
})

test_that("join_keys<-.JoinKeys to set via multiple lists that progressively merge object", {
  obj <- join_keys()
  join_keys(obj) <- list(join_key("ds1", "ds2", "id"))
  join_keys(obj) <- list(join_key("ds3", "ds4", "id_id"))
  join_keys(obj) <- join_key("ds5", "ds6", "id_id_id")

  expect_length(obj, 6)
})

# -----------------------------------------------------------------------------
#
# mutate_join_keys (empty value name)
#

test_that("mutate_join_keys with empty name is changed to the key value", {
  # set empty key name
  jk <- mutate_join_keys(join_keys(), "d1", "d2", c("A" = "B", "C"))
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- mutate_join_keys(join_keys(), "d1", "d2", c("A" = "B", "C" = ""))
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- mutate_join_keys(join_keys(), "d1", "d2", c("A" = "B", ""))
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("[<-.JoinKeys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  jk["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- join_keys()
  jk["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- join_keys()
  jk["d1", "d2"] <- c("A" = "B", "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("join_keys()[]<-.JoinKeys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(jk["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- join_keys()
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- join_keys()
  join_keys(jk)["d1", "d2"] <- c("A" = "B", "")
  expect_equal(jk["d1", "d2"], setNames(c("B", ""), c("A", "")))
})

test_that("join_keys()[]<-.teal_data with empty name is changed to the key value", {
  # set empty key name
  td <- teal_data()
  join_keys(td)["d1", "d2"] <- c("A" = "B", "C")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  td <- teal_data()
  join_keys(td)["d1", "d2"] <- c("A" = "B", "C" = "")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  td <- teal_data()
  join_keys(td)["d1", "d2"] <- c("A" = "B", "")
  expect_equal(join_keys(td)["d1", "d2"], setNames(c("B", ""), c("A", "")))
})
