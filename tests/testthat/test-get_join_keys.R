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
