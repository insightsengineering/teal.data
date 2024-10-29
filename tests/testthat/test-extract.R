testthat::test_that("`[.` returns empty teal_data for improper names", {
  data <- teal_data(x = 1, a = 2)
  testthat::expect_equal(data["y"], teal_data())
})

testthat::test_that("`[.` returns limited join_keys", {
  data <- teal_data(a = 1, b = 2)

  join_keys(data) <- join_keys(join_key("a", "b", "x"))
  empty_join_keys <- join_keys()
  attr(empty_join_keys, "names") <- character(0)
  testthat::expect_equal(
    join_keys(data["a"]),  # By default   named list || names() is character(0)
    empty_join_keys        # By default unnamed list || names() is NULL
  )
})
