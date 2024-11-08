testthat::test_that("`[.` handles empty names", {
  data <- teal_data(x = 1, a = 2)
  testthat::expect_warning(
    testthat::expect_equal(data[character(0)], teal_data())
  )
})

testthat::test_that("`[.` fails with names being NA_character_", {
  data <- teal_data(x = 1, a = 2)
  testthat::expect_error(data[NA_character_])
})

testthat::test_that("`[.` throws warning if names is NULL", {
  data <- teal_data(x = 1, a = 2)
  testthat::expect_error(
    data[NULL],
    "Assertion on 'names' failed: Must be of type 'character', not 'NULL'."
  )
})

testthat::test_that("`[.` thorws warnings if names is numeric", {
  data <- teal_data(x = 1, a = 2)
  testthat::expect_error(
    data[1],
    "Assertion on 'names' failed: Must be of type 'character', not 'double'."
  )
})

testthat::test_that("`[.` returns limited join_keys", {
  data <- teal_data(a = 1, b = 2)

  join_keys(data) <- join_keys(join_key("a", "b", "x"))
  testthat::expect_length(join_keys(data["a"]), 0)
})
