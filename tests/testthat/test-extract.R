testthat::test_that("`[.` subsets join_keys also", {
  data <- teal_data(a = 1, b = 2)
  join_keys(data) <- join_keys(join_key("a", "b", "x"))
  testthat::expect_length(join_keys(data["a"]), 0)
})

testthat::test_that("`[.` preserves @verified field", {
  testthat::expect_false(teal_data(a = 1, b = 2)["a"]@verified)
  testthat::expect_true(within(teal_data(), a <- 1)["a"]@verified)
})

testthat::test_that("`[.` warns and subsets if names are present in code", {
  data <- teal_data(a = 1, b = 2, code = "a <- 1; b <- 2; c <- 3; d <- 4")
  testthat::expect_warning(
    subset <- data[c("a", "c", "d")],
    "Some 'names' do not exist in the environment of the 'teal_data'. Skipping those: c, d."
  )
  testthat::expect_identical(subset@code, data@code[c(1, 3, 4)])
  testthat::expect_identical(as.list(subset), as.list(data)[1])
})
