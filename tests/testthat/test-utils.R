test_that("check_pkg_quietly works", {
  expect_error(check_pkg_quietly("my_random_package_1234567890", "my message"), regexp = "my message")
})

test_that("get_key_duplicates_util function", {
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5)),
    stringsAsFactors = TRUE
  )
  keys <- c("a", "b")

  # Input validations
  expect_error(get_key_duplicates_util(df, NULL))
  expect_error(get_key_duplicates_util("test", keys))
  expect_error(get_key_duplicates_util(df, c(1, 2, 3)))
  expect_error(get_key_duplicates_util(df, keys = c("a", "test")))

  # single duplicated value
  expect_true(dplyr::all_equal(
    data.frame(a = factor("b", levels = c("a", "b", "c")), b = 3, rows = "3,4", n = 2L),
    get_key_duplicates_util(df, keys)
  ))


  # multiple duplicates (and single key)
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 3, 4), c = c(1, 2, 3, 4, 5)),
    stringsAsFactors = TRUE
  )
  keys <- c("a")
  expect_true(dplyr::all_equal(
    data.frame(a = factor(c("a", "b"), levels = c("a", "b", "c")), rows = c("1,2", "3,4"), n = c(2L, 2L)),
    get_key_duplicates_util(df, keys)
  ))

  # Expect empty data.frame if there are no duplicated key values
  df <- as.data.frame(
    list(a = c("a", "a", "b", "b", "c"), b = c(1, 2, 3, 4, 5), c = c(1, 2, 3, 4, 5)),
    stringsAsFactors = TRUE
  )
  keys <- c("a", "b")

  expect_true(dplyr::all_equal(
    data.frame(a = factor(x = NULL, levels = c("a", "b", "c")), b = double(0), rows = character(0), n = integer(0)),
    get_key_duplicates_util(df, keys)
  ))
})

testthat::test_that("validate_metadata throws no error if metadata is NULL", {
  testthat::expect_error(
    validate_metadata(NULL),
    NA
  )
})

testthat::test_that("validate_metadata throws error if metadata is not a list (or NULL)", {
  testthat::expect_error(
    validate_metadata(1:10),
    "Must be of type 'list'"
  )
  testthat::expect_error(
    validate_metadata(character(0)),
    "Must be of type 'list'"
  )
})

testthat::test_that("validate_metadata throws error if metadata is not a list of length one atomics (or NULL)", {
  testthat::expect_error(
    validate_metadata(list(x = list())),
    "Must be of type 'atomic', not 'list'"
  )
  testthat::expect_error(
    validate_metadata(list(x = 1:10)),
    "Must have length 1"
  )
})

testthat::test_that("validate_metadata throws error if metadata is not a named list (or NULL)", {
  testthat::expect_error(
    validate_metadata(list(x = 1, 5)),
    "Must have names"
  )
  testthat::expect_error(
    validate_metadata(list("boo", "foo")),
    "Must have names"
  )
})

testthat::test_that("get_teal_bs_theme", {
  testthat::expect_true(is.null(get_teal_bs_theme()))
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
  withr::with_options(list("teal.bs_theme" = 1), {
    testthat::expect_warning(get_teal_bs_theme(), "the default shiny bootstrap is used")
  })
  withr::with_options(list("teal.bs_theme" = "bs_theme"), {
    testthat::expect_warning(get_teal_bs_theme(), "the default shiny bootstrap is used")
  })
})
