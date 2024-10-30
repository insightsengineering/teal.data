# get ----
testthat::test_that("names returns list of objects in teal_data", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_identical(names(td), c("i", "m"))
})

testthat::test_that("variables with dot prefix are omitted", {
  td <- teal_data(i = iris, m = mtcars)
  td <- within(td, .f <- faithful)
  testthat::expect_identical(names(td), c("i", "m"))
})

# set ---
testthat::test_that("names<- called on teal_data does not change it", {
  td <- teal_data(i = iris, m = mtcars)
  tdc <- td <- teal_data(i = iris, m = mtcars)
  suppressWarnings(names(td) <- c("a", "b", "c"))
  testthat::expect_identical(td, tdc)
})

# qenv.error support ----
testthat::test_that("names supports qenv.error class", {
  qe <- within(teal_data(), stop())
  testthat::expect_no_error(names(qe))
})


# topological_order ----

testthat::test_that("names are set in topological order in constructor if join_keys are specified", {
  data <-
    teal_data(b = data.frame(), a = data.frame(), join_keys = join_keys(join_key("a", "b", "id")))
  testthat::expect_identical(
    names(data),
    c("a", "b")
  )
})

testthat::test_that("names return parent if in constructor it was provided in join_keys and exists in env", {
  data <- teal_data(
    b = data.frame(),
    .a = data.frame(),
    join_keys = join_keys(join_key(".a", "b", "id"))
  )
  testthat::expect_setequal(names(data), c(".a", "b"))
})

testthat::test_that(
  "names do not return parent if in constructor it was provided in join_keys but do not exists in env",
  {
    data <- teal_data(b = data.frame(), join_keys = join_keys(join_key("a", "b", "id")))
    testthat::expect_identical(
      names(data),
      "b"
    )
  }
)

testthat::test_that("names return topological order of datasets once join_keys are specified", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    ADTR <- teal.data::rADTR # nolint: object_name.
    ADSL <- teal.data::rADSL # nolint: object_name.
  })
  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  testthat::expect_identical(
    names(data),
    c("ADTR", "ADSL", "ADTTE")
  )
})

testthat::test_that("names return topological order of datasets after new objects are added after join_keys", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    ADSL <- teal.data::rADSL # nolint: object_name.
  })

  testthat::expect_identical(
    names(data),
    c("ADSL", "ADTTE")
  )

  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  data <- within(data, ADTR <- teal.data::rADTR) # nolint: object_name.

  testthat::expect_identical(
    names(data),
    c("ADTR", "ADSL", "ADTTE")
  )
})


testthat::test_that("names return parent if join_keys were provided and parent exists in env", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    iris <- iris
    .ADSL <- teal.data::rADSL # nolint: object_name.
  })

  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  names(join_keys(data)) <- c(".ADSL", "ADTTE")

  testthat::expect_setequal(names(data), c(".ADSL", "ADTTE", "iris"))
})

testthat::test_that("names do not return parent if join_keys were provided and parent did not exists in env", {
  data <- teal_data(
    ADTTE = teal.data::rADTTE, # nolint: object_name.
    iris = iris
  )
  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  testthat::expect_setequal(names(data), c("ADTTE", "iris"))
})
