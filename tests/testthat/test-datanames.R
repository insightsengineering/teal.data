# get ----
testthat::test_that("datanames returns contents of @datanames slot", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_identical(datanames(td), c("i", "m"))
})

testthat::test_that("variables not in @datanames are omitted", {
  td <- teal_data(i = iris, m = mtcars)
  td <- within(td, f <- faithful)
  testthat::expect_identical(datanames(td), c("i", "m"))
})

# set ----
testthat::test_that("datanames can set value of @datanames", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_identical(datanames(td), c("i", "m"))
  datanames(td) <- "i"
  testthat::expect_identical(datanames(td), "i")
})

testthat::test_that("only names of existing variables are accepted", {
  td <- teal_data(i = iris, m = mtcars)
  testthat::expect_no_error(datanames(td) <- "i")
  testthat::expect_error(datanames(td) <- "f", "Assertion .* failed: Must be a subset")
})

# qenv.error support ----
testthat::test_that("datanames supports qenv.error class", {
  qe <- within(teal_data(), stop())
  testthat::expect_no_error(datanames(qe))
  testthat::expect_no_error(datanames(qe) <- "name")
})

testthat::test_that("datanames called on qenv.error returns NULL", {
  qe <- within(teal_data(), stop())
  testthat::expect_null(datanames(qe))
})

testthat::test_that("datanames<- called on qenv.error does not change qenv.error", {
  qe <- within(teal_data(), stop())
  qec <- qe
  testthat::expect_identical(qe, qec)
})

# topological_order ----

testthat::test_that("datanames are set in topological order in constructor if join_keys are specified", {
  data <-
    teal_data(b = data.frame(), a = data.frame(), join_keys = join_keys(join_key("a", "b", "id")))
  testthat::expect_identical(
    datanames(data),
    c("a", "b")
  )
})

testthat::test_that("datanames return parent if in constructor it was provided in join_keys and exists in env", {
  data <-
    teal_data(b = data.frame(), a = data.frame(), join_keys = join_keys(join_key("a", "b", "id")))
  datanames(data) <- "b"
  testthat::expect_identical(
    datanames(data),
    c("a", "b")
  )
})

testthat::test_that(
  "datanames do not return parent if in constructor it was provided in join_keys but do not exists in env", {
  data <-
    teal_data(b = data.frame(), join_keys = join_keys(join_key("a", "b", "id")))
  testthat::expect_identical(
    datanames(data),
    "b"
  )
})

testthat::test_that("datanames return topological order of datasets once join_keys are specified", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    iris <- iris
    ADSL <- teal.data::rADSL # nolint: object_name.
  })
  datanames(data) <- c("ADTTE", "iris", "ADSL")
  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  testthat::expect_identical(
    datanames(data),
    c("ADSL", "ADTTE", "iris")
  )
})

testthat::test_that("datanames return topological order of datasets after datanames are called after join_keys", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    iris <- iris
    ADSL <- teal.data::rADSL # nolint: object_name.
  })

  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  datanames(data) <- c("ADTTE", "iris", "ADSL")

  testthat::expect_identical(
    datanames(data),
    c("ADSL", "ADTTE", "iris")
  )
})


testthat::test_that("datanames return parent if join_keys were provided and parent exists in env", {
  data <- within(teal_data(), {
    ADTTE <- teal.data::rADTTE # nolint: object_name.
    iris <- iris
    ADSL <- teal.data::rADSL # nolint: object_name.
  })

  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]
  datanames(data) <- c("ADTTE", "iris")

  testthat::expect_identical(
    datanames(data),
    c("ADSL", "ADTTE", "iris")
  )
})

testthat::test_that("datanames do not return parent if join_keys were provided and parent did not exists in env", {
  data <- teal_data(
    ADTTE = teal.data::rADTTE, # nolint: object_name.
    iris = iris
  )

  join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE")]

  testthat::expect_identical(
    datanames(data),
    c("ADTTE", "iris")
  )
})
