testthat::describe("datanames deprecation: ", {
  testthat::it("setter does nothing to object", {
    td <- teal_data() |>
      within({
        iris <- iris
        mtcars <- mtcars
      })

    # ignore deprecation warnings
    withr::local_options(lifecycle_verbosity = "quiet")
    datanames(td) <- c("mtcars")

    testthat::expect_equal(
      td,
      teal_data() |>
        within({
          iris <- iris
          mtcars <- mtcars
        })
    )
  })

  testthat::it("getter returns same as `names()`", {
    td <- teal_data() |>
      within({
        iris <- iris
        mtcars <- mtcars
      })

    # ignore deprecation warnings
    withr::local_options(lifecycle_verbosity = "quiet")
    testthat::expect_setequal(datanames(td), names(td))
  })
})
