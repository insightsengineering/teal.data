# Single dataset connector ----
testthat::test_that("Single dataset connector", {
  # create object
  pull_adsl <- function(ADSL, n = 1) ADSL <- head(teal.data::rADSL, n)
  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = pull_adsl) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )
  default_ui <- adsl$get_ui("main-app")
  adsl$set_ui_input(function(ns) {
    list(numericInput(inputId = ns("n"), label = "Choose number of records", min = 0, value = 1))
  })
  set_ui <- adsl$get_ui("main-app")
  testthat::expect_false(isTRUE(all.equal(default_ui, set_ui)))

  # check UI
  testthat::expect_equal(
    as.character(set_ui),
    as.character(
      tags$div(
        tags$div(
          id = "main-app-inputs",
          h4("TealDataset Connector for ", code("ADSL")),
          numericInput(inputId = "main-app-n", label = "Choose number of records", min = 0, value = 1)
        )
      )
    )
  )

  testthat::expect_error(adsl$get_raw_data(), regexp = "'ADSL' has not been pulled yet")
  adsl$pull()

  testthat::expect_s3_class(adsl$get_raw_data(), "data.frame")

  # check reproducible code
  testthat::expect_equal(
    adsl$get_code(),
    "ADSL <- (function(ADSL, n = 1) ADSL <- head(teal.data::rADSL, n))(ADSL = ADSL)"
  )
})

testthat::test_that("TealDatasetConnector$print prints out expected output of class and content", {
  fun <- CallableFunction$new(function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL")))))
  adsl <- cdisc_dataset_connector(
    dataname = "ADSL",
    pull_callable = fun,
    keys = get_cdisc_keys("ADSL")
  )

  testthat::expect_identical(
    capture.output(print(adsl)),
    c(
      "A CDISCTealDatasetConnector object, named ADSL, containing a TealDataset object that has not been loaded/pulled"
    )
  )

  adsl$pull()
  testthat::expect_identical(
    capture.output(print(adsl)),
    c(
      "A CDISCTealDatasetConnector object, named ADSL, containing a TealDataset object that has been loaded/pulled:",
      "A CDISCTealDataset object containing the following data.frame (1 rows and 2 columns):",
      "  STUDYID USUBJID",
      "1 STUDYID USUBJID"
    )
  )
})
