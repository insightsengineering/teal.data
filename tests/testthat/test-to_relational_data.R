dataset_1 <- dataset("iris", head(iris))
adsl_df <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adsl_dataset <- cdisc_dataset(
  "ADSL", adsl_df,
  parent = character(0), keys = get_cdisc_keys("ADSL")
)

to_relational_data_wrapper <- function(data) {
  to_relational_data(data)
}

test_that("to_relational_data accepts data.frame as input", {
  iris <- head(iris)
  output <- to_relational_data_wrapper(iris)
  testthat::expect_s4_class(output, "teal_data")
})

test_that("to_relational_data accepts cdisc data.frame as input", {
  output <- to_relational_data_wrapper(adsl_df)
  testthat::expect_s4_class(output, "teal_data")
})

test_that("to_relational_data accepts TealDataset/CDISCTealDataset as input", {
  output_dataset <- to_relational_data(dataset_1)
  testthat::expect_s4_class(output_dataset, "teal_data")

  output_cdisc_dataset <- to_relational_data(adsl_dataset)
  testthat::expect_s4_class(output_cdisc_dataset, "teal_data")
})

test_that("to_relational_data accepts TealDatasetConnector as input", {
  dsc1 <- dataset_connector("iris", callable_function(function() head(iris)))
  output_datasetconnector <- to_relational_data(dsc1)
  testthat::expect_error(output_datasetconnector, NA)
  testthat::expect_is(output_datasetconnector, "TealData")
  testthat::expect_identical(output_datasetconnector$get_datanames(), "iris")
})

test_that("to_relational_data accepts an unnamed list of data.frame as input", {
  output_dataset_list <- to_relational_data_wrapper(list(iris))
  testthat::expect_s4_class(output_dataset_list, "teal_data")
  testthat::expect_identical(output_dataset_list@datanames, "iris")
})

test_that("to_relational_data accepts a named list of data.frame as input", {
  output_dataset_list <- to_relational_data_wrapper(list(AA = head(iris)))
  testthat::expect_s4_class(output_dataset_list, "teal_data")
  testthat::expect_identical(output_dataset_list@datanames, "AA")
})

test_that("to_relational_data accepts a mixed named list of data.frame as input", {
  head_mtcars <- head(mtcars) # wouldn't be a valid R object name (head(mtcars))
  output_dataset_list <- to_relational_data_wrapper(list(AA = head(iris), head_mtcars))
  testthat::expect_s4_class(output_dataset_list, "teal_data")
  testthat::expect_identical(output_dataset_list@datanames, c("AA", "head_mtcars"))
})

test_that("to_relational_data accepts a complete named list of data.frame as input", {
  output_dataset_list <- to_relational_data_wrapper(list(AA = head(iris), BB = head(mtcars)))
  testthat::expect_error(output_dataset_list, NA)
  testthat::expect_s4_class(output_dataset_list, "teal_data")
  testthat::expect_identical(output_dataset_list@datanames, c("AA", "BB"))
})

test_that("to_relational_data accepts a mixed named list of objects as input", {
  dataset_22 <- dataset("iris22", head(iris))
  dsc1 <- dataset_connector("dsc1", callable_function(function() head(iris)))
  load_dataset(dsc1)

  output_dataset_list <- to_relational_data_wrapper(list(AA = head(iris), dataset_22))
  testthat::expect_s4_class(output_dataset_list, "teal_data")
  testthat::expect_identical(output_dataset_list@datanames, c("AA", "iris22"))

  output_dataset_list2 <- to_relational_data_wrapper(list(AA = head(iris), dataset_22, mtcars, dsc1))
  testthat::expect_s4_class(output_dataset_list2, "teal_data")
  testthat::expect_identical(output_dataset_list2@datanames, c("AA", "iris22", "mtcars", "dsc1"))
})

test_that("to_relational_data accepts a function returning a named list as input", {
  fun <- function() list(AA = adsl_df, BB = adsl_df)

  output_dataset_fun <- to_relational_data_wrapper(fun())
  testthat::expect_s4_class(output_dataset_fun, "teal_data")
  testthat::expect_identical(output_dataset_fun@datanames, c("AA", "BB"))
})

test_that("to_relational_data accepts a function returning a TealDataset as input", {
  fun <- function() cdisc_dataset("ADSL", adsl_df)

  output_dataset_fun <- to_relational_data(fun())
  testthat::expect_s4_class(output_dataset_fun, "teal_data")
  testthat::expect_identical(output_dataset_fun@datanames, "ADSL")
})

test_that("to_relational_data accepts a MultiAssayExperiment as input", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- miniACC
  output_dataset <- to_relational_data(mae)
  testthat::expect_s4_class(output_dataset, "teal_data")
  testthat::expect_identical(output_dataset@datanames, "MAE")
})


test_that("to_relational_data accepts a list containing a named MultiAssayExperiment as input", {
  utils::data(miniACC, package = "MultiAssayExperiment")
  mae <- miniACC
  output_dataset <- to_relational_data(list(aa = mae))
  testthat::expect_s4_class(output_dataset, "teal_data")
  testthat::expect_identical(output_dataset@datanames, "aa")
})

test_that("to_relational_data throws error with a function returning a non-named list", {
  fun <- function() list(iris, mtcars)

  testthat::expect_error(
    to_relational_data_wrapper(fun()),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list."
  )
})

test_that("to_relational_data throws error with a function returning a semi-named list", {
  fun <- function() list(iris = iris, mtcars)

  testthat::expect_error(
    to_relational_data_wrapper(fun()),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list."
  )
})

test_that("to_relational_data throws error with a multiple functions returning data.frame", {
  fun_iris <- function() iris
  fun_mtcars <- function() mtcars

  testthat::expect_error(
    to_relational_data_wrapper(setNames(nm = c("AA"), list(fun_iris(), fun_mtcars()))),
    "Unnamed lists shouldn't be provided as input for data. Please use a named list."
  )
})
