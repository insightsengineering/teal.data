# All TealDataAbstract tests are run using TealData objects
tealdata_mixed_call <- function(check = TRUE) {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  load_dataset(adsl)
  load_dataset(adtte)

  TealData$new(adsl, adtte, check = check)
}

testthat::test_that("TealDataAbstract cannot be instantiated", {
  testthat::expect_error(TealDataAbstract$new(), "Pure virtual method")
})

testthat::test_that("check returns NULL if the check parameter is false", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = FALSE)
  testthat::expect_null(data$check())
})

testthat::test_that("check throws an error when one of the passed datasets has empty code", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars))
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_error(data$check(), "code is empty")
})

testthat::test_that("check returns FALSE if the code provided in datasets does not reproduce them", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(iris)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_false(data$check())
})

testthat::test_that("check returns TRUE if the code is reproducible", {
  data <- tealdata_mixed_call()
  testthat::expect_true(data$check())
})

testthat::test_that("check_reproducibility passes if the reproducibility check passes", {
  data <- tealdata_mixed_call()
  testthat::expect_silent(data$check_reproducibility())
})

testthat::test_that("check_reproducibility throws error if reproducibility check does not pass", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(iris)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_error(data$check_reproducibility(), "Reproducibility check failed.")
})

testthat::test_that("get_check_result method returns TRUE if check passed", {
  data <- tealdata_mixed_call()
  testthat::expect_true(data$check())
  testthat::expect_true(data$get_check_result())
})

testthat::test_that("get_check_result method returns NULL if check is set to FALSE", {
  data <- tealdata_mixed_call(FALSE)

  testthat::expect_silent(data$check())
  testthat::expect_null(data$get_check_result())
})

testthat::test_that("get_code returns the code of the datasets when no input is specified", {
  data <- tealdata_mixed_call()

  testthat::expect_identical(
    data$get_code(),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
      "x <- ADSL\n",
      "ADTTE <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))\n})()"
  ))
})

testthat::test_that("get_code returns the code of the dataset specifed", {
  data <- tealdata_mixed_call()

  testthat::expect_identical(
    data$get_code("ADTTE"),
    "ADTTE <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))\n})()"
  )
})

testthat::test_that("get_code returns the non deparsed code when deparse is set to FALSE", {
  data <- tealdata_mixed_call()

  testthat::expect_identical(
    data$get_code("ADSL", deparse = FALSE),
    list(
      str2lang("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"),
      str2lang("x <- ADSL")
    )
  )
})

testthat::test_that("get_code throws error if dataname is not character or deparse is not logical", {
  data <- tealdata_mixed_call()

  testthat::expect_error(
    data$get_code(1),
    "Assertion on 'dataname' failed: Must be of type 'character' \\(or 'NULL'\\), not 'double'."
  )

  testthat::expect_error(
    data$get_code("ADSL", deparse = "TRUE"),
    "Assertion on 'deparse' failed: Must be of type 'logical flag', not 'character'."
  )
})

testthat::test_that("get_code_class with TRUE returns code without mutate code", {
  data <- tealdata_mixed_call()

  # MUTATE
  data <- data %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
    mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
      "x <- ADSL\n",
      "ADTTE <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))\n})()"
    )
  )
})

testthat::test_that("get_code_class with FALSE returns code with mutate code", {
  data <- tealdata_mixed_call()

  # MUTATE
  data <- data %>%
    mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
    mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
    mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")


  testthat::expect_identical(
    data$get_code_class(FALSE)$get_code(),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
      "x <- ADSL\n",
      "ADTTE <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))\n})()\n",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")\n",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)\n",
      "ADSL$x <- 1"
    )
  )
})

testthat::test_that("get_datanames returns a vector of characters", {
  data <- tealdata_mixed_call()
  testthat::expect_identical(data$get_datanames(), c("ADSL", "ADTTE"))
})

testthat::test_that("get_datanames throws error if an argument is passed", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$get_datanames("ADSL"), "unused argument")
})

testthat::test_that("get_dataset throws an error if no dataset is found with the passed name", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$get_dataset("iris"), "dataset iris not found")
})

testthat::test_that("get_dataset throws an error if passed name is not character", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$get_dataset(iris), "Must be of type 'string'")
})

testthat::test_that("get_dataset returns the dataset with the passed name", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  data <- tealdata_mixed_call()

  testthat::expect_equal(
    data$get_dataset("ADSL"),
    dataset(
      dataname = "ADSL",
      x = adsl_raw,
      code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
    )
  )
})

testthat::test_that("get_dataset returns a list of all datasets if passed NULL", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
  load_dataset(adsl)
  load_dataset(adtte)

  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_dataset(), list(ADSL = adsl, ADTTE = adtte$get_dataset()))
})

testthat::test_that("get_datasets returns a list of all datasets if passed NULL", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
  load_dataset(adsl)
  load_dataset(adtte)

  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_datasets(), list(ADSL = adsl, ADTTE = adtte$get_dataset()))
})

testthat::test_that("get_datasets throws an error is a dataset is not pulled yet", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )

  adtte_cf2 <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte2 <- dataset_connector("ADTTE", adtte_cf2, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  data <- TealData$new(adtte2)
  testthat::expect_error(data$get_datasets(), "Not all datasets have been pulled yet.")
})

testthat::test_that("get_items returns all items in TealDataAbstract object when no input dataname is specified", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
  load_dataset(adsl)
  load_dataset(adtte)

  data <- tealdata_mixed_call(check = TRUE)
  testthat::expect_equal(data$get_items(), list(ADSL = adsl, ADTTE = adtte))
})

testthat::test_that("get_items returns the item of the specified input dataname", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )

  load_dataset(adsl)
  data <- tealdata_mixed_call(check = TRUE)
  testthat::expect_equal(data$get_items("ADSL"), adsl)
})

testthat::test_that("get_items throws error if dataname is not found", {
  data <- tealdata_mixed_call(check = TRUE)
  testthat::expect_error(data$get_items("ADSL1"), "dataset ADSL1 not found")
})

testthat::test_that("get_check returns the check status", {
  data <- tealdata_mixed_call(check = TRUE)
  testthat::expect_true(data$get_check())

  data <- tealdata_mixed_call(check = FALSE)
  testthat::expect_false(data$get_check())
})

testthat::test_that("is_pulled returns if the datasets are pulled", {
  data <- tealdata_mixed_call()
  testthat::expect_true(data$is_pulled())
})

testthat::test_that("mutate updates the code", {
  data <- tealdata_mixed_call()
  data$mutate("ADSL$new_column <- 1")
  testthat::expect_equal(
    data$get_code(),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\nx <- ADSL\n",
      "ADTTE <- (function() {\n    as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))\n})()\n",
      "ADSL$new_column <- 1"
  ))
})

testthat::test_that("mutate_dataset updates the code of the dataset", {
  data <- tealdata_mixed_call()
  data$mutate_dataset("ADSL", "ADSL$new_column <- 1")
  testthat::expect_equal(
    data$get_code("ADSL"),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\nx <- ADSL\n",
      "ADSL$new_column <- 1"
    ))
})

testthat::test_that("mutate_dataset throws an error if the dataname is not found", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$mutate_dataset("ADSL2", "ADSL$new_column <- 1"),
    "all\\(dataname %in% self\\$get_datanames\\(\\)\\) is not TRUE"
  )
})

testthat::test_that("mutate_dataset throws an error if the dataname is not character", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$mutate_dataset(dataname = 1, "ADSL$new_column <- 1"),
    "Must be of type 'character', not 'double'."
  )
})

testthat::test_that("set_check sets the reproducibility check", {
  data <- tealdata_mixed_call()
  testthat::expect_silent(data$set_check(FALSE))
  testthat::expect_false(data$get_check())
})

testthat::test_that("set_check accepts only logical input", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$set_check("FALSE"), "Must be of type 'logical flag', not 'character'.")
})

testthat::test_that("set_pull_code sets code correctly", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  data <- data <- TealData$new(
    cdisc_dataset(
      dataname = "ADSL",
      x = adsl_raw
    ),
    check = TRUE
  )
  testthat::expect_identical(data$get_code(), "")

  data$set_pull_code("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))")
  testthat::expect_identical(data$get_code(), "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))")
})

testthat::test_that("set_pull_code throws error if code is not character", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  load_dataset(adsl)
  load_dataset(adtte)

  data <- TealData$new(adtte, check = TRUE)
  testthat::expect_error(
    data$set_pull_code(111),
    "Must be of type 'string', not 'double'."
  )
})

testthat::test_that("set_pull_code throws error if code is specified on data and dataset levels", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$set_pull_code("Add code here"),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )
})

testthat::test_that("set_pull_code throws error if TealDataAbstract has only connectors", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adsl <- dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
  adtte_cf <- callable_function(
    function() {
      as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
    }
  )
  adtte <- dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))

  load_dataset(adsl)
  load_dataset(adtte)

  data <- TealData$new(adtte, check = TRUE)
  testthat::expect_error(
    data$set_pull_code("Add code here"),
    "Connectors are reproducible by default and setting 'code' argument might break it"
  )
})

testthat::test_that("reassign_datasets_vars updates the references of vars in items according to items addresses", {
  test_ds0 <- TealDataset$new("test_ds0", head(mtcars), code = "test_ds0 <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_ds1",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )
  data <- TealData$new(test_ds0, test_ds1)

  # after reassignment vars_r6, vars and muatate_vars match new reference
  data_cloned <- data$clone(deep = TRUE)
  cloned_items <- data$get_items()
  data$reassign_datasets_vars()

  testthat::expect_identical(
    data$get_items()$test_ds1$get_var_r6()$test_ds0, cloned_items$test_ds0
  )
})

# private methods
testthat::test_that("check_combined_code returns TRUE when the code is reproducible", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  iris_ds <- TealDataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  data <- TealData$new(mtcars_ds, iris_ds)
  testthat::expect_true(data$.__enclos_env__$private$check_combined_code())
})

testthat::test_that("check_combined_code returns error when the code is not reproducible", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- mtcars")
  iris_ds <- TealDataset$new("head_iris", head(iris), code = "head_iris <- iris")
  data <- TealData$new(mtcars_ds, iris_ds)
  testthat::expect_false(data$.__enclos_env__$private$check_combined_code())
})

testthat::test_that("check_combined_code returns error when the code is not supplied", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars))
  iris_ds <- TealDataset$new("head_iris", head(iris))
  data <- TealData$new(mtcars_ds, iris_ds)
  testthat::expect_error(data$.__enclos_env__$private$check_combined_code())
})

testthat::test_that("get_datasets_code_class returns an empty `CodeClass` object when no code is passed", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars))
  iris_ds <- TealDataset$new("head_iris", head(iris))
  data <- TealData$new(mtcars_ds, iris_ds)
  code_class <- data$.__enclos_env__$private$get_datasets_code_class()
  testthat::expect_s3_class(code_class, "CodeClass")
  testthat::expect_identical(code_class$get_code(), "")
})

testthat::test_that("get_datasets_code_class returns a `CodeClass` object with the code passed", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  iris_ds <- TealDataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  data <- TealData$new(mtcars_ds, iris_ds)
  code_class <- data$.__enclos_env__$private$get_datasets_code_class()
  testthat::expect_s3_class(code_class, "CodeClass")
  testthat::expect_identical(code_class$get_code(), "head_mtcars <- head(mtcars)\nhead_iris <- head(iris)")
})

testthat::test_that("get_pull_code_class gets code correctly", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  data <- data <- TealData$new(
    cdisc_dataset(
      dataname = "ADSL",
      x = adsl_raw
    ),
    check = TRUE
  )
  testthat::expect_identical(data$.__enclos_env__$private$get_pull_code_class()$get_code(), "")

  data$set_pull_code("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))")
  testthat::expect_identical(
    data$.__enclos_env__$private$get_pull_code_class()$get_code(),
    "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
  )
})

testthat::test_that("set_mutate_code updates the object code", {
  data <- tealdata_mixed_call()
  data$.__enclos_env__$private$set_mutate_code("ADSL$new <- 1")

  testthat::expect_identical(
    data$get_code(),
    paste0("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
           "x <- ADSL\nADTTE <- (function() {\n    as.data.frame(as.list(setNames(",
           "nm = get_cdisc_keys(\"ADTTE\"))))\n})()\nADSL$new <- 1"
    )
  )
})

testthat::test_that("set_mutate_code accepts character code of length 1", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$.__enclos_env__$private$set_mutate_code(c("ADSL$new <- 1", "ADSL$new2 <- 2")),
    "Assertion failed"
  )

  testthat::expect_error(data$.__enclos_env__$private$set_mutate_code(c(1 + 1)), "Assertion failed")
})

testthat::test_that("set_mutate_vars appends the new mutate_vars", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  iris_ds <- TealDataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  data <- TealData$new(mtcars_ds, iris_ds)
  testthat::expect_silent(data$.__enclos_env__$private$set_mutate_vars(list("A" = "A")))
  testthat::expect_identical(
    data$.__enclos_env__$private$mutate_vars,
    list("A" = "A")
  )
})

testthat::test_that("set_mutate_vars accepts a unique names list, throws error otherwise", {
  mtcars_ds <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")
  iris_ds <- TealDataset$new("head_iris", head(iris), code = "head_iris <- head(iris)")
  data <- TealData$new(mtcars_ds, iris_ds)
  testthat::expect_silent(data$.__enclos_env__$private$set_mutate_vars(list("A" = "A")))
  testthat::expect_error(
    data$.__enclos_env__$private$set_mutate_vars(c("A" = "A")),
    "Must be of type 'list', not 'character'."
  )
  testthat::expect_error(
    data$.__enclos_env__$private$set_mutate_vars(list("A" = "A", "A" = "B")),
    "Must have unique names, but element 2 is duplicated."
  )
})

testthat::test_that("check_names throws if passed two datasets with the same name", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars))
  mtcars_ds2 <- TealDataset$new("cars", head(mtcars))
  testthat::expect_error(TealData$new(mtcars_ds, mtcars_ds2), "TealDatasets names should be unique")
})

testthat::test_that("execute_mutate returns current datasets if no mutate_code", {
  data <- tealdata_mixed_call()
  testthat::expect_identical(data$execute_mutate(), data$get_datasets())
})

testthat::test_that("execute_mutate returns updated datasets", {
  data <- tealdata_mixed_call()
  data %>% mutate_data("ADSL$new <- 1")
  testthat::expect_silent(data$execute_mutate())
  testthat::expect_identical(
    data$get_code(),
    paste0("ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
           "x <- ADSL\nADTTE <- (function() {\n    as.data.frame(as.list(setNames(",
           "nm = get_cdisc_keys(\"ADTTE\"))))\n})()\nADSL$new <- 1"
    )
  )
  testthat::expect_identical(data$get_dataset("ADSL")$data$new, 1)
})
