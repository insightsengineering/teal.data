# All TealDataAbstract tests are run using TealData objects
adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
adtte_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
adae_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))

adsl <- cdisc_dataset(
  dataname = "ADSL",
  x = adsl_raw,
  code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
)
adtte_cf <- callable_function(
  function() {
    as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  }
)
adtte <- cdisc_dataset_connector("ADTTE", adtte_cf, keys = get_cdisc_keys("ADTTE"), vars = list(x = adsl))
load_dataset(adsl)
load_dataset(adtte)

tealdata_mixed_call <- function(check = TRUE) {
  TealData$new(adsl, adtte, check = check)
}

testthat::test_that("TealDataAbstract cannot be instantiated", {
  testthat::expect_error(TealDataAbstract$new(), "Pure virtual method")
})

testthat::test_that("TealDataAbstract$check returns NULL if the check parameter is false", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = FALSE)
  testthat::expect_null(data$check())
})

testthat::test_that("TealDataAbstract$check throws an error when one of the passed datasets have empty code", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars))
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_error(data$check(), "code is empty")
})

testthat::test_that("TealDataAbstract$check returns FALSE if the code provided in datasets does not reproduce them", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(iris)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_false(data$check())
})

testthat::test_that("TealDataAbstract$check returns TRUE if the code is reproducible", {
  data <- tealdata_mixed_call()
  testthat::expect_true(data$check())
})

testthat::test_that("TealDataAbstract$check_reproducibility returns NULL if the reproducibility check passes", {
  data <- tealdata_mixed_call()
  testthat::expect_silent(data$check_reproducibility())
})

testthat::test_that("TealDataAbstract$check throws error if reproducibility check does not pass", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(iris)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_error(data$check_reproducibility(), "Reproducibility check failed.")
})

# more tests required for execute_mutate
testthat::test_that("execute_mutate returns current datasets if no mutate_code", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun)
  t_ds <- dataset("head_rock", head(rock), code = "head_rock <- head(rock)") %>%
    mutate_dataset("head_rock$head_letters <- test_dc$head_letters", vars = list(test_dc = t_dc))
  data <- TealData$new(t_dc, t_ds)
  testthat::expect_identical(
    data$execute_mutate(),
    list(head_rock = t_ds)
  )
})

testthat::test_that("get_check_result method returns NULL if check was set to FALSE", {
  data <- tealdata_mixed_call(FALSE)
  testthat::expect_null(data$get_check_result())
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

testthat::test_that("get_dataset returns the dataset with the passed name", {
  data <- tealdata_mixed_call()

  testthat::expect_equal(
    data$get_dataset("ADSL"),
    cdisc_dataset(
      dataname = "ADSL",
      x = adsl_raw,
      code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
    )
  )
})

testthat::test_that("TealDataAbstract$get_dataset returns a list of all datasets if passed NULL", {
  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_dataset(), list(ADSL = adsl, ADTTE = adtte$get_dataset()))
})

testthat::test_that("TealDataAbstract$get_datasets returns a list of all datasets if passed NULL", {
  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_datasets(), list(ADSL = adsl, ADTTE = adtte$get_dataset()))
})

testthat::test_that("TealDataAbstract$get_items returns a list of all datasets if passed NULL", {
  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_items(), list(ADSL = adsl, ADTTE = adtte))
})

testthat::test_that("TealDataAbstract$get_items returns the dataset passed in the input", {
  data <- tealdata_mixed_call()
  testthat::expect_equal(data$get_items("ADSL"), adsl)
})

testthat::test_that("TealDataAbstract$get_items throws error when the name of the dataset is not found", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$get_items("ADSL2"), "dataset ADSL2 not found")
})

testthat::test_that("TealData keeps references to the objects passed to the constructor", {
  test_ds0 <- TealDataset$new("test_ds0", head(mtcars), code = "test_ds0 <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_ds1",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )
  data <- TealData$new(test_ds0, test_ds1)
  testthat::expect_identical(data$get_items(), list(test_ds0 = test_ds0, test_ds1 = test_ds1))
})

testthat::test_that("TealDataAbstract$get_check returns the check status", {
  data <- tealdata_mixed_call(check = TRUE)
  testthat::expect_true(data$get_check())

  data <- tealdata_mixed_call(check = FALSE)
  testthat::expect_false(data$get_check())
})

testthat::test_that("TealDataAbstract$is_pulled returns if the datasets are pulled", {
  data <- tealdata_mixed_call()
  testthat::expect_true(data$is_pulled())
})

testthat::test_that("TealDataAbstract$mutate updates the code", {
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

testthat::test_that("TealDataAbstract$mutate_dataset updates the code of the dataset", {
  data <- tealdata_mixed_call()
  data$mutate_dataset("ADSL", "ADSL$new_column <- 1")
  testthat::expect_equal(
    data$get_code("ADSL"),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\nx <- ADSL\n",
      "ADSL$new_column <- 1"
    ))
})

testthat::test_that("TealDataAbstract$mutate_dataset throws an error if the dataname is not found", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$mutate_dataset("ADSL2", "ADSL$new_column <- 1"),
    "all\\(dataname %in% self\\$get_datanames\\(\\)\\) is not TRUE"
  )
})

testthat::test_that("TealDataAbstract$mutate_dataset throws an error if the dataname is not character", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$mutate_dataset(dataname = 1, "ADSL$new_column <- 1"),
    "Must be of type 'character', not 'double'."
  )
})

testthat::test_that("TealDataAbstract$set_check sets the reproducibility check", {
  data <- tealdata_mixed_call()
  testthat::expect_silent(data$set_check(FALSE))
  testthat::expect_false(data$get_check())
})

testthat::test_that("TealDataAbstract$set_check accepts only logical input", {
  data <- tealdata_mixed_call()
  testthat::expect_error(data$set_check("FALSE"), "Must be of type 'logical flag', not 'character'.")
})

testthat::test_that("TealDataAbstract$set_pull_code sets code correctly", {
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

testthat::test_that("TealDataAbstract$set_pull_code throws error if code is not character", {
  data <- TealData$new(adtte, check = TRUE)
  testthat::expect_error(
    data$set_pull_code(111),
    "Must be of type 'string', not 'double'."
  )
})

testthat::test_that("TealDataAbstract$set_pull_code throws error if code is specified on data and dataset levels", {
  data <- tealdata_mixed_call()
  testthat::expect_error(
    data$set_pull_code("Add code here"),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )
})

testthat::test_that("TealDataAbstract$set_pull_code throws error if TealDataAbstract has only connectors", {
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
testthat::test_that("TealDataAbstract$check_names throws if passed two datasets with the same name", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars))
  mtcars_ds2 <- TealDataset$new("cars", head(mtcars))
  testthat::expect_error(TealData$new(mtcars_ds, mtcars_ds2), "TealDatasets names should be unique")
})
####











# 3. two datasets / global code -------------------------------
testthat::test_that("two datasets / datasets code", {
  adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"), object = list(1:3, letters[1:3]))))
  adsl <- cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))"
  )

  adtte <- cdisc_dataset(
    dataname = "ADTTE",
    x = adtte_raw,
    code = "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))"
  )

  testthat::expect_error(
    cdisc_data(
      adsl,
      adtte,
      code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl\nADTTE <- synthetic_cdisc_data(\"latest\")$adtte", # nolint
      check = TRUE
    )
  )

  testthat::expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADTTE", adtte_raw),
      code = paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        sep = "\n"
      ),
      check = TRUE
    )
  )

  testthat::expect_identical(
    data$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code("ADSL"),
    data$get_code(),
  )
  testthat::expect_identical(
    data$get_code("ADTTE"),
    data$get_code()
  )
  testthat::expect_error(cdisc_dataset("ADSL", adsl_raw)$check(), "code is empty")
  testthat::expect_error(cdisc_dataset("ADTTE", adtte_raw)$check(), "code is empty")
  testthat::expect_true(data$check())

  # MUTATE
  adsl <- cdisc_dataset(dataname = "ADSL", x = adsl_raw)
  adtte <- cdisc_dataset(dataname = "ADTTE", x = adtte_raw)

  testthat::expect_silent(
    data <- cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ADTTE", adtte_raw),
      code = paste(
        "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
        "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
        sep = "\n"
      ),
      check = TRUE
    ) %>%
      mutate_data(code = "ADSL <- dplyr::filter(ADSL, USUBJID == 'a')") %>%
      mutate_dataset(dataname = "ADTTE", code = "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)") %>%
      mutate_dataset(dataname = "ADSL", code = "ADSL$x <- 1")
  )


  testthat::expect_error(adsl$check(), "code is empty")
  testthat::expect_error(adtte$check(), "code is empty")
  testthat::expect_true(data$check())

  testthat::expect_identical(
    data$get_code_class(TRUE)$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code(),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_identical(
    data$get_code("ADTTE"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADTTE <- dplyr::filter(ADTTE, USUBJID %in% ADSL$USUBJID)",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    data$get_code("ADSL"),
    paste(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"), object = list(1:3, letters[1:3]))))",
      "ADTTE <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADTTE\"))))",
      "ADSL <- dplyr::filter(ADSL, USUBJID == \"a\")",
      "ADSL$x <- 1",
      sep = "\n"
    )
  )

  testthat::expect_reference(
    data$get_dataset("ADSL")$get_raw_data(),
    adsl_raw
  )
  testthat::expect_reference(
    data$get_dataset("ADTTE")$get_raw_data(),
    adtte_raw
  )
  data$execute_mutate()

  new_env <- new.env()
  eval(parse(text = data$get_code(), keep.source = FALSE), envir = new_env)
  testthat::expect_identical(
    get(x = "ADSL", envir = new_env),
    data$get_dataset("ADSL")$get_raw_data()
  )
  testthat::expect_identical(
    get(x = "ADTTE", envir = new_env),
    data$get_dataset("ADTTE")$get_raw_data()
  )
})


# 7. invalid arguments -----
testthat::test_that("Cannot create TealData if arguments include TealData object", {
  c_data <- cdisc_data(
    cdisc_dataset("ADSL", adsl_raw)
  )

  testthat::expect_error(cdisc_data(c_data))
  testthat::expect_error(cdisc_data(cdisc_dataset("ADSL", adsl_raw), c_data))
})
