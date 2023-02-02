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
adae_cf <- callable_function(
  function() {
    as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
  }
)
adae_cdc <- cdisc_dataset_connector("ADAE", adae_cf, keys = get_cdisc_keys("ADAE"))
adae_rdc <- cdisc_data_connector(
  connection = data_connection(open_fun = callable_function(function() "open function")),
  connectors = list(adae_cdc)
)

load_dataset(adsl)
load_dataset(adtte)
load_dataset(adae_cdc)

cdisc_data_mixed_call <- function(check = TRUE, join_keys1 = join_keys()) {
  cdisc_data(adsl, adtte, adae_rdc, check = check, join_keys = join_keys1)
}

testthat::test_that("cdisc_data accepts TealDataset, TealDatasetConnector, TealDataConnector objects", {
  testthat::expect_silent(data <- cdisc_data_mixed_call())
  testthat::expect_identical(data$get_datanames(), c("ADSL", "ADTTE", "ADAE"))
})

testthat::test_that("cdisc_data throws error if it receives undesired objects", {
  testthat::expect_error(
    teal_data(adsl_raw, check = TRUE),
    "May only contain the following types: \\{TealDataset,TealDatasetConnector,TealDataConnector\\}"
  )
})

testthat::test_that("cdisc_data sets the join_keys internally", {
  data <- cdisc_data_mixed_call()

  jks <- join_keys(
    join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")),
    join_key("ADTTE", "ADTTE", c("STUDYID", "USUBJID", "PARAMCD")),
    join_key("ADAE", "ADAE", c("STUDYID", "USUBJID", "ASTDTM", "AETERM", "AESEQ")),
    join_key("ADSL", "ADTTE", c("STUDYID", "USUBJID")),
    join_key("ADSL", "ADAE", c("STUDYID", "USUBJID")),
    join_key("ADTTE", "ADAE", c("STUDYID", "USUBJID"))
  )
  jks$set_parents(list(ADSL = character(0), ADTTE = "ADSL", ADAE = "ADSL"))
  testthat::expect_equal(data$get_join_keys(), jks)
})

testthat::test_that(
  "cdisc_data sets the join_keys internally based on parents relations when primary keys are altered", {
  jks <- join_keys(join_key("ADSL", "ADSL", c("STUDYID")))
  data <- cdisc_data_mixed_call(join_keys1 = jks)

  jks <- join_keys(
    join_key("ADSL", "ADSL", c("STUDYID")),
    join_key("ADTTE", "ADTTE", c("STUDYID", "USUBJID", "PARAMCD")),
    join_key("ADAE", "ADAE", c("STUDYID", "USUBJID", "ASTDTM", "AETERM", "AESEQ")),
    join_key("ADSL", "ADTTE", c("STUDYID")),
    join_key("ADSL", "ADAE", c("STUDYID")),
    join_key("ADTTE", "ADAE", c("STUDYID"))
  )
  jks$set_parents(list(ADSL = character(0), ADTTE = "ADSL", ADAE = "ADSL"))
  testthat::expect_equal(
    data$get_join_keys(),
    jks
  )
})

testthat::test_that("cdisc_data sets primary keys as join_keys when no join_keys are present", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  data <- cdisc_data(df1, df2, check = FALSE)

  jks <- join_keys(
    join_key("df1", "df1", "id"),
    join_key("df2", "df2", "df2_id")
  )
  jks$set_parents(list(df1 = character(0), df2 = character(0)))
  testthat::expect_equal(data$get_join_keys(), jks)
})

testthat::test_that("cdisc_data throws error when a parent/child graph is not correct", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- cdisc_dataset("df1", df1, keys = "id", parent = "df1")
  df2 <- cdisc_dataset("df2", df2, keys = "df2_id", parent = "df1")

  testthat::expect_error(
    cdisc_data(df1, df2, check = FALSE),
    "Cycle detected in a parent and child dataset graph."
  )
})

testthat::test_that("Basic example - without code and check", {
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", adsl_raw), code = "", check = FALSE))
  testthat::expect_silent(cdisc_data(cdisc_dataset("ADSL", adsl_raw),
    cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
    cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")),
    code = "", check = FALSE
  ))
})

testthat::test_that("Basic example - check overall code", {
  testthat::expect_silent(
    cdisc_data(
      cdisc_dataset("ADSL", adsl_raw),
      cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")),
      code = "ADSL <- ARG1 <- ARG2 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))",
      check = TRUE
    )
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset(
        "ADSL",
        adsl_raw,
        code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset("ARG1", adsl_raw, keys = get_cdisc_keys("ADSL")),
      cdisc_dataset("ARG2", adsl_raw, keys = get_cdisc_keys("ADSL")),
      code = "test",
      check = TRUE
    ),
    "'code' argument should be specified only in the 'cdisc_data' or in 'cdisc_dataset' but not in both"
  )

  testthat::expect_error(
    cdisc_data(
      cdisc_dataset(
        "ADSL",
        adsl_raw,
        code = "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset(
        dataname = "ARG1",
        x = dplyr::mutate(adsl_raw, x1 = 1),
        keys = get_cdisc_keys("ADSL"),
        code = "ARG1 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      cdisc_dataset(
        "ARG2",
        adsl_raw,
        keys = get_cdisc_keys("ADSL"),
        code = "ARG2 <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))"
      ),
      check = TRUE
    ),
    "Reproducibility check failed."
  )
})

testthat::test_that("List values", {
  test_relational_data_equal <- function(data1, data2) {
    testthat::expect_equal(data1$get_items(), data2$get_items())
    testthat::expect_equal(data1$get_join_keys(), data2$get_join_keys())
    mapply(testthat::expect_equal, data1$get_ui("test"), data2$get_ui("test"))
  }

  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw, label = "test_label"))

  datasets <- list(cdisc_dataset(
    dataname = "ADSL",
    x = adsl_raw,
    keys = c("STUDYID", "USUBJID"),
    parent = character(0),
    label = "test_label"
  ))

  result_to_compare <- do.call(cdisc_data, datasets)

  test_relational_data_equal(result, result_to_compare)

  result <- cdisc_data(cdisc_dataset("ADSL", adsl_raw), cdisc_dataset("ADTTE", adtte_raw))

  datasets <- list(
    cdisc_dataset(
      dataname = "ADSL",
      x = adsl_raw,
      keys = c("STUDYID", "USUBJID"),
      parent = character(0),
      label = character(0)
    ),
    cdisc_dataset(
      dataname = "ADTTE",
      x = adtte_raw,
      keys = c("STUDYID", "USUBJID", "PARAMCD"),
      parent = "ADSL",
      label = character(0)
    )
  )

  result_to_compare <- do.call("cdisc_data", datasets)

  test_relational_data_equal(result, result_to_compare)
})

testthat::test_that("cdisc_data_file loads the TealData object", {
  tmp_file <- tempfile(fileext = ".R")
  writeLines(text = c(
    "adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys('ADSL'))))
    adsl <- cdisc_dataset(
      dataname = 'ADSL',
      x = adsl_raw,
      code = 'ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))'
    )
    teal_data(adsl)
    "
  ),
  con = tmp_file
  )
  tdf <- cdisc_data_file(tmp_file)
  file.remove(tmp_file)
  testthat::expect_s3_class(tdf, "TealData")
  testthat::expect_identical(
    tdf$get_code(),
    paste0(
      "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
      "adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n",
      "adsl <- cdisc_dataset(dataname = \"ADSL\", x = adsl_raw, ",
      "code = \"ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\\\"ADSL\\\"))))\")\n",
      "teal_data(adsl)"
    )
  )
})

testthat::test_that("cdisc_data_file uses the code input to mutate the code of the loaded TealData object", {
  tmp_file <- tempfile(fileext = ".R")
  writeLines(text = c(
    "adsl_raw <- as.data.frame(as.list(setNames(nm = get_cdisc_keys('ADSL'))))
    adsl <- cdisc_dataset(
      dataname = 'ADSL',
      x = adsl_raw,
      code = 'ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))'
    )
    teal_data(adsl)
    "
  ),
  con = tmp_file
  )
  tdf <- cdisc_data_file(tmp_file, "# MUTATE code")
  file.remove(tmp_file)
  testthat::expect_s3_class(tdf, "TealData")
  testthat::expect_identical(
    tdf$get_code(),
    "ADSL <- as.data.frame(as.list(setNames(nm = get_cdisc_keys(\"ADSL\"))))\n# MUTATE code"
  )
})
