testthat::test_that("TealData$new throws if data is not valid", {
  testthat::expect_error(
    TealData$new("mtcars"),
    "All elements should be of TealDataset\\(Connector\\) or TealDataConnector class"
  )

  testthat::expect_error(
    TealData$new(mtcars),
    "All elements should be of TealDataset\\(Connector\\) or TealDataConnector class"
  )
})

testthat::test_that("TealData$new sets join_keys datasets based on the passed join_keys input otherwise empty", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  join_keys1 <- join_keys(join_key("df1", "df1", "id"), join_key("df2", "df2", "df2_id"))
  data <- TealData$new(df1, df2, join_keys = join_keys1)
  # primary keys are not taken from datasets when calling TealData$new(),
  # these are only added if using wrappers e.g. teal_data or cdisc_data
  testthat::expect_equal(
    data$get_join_keys(),
    join_keys1
  )

  data2 <- TealData$new(df1, df2)
  testthat::expect_equal(
    data2$get_join_keys(),
    join_keys()
  )
})

testthat::test_that("TealData$new sets pull and mutate code as empty CodeClass", {
  TestTealData <- R6::R6Class( # nolint
    classname = "TestTealData",
    inherit = TealData,
    public = list(
      get_mutate_code = function() private$mutate_code,
      get_pull_code = function() private$pull_code
    )
  )
  data <- TestTealData$new(dataset("mtcars", mtcars))
  mutate_code <- data$get_mutate_code()
  pull_code <- data$get_pull_code()

  testthat::expect_s3_class(mutate_code, "CodeClass")
  testthat::expect_equal(mutate_code$get_code(), "")

  testthat::expect_s3_class(pull_code, "CodeClass")
  testthat::expect_equal(pull_code$get_code(), "")
})

testthat::test_that("copy(deep = TRUE) deep copies self and the items", {
  test_ds0 <- TealDataset$new("test_ds0", head(mtcars), code = "test_ds0 <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_ds1",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )
  data <- TealData$new(test_ds0, test_ds1)
  data_cloned <- data$copy(deep = TRUE)
  testthat::expect_false(identical(data, data_cloned))
  testthat::expect_false(identical(data_cloned$get_items()$test_ds0, test_ds0))
})

testthat::test_that("copy(deep = TRUE) keeps valid references between items", {
  test_ds0 <- TealDataset$new("test_ds0", head(mtcars), code = "test_ds0 <- head(mtcars)")
  test_ds1 <- TealDatasetConnector$new(
    dataname = "test_ds1",
    pull_callable = CallableFunction$new(data.frame),
    vars = list(test_ds0 = test_ds0)
  )
  data <- TealData$new(test_ds0, test_ds1)
  data_cloned <- data$copy(deep = TRUE)
  new_test_ds0 <- data_cloned$get_items()$test_ds0
  new_test_ds1 <- data_cloned$get_items()$test_ds1
  testthat::expect_identical(
    new_test_ds1$get_var_r6()$test_ds0,
    new_test_ds0
  )
})

testthat::test_that("TealData$print prints out expected output on basic input", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  data <- TealData$new(df1, df2)

  out <- capture.output(print(data))
  testthat::expect_equal(
    out,
    c(
      "A TealData object containing 2 TealDataset/TealDatasetConnector object(s) as element(s):",
      "--> Element 1:",
      "A TealDataset object containing the following data.frame (2 rows and 2 columns):",
      "  id a",
      "1  A 1",
      "2  B 2",
      "--> Element 2:",
      "A TealDataset object containing the following data.frame (2 rows and 3 columns):",
      "  df2_id fk b",
      "1      A  A 1",
      "2      B  B 2"
    )
  )
})

testthat::test_that("TealData$get_connectors returns an empty list if no connectors are provided", {
  mtcars_ds1 <- TealDataset$new("cars1", head(mtcars), code = "cars1 <- head(mtcars)")
  data <- TealData$new(mtcars_ds1, check = TRUE)
  testthat::expect_identical(data$get_connectors(), list())
})

testthat::test_that("TealData$get_connectors returns a list with the numbers of connectors provided", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal.data"))
    con <- TealDataConnection$new(open_fun = open_fun)
    TealDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data("ADSL"), n)) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )
  adsl_data <- example_data_connector(adsl)
  mtcars_ds1 <- TealDataset$new("cars1", head(mtcars), code = "cars1 <- head(mtcars)")
  data <- TealData$new(adsl_data, mtcars_ds1, check = TRUE)
  testthat::expect_identical(length(data$get_connectors()), 1L)
})

testthat::test_that("TealData$get_items returns a dataset of the passed dataset name", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_identical(data$get_items("cars"), mtcars_ds)
})

testthat::test_that("TealData$get_items returns the content of the passed TealDataConnector name", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal.data"))
    con <- TealDataConnection$new(open_fun = open_fun)
    TealDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data("ADSL"), n)) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )

  adsl_data <- example_data_connector(adsl)
  data <- TealData$new(adsl_data, check = TRUE)
  testthat::expect_identical(data$get_items("ADSL"), adsl_data$get_items()$ADSL)
})

testthat::test_that("TealData$get_items returns a list of the contents if no dataname is defined", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_identical(data$get_items(), list(cars = mtcars_ds))
})

testthat::test_that("TealData$get_items throws an error if the desired dataset is not found", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_error(data$get_items("iris"), "dataset iris not found")
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

testthat::test_that("TealData$get_join_keys returns an empty joinKeys if no join_keys are passed", {
  mtcars_ds <- TealDataset$new("cars", head(mtcars), code = "cars <- head(mtcars)")
  data <- TealData$new(mtcars_ds, check = TRUE)
  testthat::expect_equal(data$get_join_keys(), join_keys())
})

testthat::test_that("TealData$get_join_keys returns all join_keys when no input datanme is specified", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- TealData$new(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_equal(
    data$get_join_keys(),
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df2", "df1", "id")
    )
  )
})

testthat::test_that("TealData$get_join_keys returns all join_keys of the single dataname specified", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- TealData$new(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_equal(
    data$get_join_keys("df1"),
    list(df2 = setNames("id", "id"))
  )
})

testthat::test_that("TealData$get_join_keys returns the join_keys of the specified datanames", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- TealData$new(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_equal(
    data$get_join_keys("df1", "df2"),
    setNames("id", "id")
  )
})

testthat::test_that("TealData$get_parents returns an empty list even when parents are specified", {
  # Parent information is passed through the join_keys argument - this test does not create
  # TealData with such a join_keys object set (it is only set for you if you call teal_data/cdisc_data)
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df1 <- CDISCTealDataset$new("df1", df1, keys = "id", parent = "parent")
  data <- TealData$new(df1, check = FALSE)
  testthat::expect_equal(data$get_parents(), list())
})

testthat::test_that("TealData$mutate_join_keys returns a JoinKeys object with the updated join_keys", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- TealData$new(df1, df2, join_keys = jk, check = FALSE)

  data$mutate_join_keys("df1", "df2", "id2")
  updated_jks <- data$get_join_keys()
  testthat::expect_equal(
    updated_jks,
    join_keys(join_key("df1", "df2", "id2"))
  )
})

testthat::test_that("TealData$mutate_join_keys changes keys for both datasets (same key in both)", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))
  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")
  data <- TealData$new(df1, df2, check = FALSE)
  data$mutate_join_keys("df1", "df2", "id")

  testthat::expect_equal(
    data$get_join_keys(),
    join_keys(
      join_key("df1", "df2", "id")
    )
  )
})

test_that("TealData$check_metadata fails if inconsistent join_keys for given datasets", {
  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- TealData$new(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "w")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "y", "v" = "v")))
    )$check_metadata()
  )

  expect_error(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "x")))
    )$check_metadata()
  )
})

test_that("TealData$check_metadata does not produce error if join_keys are consistent for given datasets", {
  df_1 <- data.frame(x = 1:10, y = 1:10)
  df_2 <- data.frame(u = 1:10, v = 1:10)

  constructor_wrapper <- function(join_keys) {
    data <- TealData$new(
      dataset("df_1", df_1),
      dataset("df_2", df_2),
      join_keys = join_keys
    )
  }

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys()
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_1", "df_2", c("x" = "u", "y" = "v")))
    )$check_metadata()
  )

  expect_silent(
    constructor_wrapper(
      join_keys = join_keys(join_key("df_2", "df_2", c("u" = "u")))
    )$check_metadata()
  )
})

testthat::test_that("TealData$check_metadata returns error when a column in the keys is not found", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id6")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- TealData$new(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_error(
    data$check_metadata(),
    "Primary keys specifed for df1 do not exist in the data."
  )
})

# TealData with single dataset and connector ----
testthat::test_that("TealData with single dataset and connector", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal.data"))
    con <- TealDataConnection$new(open_fun = open_fun)
    TealDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = function(ADSL, n = 5) ADSL <- head(teal.data::example_cdisc_data("ADSL"), n)) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )
  adsl_data <- example_data_connector(adsl)

  adtte <- dataset(
    dataname = "ADTTE",
    x = teal.data::example_cdisc_data("ADTTE"),
    code = "ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")"
  )

  data <- TealData$new(adsl_data, adtte)
  items <- data$get_items()
  testthat::expect_length(items, 2)
  testthat::expect_true(inherits(data, "TealData"))
  testthat::expect_true(inherits(items$ADSL, "TealDatasetConnector"))
  testthat::expect_true(inherits(items$ADTTE, "TealDataset"))

  connectors <- data$get_connectors()
  testthat::expect_length(connectors, 1)
  testthat::expect_true(inherits(connectors[[1]], "TealDataConnector"))

  testthat::expect_equal(
    items$ADSL$get_pull_callable()$get_call(),
    "(function(ADSL, n = 5) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)"
  )

  testthat::expect_identical(adtte$get_raw_data(), items$ADTTE$get_raw_data())

  # simulate pull with a click of the submit button
  for (connector in data$get_connectors()) {
    connector$pull()
  }

  testthat::expect_equal(
    data$get_code("ADSL"),
    paste0(
      "library(package = \"teal.data\")\n",
      "ADSL <- (function(ADSL, n = 5) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)"
    )
  )
  testthat::expect_equal(
    data$get_code("ADTTE"),
    paste0(
      "library(package = \"teal.data\")\n",
      "ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")"
    )
  )

  testthat::expect_equal(
    data$get_code(),
    paste0(
      "library(package = \"teal.data\")\n",
      "ADSL <- (function(ADSL, n = 5) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\n",
      "ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")"
    )
  )
})

# TealData with mutliple datasets and connectors ----
testthat::test_that("TealData with mutliple datasets and connectors", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal"))
    con <- teal.data:::TealDataConnection$new(open_fun = open_fun)
    x <- teal.data:::TealDataConnector$new(connection = con, connectors = connectors)
    x$set_ui(
      function(id, connection, connectors) {
        ns <- NS(id)
        tagList(
          connection$get_open_ui(ns("open_connection")),
          do.call(
            what = "tagList",
            args = lapply(
              connectors,
              function(connector) {
                div(
                  connector$get_ui(
                    id = ns(connector$get_dataname())
                  ),
                  br()
                )
              }
            )
          )
        )
      }
    )
    return(x)
  }

  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data("ADSL"), n)) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )
  adsl_data <- example_data_connector(adsl)

  adtte <- dataset(
    dataname = "ADTTE",
    x = teal.data::example_cdisc_data("ADTTE"),
    code = "ADTTE <- teal.data::example_cdisc_data(\"ADTTE\")"
  )

  adsl_2 <- code_dataset_connector("ADSL_2", "ADSL", keys = get_cdisc_keys("ADSL"), ADSL = adsl)
  # add custom input
  adsl_2$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "Example UI", min = 0, value = 2)
    )
  })

  advs <- dataset_connector(
    dataname = "ADVS",
    pull_callable = callable_function(fun = function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data("ADVS"), n)) %>% # nolint
      set_args(list(ADVS = as.name("ADVS"))),
    keys = get_cdisc_keys("ADVS"),
    label = "ADVS connector"
  )
  advs$set_ui_input(function(ns) {
    list(
      numericInput(inputId = ns("seed"), label = "Example UI", min = 0, value = 4)
    )
  })

  adlb <- dataset_connector(
    dataname = "ADLB",
    pull_callable = callable_function(fun = function(ADLB, n) ADLB <- head(teal.data::example_cdisc_data("ADLB"), n)) %>% # nolint
      set_args(list(ADLB = as.name("ADLB"))),
    keys = get_cdisc_keys("ADLB"),
    label = "ADLB connector"
  )

  advs_adlb_data <- example_data_connector(advs, adlb)

  temp_file <- tempfile()
  saveRDS(rADRS, file = temp_file)
  adrs <- rds_dataset_connector(dataname = "ADRS", file = temp_file)

  adsamp <- script_dataset_connector(
    dataname = "ADSAMP",
    keys = get_cdisc_keys("ADVS"),
    file = "delayed_data_script/asdamp_with_adsl.R",
    ADSL = adsl,
    ADVS = advs
  )

  data <- TealData$new(adsl_data, adtte, adsl_2, advs_adlb_data, adrs, adsamp)

  testthat::expect_true(inherits(data, "TealData"))
  items <- data$get_items()
  testthat::expect_true(all(vapply(items[-2], inherits, logical(1), "TealDatasetConnector")))
  testthat::expect_true(inherits(items$ADTTE, "TealDataset"))

  testthat::expect_equal(
    items$ADSL$get_pull_callable()$get_call(),
    "(function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)"
  )
  testthat::expect_equal(items$ADSL_2$get_pull_callable()$get_call(), "ADSL")
  testthat::expect_equal(
    items$ADVS$get_pull_callable()$get_call(),
    "(function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)"
  )
  testthat::expect_equal(
    items$ADLB$get_pull_callable()$get_call(),
    "(function(ADLB, n) ADLB <- head(teal.data::example_cdisc_data(\"ADLB\"), n))(ADLB = ADLB)"
  )
  testthat::expect_equal(
    items$ADSAMP$get_pull_callable()$get_call(),
    "source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
  )
  testthat::expect_identical(adtte$get_raw_data(), items$ADTTE$get_raw_data())

  testthat::expect_equal(
    data$get_code("ADSL"),
    "library(package = \"teal\")\nADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADSL_2"),
    paste0(
      "library(package = \"teal\")\n",
      "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\n",
      "ADSL_2 <- ADSL"
    )
  )
  testthat::expect_equal(
    data$get_code("ADVS"),
    "library(package = \"teal\")\nADVS <- (function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADLB"),
    "library(package = \"teal\")\nADLB <- (function(ADLB, n) ADLB <- head(teal.data::example_cdisc_data(\"ADLB\"), n))(ADLB = ADLB)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADSAMP"),
    paste0(
      "library(package = \"teal\")\n",
      "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\n",
      "ADVS <- (function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)\n",
      "ADSAMP <- source(file = \"delayed_data_script/asdamp_with_adsl.R\", local = TRUE)$value"
    )
  )
  testthat::expect_equal(
    data$get_code("ADTTE"),
    "library(package = \"teal\")\nADTTE <- teal.data::example_cdisc_data(\"ADTTE\")" # nolint
  )
})

# Multiple connectors ----
testthat::test_that("Multiple connectors", {
  example_data_connector <- function(...) {
    connectors <- list(...)
    open_fun <- callable_function(library)
    open_fun$set_args(list(package = "teal"))
    con <- TealDataConnection$new(open_fun = open_fun)
    TealDataConnector$new(connection = con, connectors = connectors)
  }

  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data("ADSL"), n)) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )

  adae <- dataset_connector(
    dataname = "ADAE",
    pull_callable = callable_function(fun = function(ADAE, n) ADAE <- head(teal.data::example_cdisc_data("ADAE"), n)) %>% # nolint
      set_args(list(ADAE = as.name("ADAE"))),
    keys = get_cdisc_keys("ADAE"),
    label = "ADAE connector"
  )

  advs <- dataset_connector(
    dataname = "ADVS",
    pull_callable = callable_function(fun = function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data("ADVS"), n)) %>% # nolint
      set_args(list(ADVS = as.name("ADVS"))),
    keys = get_cdisc_keys("ADVS"),
    label = "ADVS connector"
  )

  adsl_2 <- code_dataset_connector("ADSL_2",
    code = "ADSL",
    keys = get_cdisc_keys("ADSL"), ADSL = adsl
  )
  adsl_adae <- example_data_connector(adsl, adae)
  advs_adsl_2 <- example_data_connector(advs, adsl_2)
  data <- TealData$new(adsl_adae, advs_adsl_2)

  items <- data$get_items()
  testthat::expect_true(inherits(data, "TealData"))
  testthat::expect_true(all(vapply(items, inherits, logical(1), "TealDatasetConnector")))
  testthat::expect_true(all(vapply(data$get_connectors(), inherits, logical(1), "TealDataConnector")))

  testthat::expect_equal(names(items), c("ADSL", "ADAE", "ADVS", "ADSL_2"))

  testthat::expect_equal(
    items$ADSL$get_code(),
    "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)"
  )
  testthat::expect_equal(
    items$ADAE$get_code(),
    "ADAE <- (function(ADAE, n) ADAE <- head(teal.data::example_cdisc_data(\"ADAE\"), n))(ADAE = ADAE)"
  )
  testthat::expect_equal(
    items$ADVS$get_code(),
    "ADVS <- (function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)"
  )
  testthat::expect_equal(
    items$ADSL_2$get_code(),
    "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\nADSL_2 <- ADSL"
  )

  testthat::expect_equal(
    data$get_code("ADSL"),
    "library(package = \"teal\")\nADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADAE"),
    "library(package = \"teal\")\nADAE <- (function(ADAE, n) ADAE <- head(teal.data::example_cdisc_data(\"ADAE\"), n))(ADAE = ADAE)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADVS"),
    "library(package = \"teal\")\nADVS <- (function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)" # nolint
  )
  testthat::expect_equal(
    data$get_code("ADSL_2"),
    paste0(
      "library(package = \"teal\")\n",
      "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\n",
      "ADSL_2 <- ADSL"
    )
  )
  testthat::expect_equal(
    data$get_code(),
    paste0(
      "library(package = \"teal\")\n",
      "ADSL <- (function(ADSL, n) ADSL <- head(teal.data::example_cdisc_data(\"ADSL\"), n))(ADSL = ADSL)\n",
      "ADAE <- (function(ADAE, n) ADAE <- head(teal.data::example_cdisc_data(\"ADAE\"), n))(ADAE = ADAE)\n",
      "ADVS <- (function(ADVS, n) ADVS <- head(teal.data::example_cdisc_data(\"ADVS\"), n))(ADVS = ADVS)\n",
      "ADSL_2 <- ADSL"
    )
  )
})
