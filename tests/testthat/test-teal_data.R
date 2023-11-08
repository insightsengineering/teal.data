teal_data_mixed_call <- function(check = TRUE, join_keys1 = join_keys()) {
  df1 <- data.frame(AA = c(1, 2, 3), BB = c("A", "B", "C"))
  df2 <- data.frame(AA = c(4, 5, 6), BB = c("A", "B", "C"))
  df3 <- data.frame(AA = c(7, 8, 9), BB = c("A", "B", "C"))

  df1_ds <- dataset(
    dataname = "df1",
    x = df1,
    code = "df1 <- data.frame(AA = c(1,2,3), BB = c(\"A\", \"B\", \"C\"))"
  )

  df2_cf <- callable_function(
    function() {
      data.frame(A = c(4, 5, 6), BB = c("A", "B", "C"))
    }
  )
  df2_dc <- dataset_connector("df2", df2_cf)

  df3_cf <- callable_function(
    function() {
      data.frame(AA = c(7, 8, 9), BB = c("A", "B", "C"))
    }
  )
  df3_cdc <- dataset_connector("df3", df3_cf)
  df3_rdc <- relational_data_connector(
    connection = data_connection(open_fun = callable_function(function() "open function")),
    connectors = list(df3_cdc)
  )

  load_dataset(df1_ds)
  load_dataset(df2_dc)
  load_dataset(df3_cdc)

  teal_data(df1_ds, df2_dc, df3_rdc, check = check, join_keys = join_keys1)
}

testthat::test_that("teal_data allows to initialize empty teal_data object", {
  testthat::expect_s4_class(teal_data(), "teal_data")
})

testthat::test_that("empty teal_data returns empty code, id, wartnings and messages", {
  testthat::expect_identical(teal_data()@code, character(0))
  testthat::expect_identical(teal_data()@id, integer(0))
  testthat::expect_identical(teal_data()@messages, character(0))
  testthat::expect_identical(teal_data()@warnings, character(0))
})

testthat::test_that("teal_data allows to initialize empty teal_data with join_keys", {
  testthat::expect_equal(
    teal_data(join_keys = join_keys(join_key("data1", "data2", "id")))@join_keys,
    join_keys(join_key("data1", "data2", "id"))
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from passed objects", {
  testthat::expect_identical(
    teal_data(iris = iris, mtcars = mtcars)@datanames,
    c("iris", "mtcars")
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from passed join_keys", {
  testthat::expect_identical(
    teal_data(join_keys = join_keys(join_key("parent", "child", "id")))@datanames,
    c("parent", "child")
  )
})

testthat::test_that("teal_data initializes teal_data object with @datanames taken from join_keys and passed objects", {
  testthat::expect_identical(
    teal_data(iris = iris, join_keys = join_keys(join_key("parent", "child", "id")))@datanames,
    c("iris", "parent", "child")
  )
})

testthat::test_that("teal_data accepts TealDataset, TealDatasetConnector, TealDataConnector objects", {
  testthat::expect_silent(data <- teal_data_mixed_call())
  testthat::expect_identical(data$get_datanames(), c("df1", "df2", "df3"))
})

testthat::test_that("teal_data returns teal_data when data passed as named list", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  testthat::expect_s4_class(teal_data(df1 = df1), "teal_data")
})

testthat::test_that("teal_data accepts any data provided as named list", {
  df1 <- structure(1L, class = "anyclass")
  testthat::expect_no_error(teal_data(df1 = df1))
})

testthat::test_that("teal_data accepts code as character", {
  testthat::expect_no_error(
    teal_data(
      iris1 = iris,
      code = "iris1 <- iris"
    )
  )
})

testthat::test_that("teal_data accepts code as language", {
  testthat::expect_no_error(
    teal_data(
      iris1 = iris,
      code = quote(iris1 <- iris)
    )
  )
})

testthat::test_that("teal_data code unfolds code-block wrapped in '{'", {
  testthat::expect_identical(
    teal_data(iris1 = iris, code = quote({
      iris1 <- iris
    }))@code,
    "iris1 <- iris"
  )
})

testthat::test_that("teal_data code is concatenated into single string", {
  testthat::expect_identical(
    teal_data(iris1 = iris, code = c("iris1 <- iris", "iris2 <- iris1"))@code,
    "iris1 <- iris\niris2 <- iris1"
  )
})

testthat::test_that("teal_data@env is locked. Not able to modify, add or remove bindings", {
  data <- teal_data(iris = iris)
  testthat::expect_error(data@env$iris <- iris, "cannot change value of locked binding for 'iris'")
  testthat::expect_error(data@env$iris2 <- iris, "cannot add bindings to a locked environment")
  testthat::expect_error(rm("iris", envir = data@env), "cannot remove bindings from a locked environment")
})

testthat::test_that("teal_data sets passed join_keys to datasets correctly", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- teal_data(df1, df2, join_keys = jk, check = FALSE)

  jk_expected <- join_keys(
    join_key("df1", "df2", "id"),
    join_key("df1", "df1", "id"),
    join_key("df2", "df2", "df2_id")
  )
  parents(jk_expected) <- list(df1 = character(0), df2 = character(0))

  testthat::expect_equal(join_keys(data), jk_expected)
})

testthat::test_that("teal_data sets passed join_keys to datasets correctly when key names differ", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), b = c(1L, 2L))
  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")
  jk <- join_keys(join_key("df1", "df2", c(id = "fk")))
  data <- teal_data(df1, df2, join_keys = jk, check = FALSE)

  jk_expected <- join_keys(
    join_key("df1", "df2", c(id = "fk")),
    join_key("df1", "df1", "id"),
    join_key("df2", "df1", c(fk = "id")),
    join_key("df2", "df2", "df2_id")
  )
  parents(jk_expected) <- list(df1 = character(0), df2 = character(0))

  testthat::expect_equal(join_keys(data), jk_expected)
})

testthat::test_that("teal_data sets passes join_keys to datasets correctly when key names differ (multiple keys)", {
  df1 <- data.frame(id = c("A", "B"), id2 = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), fk2 = c("A", "B"), b = c(1L, 2L))
  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")
  data <- teal_data(df1, df2, check = FALSE)
  data$mutate_join_keys("df1", "df2", c(id = "fk", id2 = "fk2"))

  jk_expected <- join_keys(
    join_key("df1", "df1", "id"),
    join_key("df2", "df2", "df2_id"),
    join_key("df1", "df2", c(id = "fk", id2 = "fk2"))
  )
  parents(jk_expected) <- list(df1 = character(0), df2 = character(0))
  testthat::expect_equal(join_keys(data), jk_expected)
})

testthat::test_that("teal_data returns TealData object with cdisc_dataset input", {
  dummy_adsl <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADSL"))))
  adsl <- cdisc_dataset("ADSL", dummy_adsl)
  dummy_adtte <- as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADTTE"))))
  adtte <- cdisc_dataset("ADTTE", dummy_adtte)
  ds2 <- dataset("ds", iris)

  dummy_mae <- head(iris)
  class(dummy_mae) <- "MultiAssayExperiment"

  mae <- MAETealDataset$new("MAE", dummy_mae)

  mixed_data <- teal_data(mae, adsl, adtte, ds2)
  testthat::expect_equal(class(mixed_data), c("TealData", "TealDataAbstract", "R6"))

  mae_only <- teal_data(mae)
  testthat::expect_equal(class(mae_only), c("TealData", "TealDataAbstract", "R6"))

  dataset_only <- teal_data(ds2)
  testthat::expect_equal(class(dataset_only), c("TealData", "TealDataAbstract", "R6"))

  mae_and_dataset <- teal_data(mae, ds2)
  testthat::expect_equal(class(mae_and_dataset), c("TealData", "TealDataAbstract", "R6"))

  cdisc_only <- teal_data(adsl, adtte)
  testthat::expect_equal(class(cdisc_only), c("TealData", "TealDataAbstract", "R6"))
})

testthat::test_that("teal_data_file loads the TealData object", {
  tmp_file <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "df <- data.frame(A = c(1, 2, 3))
    df1_ds <- dataset('df', df, code = 'df <- data.frame(A = c(1, 2, 3))')
    teal_data(df1_ds)
    "
    ),
    con = tmp_file
  )
  tdf <- teal_data_file(tmp_file)
  file.remove(tmp_file)
  testthat::expect_s3_class(tdf, "TealData")
  testthat::expect_identical(
    tdf$get_code(),
    paste0(
      "df <- data.frame(A = c(1, 2, 3))\n",
      "df1_ds <- dataset(\"df\", df, code = \"df <- data.frame(A = c(1, 2, 3))\")\n",
      "teal_data(df1_ds)"
    )
  )
})

testthat::test_that("teal_data_file uses the code input to mutate the code of the loaded TealData object", {
  tmp_file <- tempfile(fileext = ".R")
  writeLines(
    text = c(
      "df <- data.frame(A = c(1, 2, 3))
    df1_ds <- dataset('df', df, code = 'df <- data.frame(A = c(1, 2, 3))')
    teal_data(df1_ds)
    "
    ),
    con = tmp_file
  )
  tdf <- teal_data_file(tmp_file, "# MUTATE code")
  file.remove(tmp_file)
  testthat::expect_identical(tdf$get_code(), "df <- data.frame(A = c(1, 2, 3))\n# MUTATE code")
})

testthat::test_that("update_join_keys_to_primary updates the join_keys", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jks <- join_keys(join_key("df1", "df2", "id"))
  data_objects <- list(df1, df2)

  jks <- update_join_keys_to_primary(data_objects, jks)
  testthat::expect_equal(
    jks,
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df1", "df1", "id"),
      join_key("df2", "df2", "df2_id")
    )
  )
})

testthat::test_that("update_join_keys_to_primary updates the join_keys when primary keys exist", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jks <- join_keys(join_key("df1", "df2", "id"))
  data_objects <- list(df1, df2)

  jks <- update_join_keys_to_primary(data_objects, jks)
  testthat::expect_equal(
    jks,
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df1", "df1", "id"),
      join_key("df2", "df2", "df2_id")
    )
  )
})

testthat::test_that("update_join_keys_to_primary updates join_keys with character(0) when no primary keys exist", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1)
  df2 <- dataset("df2", df2)

  jks <- join_keys(join_key("df1", "df2", "id"))
  data_objects <- list(df1, df2)

  jks <- update_join_keys_to_primary(data_objects, jks)
  testthat::expect_equal(
    jks,
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df1", "df1", character(0)),
      join_key("df2", "df2", character(0))
    )
  )
})
