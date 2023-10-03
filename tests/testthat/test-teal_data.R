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

testthat::test_that("teal_data accepts TealDataset, TealDatasetConnector, TealDataConnector objects", {
  testthat::expect_silent(data <- teal_data_mixed_call())
  testthat::expect_identical(data@datanames, c("df1", "df2", "df3"))
})

testthat::test_that("teal_data throws error if it receives undesired objects", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))

  testthat::expect_error(
    teal_data(df1, check = TRUE),
    "May only contain the following types: \\{TealDataset,TealDatasetConnector,TealDataConnector\\}"
  )
})

testthat::test_that("teal_data sets passed join_keys to datasets correctly", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jk <- join_keys(join_key("df1", "df2", "id"))
  data <- teal_data(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_equal(
    data@join_keys,
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df1", "df1", "id"),
      join_key("df2", "df2", "df2_id")
    )
  )
})

testthat::test_that("teal_data sets passed JoinKeys to datasets correctly when key names differ", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), b = c(1L, 2L))
  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")
  jk <- join_keys(join_key("df1", "df2", c(id = "fk")))
  data <- teal_data(df1, df2, join_keys = jk, check = FALSE)

  testthat::expect_equal(
    data@join_keys,
    join_keys(
      join_key("df1", "df2", c(id = "fk")),
      join_key("df1", "df1", "id"),
      join_key("df2", "df1", c(fk = "id")),
      join_key("df2", "df2", "df2_id")
    )
  )
})

testthat::test_that("teal_data sets passes JoinKeys to datasets correctly when key names differ (multiple keys)", {
  df1 <- data.frame(id = c("A", "B"), id2 = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), fk = c("A", "B"), fk2 = c("A", "B"), b = c(1L, 2L))
  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")
  data <- teal_data(df1, df2, check = FALSE)
  data@join_keys$mutate("df1", "df2", c(id = "fk", id2 = "fk2"))

  testthat::expect_equal(
    data@join_keys,
    join_keys(
      join_key("df1", "df1", "id"),
      join_key("df2", "df2", "df2_id"),
      join_key("df1", "df2", c(id = "fk", id2 = "fk2"))
    )
  )
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
  testthat::expect_s4_class(mixed_data, "teal_data")

  mae_only <- teal_data(mae)
  testthat::expect_s4_class(mae_only, "teal_data")

  dataset_only <- teal_data(ds2)
  testthat::expect_s4_class(dataset_only, "teal_data")

  mae_and_dataset <- teal_data(mae, ds2)
  testthat::expect_s4_class(mae_and_dataset, "teal_data")

  cdisc_only <- teal_data(adsl, adtte)
  testthat::expect_s4_class(cdisc_only, "teal_data")

  testthat::expect_error(
    teal_data()
  )
})

testthat::test_that("teal_data_file loads the teal_data object", {
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
  testthat::expect_s4_class(tdf, "teal_data")
  expected_code <- expression(
    df <- data.frame(A = c(1, 2, 3)),
    df <- data.frame(A = c(1, 2, 3)),
    df1_ds <- dataset("df", df, code = "df <- data.frame(A = c(1, 2, 3))"),
    teal_data(df1_ds)
  )
  testthat::expect_identical(
    deparse(tdf@code),
    deparse(expected_code)
  )
})

testthat::test_that("teal_data_file uses the code input to mutate the code of the loaded teal_data object", {
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
  tdf <- teal_data_file(tmp_file, "df2 <- df")
  file.remove(tmp_file)
  expected_code <- expression(
    df <- data.frame(A = c(1, 2, 3)),
    df2 <- df
  )
  testthat::expect_identical(
    deparse(tdf@code),
    deparse(expected_code)
  )
})

testthat::test_that("update_join_keys_to_primary updates the join_keys", {
  df1 <- data.frame(id = c("A", "B"), a = c(1L, 2L))
  df2 <- data.frame(df2_id = c("A", "B"), id = c("A", "B"), b = c(1L, 2L))

  df1 <- dataset("df1", df1, keys = "id")
  df2 <- dataset("df2", df2, keys = "df2_id")

  jks <- join_keys(join_key("df1", "df2", "id"))
  data_objects <- list(df1, df2)

  update_join_keys_to_primary(data_objects, jks)
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

  update_join_keys_to_primary(data_objects, jks)
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

  update_join_keys_to_primary(data_objects, jks)
  testthat::expect_equal(
    jks,
    join_keys(
      join_key("df1", "df2", "id"),
      join_key("df1", "df1", character(0)),
      join_key("df2", "df2", character(0))
    )
  )
})
