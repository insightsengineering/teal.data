test_that("cdisc_join_keys merges joins keys with CDISC default join_keys", {
  result <- cdisc_join_keys(join_key("dataset_A", "dataset_B", c("col_1" = "col_a")), "ADTTE")

  expect_named(result, c("ADSL", "ADTTE", "dataset_A", "dataset_B"), ignore.order = TRUE)
  expect_identical(result[["dataset_B"]][["dataset_A"]], c("col_a" = "col_1"))
})

test_that("cdisc_join_keys will generate join_keys for named list with non-named elements", {
  new_dataset <- cdisc_join_keys("ADSL", ADTTE = rADTTE)
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk[["ADSL"]][["ADSL"]]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk[["ADTTE"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk[["ADSL"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk[["ADTTE"]][["ADSL"]]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate join_keys for character list", {
  new_dataset <- cdisc_join_keys("ADSL", "ADTTE")
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk[["ADSL"]][["ADSL"]]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk[["ADTTE"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk[["ADSL"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk[["ADTTE"]][["ADSL"]]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate join_keys for named list", {
  new_dataset <- cdisc_join_keys(ADSL = rADSL, ADTTE = rADTTE)
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk[["ADSL"]][["ADSL"]]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk[["ADTTE"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk[["ADSL"]][["ADTTE"]]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk[["ADTTE"]][["ADSL"]]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will retrieve ADTTE primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  internal_keys <- default_cdisc_keys[["ADTTE"]]
  jk <- cdisc_join_keys("ADTTE")
  primary_keys <- unname(jk[["ADTTE"]][["ADTTE"]])

  expect_equal(primary_keys, internal_keys$primary)

  foreign_keys <- unname(jk[["ADTTE"]][[internal_keys$parent]])
  expect_equal(foreign_keys, internal_keys$foreign)
})

test_that("cdisc_join_keys will retrieve known primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  vapply(
    datasets,
    function(.x) {
      internal_keys <- default_cdisc_keys[[.x]]
      jk <- cdisc_join_keys(.x)
      primary_keys <- unname(jk[[.x]][[.x]])
      expect_equal(primary_keys, internal_keys$primary)
      if (!is.null(internal_keys$foreign)) {
        foreign_keys <- unname(jk[[.x]][[internal_keys$parent]])
        expect_equal(foreign_keys, internal_keys$foreign)
      }
      character(0)
    },
    character(0)
  )
})

test_that("cdisc_join_keys will set parents of datasets", {
  datasets <- names(default_cdisc_keys)

  vapply(
    datasets,
    function(.x) {
      jk <- cdisc_join_keys(.x)
      parent_name <- default_cdisc_keys[[.x]][["parent"]]
      if (!is.null(parent_name)) {
        expect_identical(parent(jk, .x), parent_name)
      }
      character(0)
    },
    character(0)
  )
})

test_that("cdisc_join_keys will retrieve known primary keys", {
  datasets <- names(default_cdisc_keys)

  vapply(
    datasets,
    function(.x) {
      jk <- cdisc_join_keys(.x)
      expect_equal(unname(jk[[.x]][[.x]]), get_cdisc_keys(.x))
      character(0)
    },
    character(0)
  )
})

test_that("cdisc_join_keys does nothing with TealDataset", {
  adae_cf <- callable_function(
    function() as.data.frame(as.list(setNames(nm = get_cdisc_keys("ADAE"))))
  )
  adae_cdc <- cdisc_dataset_connector("ADAE", adae_cf, keys = get_cdisc_keys("ADAE"))
  expect_length(join_keys(cdisc_join_keys(adae_cdc)), 0)
})

test_that("default_cdisc_join_keys can get a valid `join_keys` object", {
  ds1 <- sample(names(default_cdisc_keys), 3)
  result <- default_cdisc_join_keys[ds1]
  expect_gte(length(result), 3)
  expect_gte(length(parents(result)), 3)
})
