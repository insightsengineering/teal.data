test_that("parents will return empty list when empty/not set", {
  jk <- join_keys()
  expect_identical(parents(jk), list())
})

test_that("parents will return empty NULL when there is no parent", {
  jk <- join_keys()
  expect_null(parents(jk)[["ds1"]])
})

test_that("parents<- will add to parents attribute using `[` notation", {
  jk <- join_keys()
  parents(jk)["ds1"] <- "ds2"
  parents(jk)["ds3"] <- "ds4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4"))
})

test_that("parents<- will add to parents attribute using `[[` notation", {
  jk <- join_keys()
  parents(jk)[["ds1"]] <- "ds2"
  parents(jk)[["ds3"]] <- "ds4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4"))
})

test_that("parents<- does nothing with empty value", {
  jk <- join_keys()
  jk2 <- `parents<-`(jk)

  expect_length(parents(jk2), 0)
  expect_equal(jk, jk2)
})

test_that("parents<- will add to parents attribute using list", {
  jk <- join_keys()
  parents(jk) <- list(ds1 = "ds2", "ds3" = "ds4")

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4"))
})

test_that("parents<- will add to parents attribute using list, `[` and `[[` notation", {
  jk <- join_keys()
  parents(jk)[["ds1"]] <- "ds2"
  parents(jk) <- list(ds3 = "ds4", "ds5" = "ds6")
  parents(jk)["ds7"] <- "ds8"

  expect_length(parents(jk), 4)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4", ds5 = "ds6", ds7 = "ds8"))
})

test_that("assert_parent_child will detect empty keys", {
  jk <- join_keys()
  jk[["ds1"]][["ds2"]] <- character(0)
  parents(jk) <- list(ds1 = "ds2")
  expect_error(assert_parent_child(jk))
})

test_that("assert_parent_child will detect invalid key pairs", {
  jk <- join_keys()
  jk[["ds1"]][["ds2"]] <- "key1"
  jk[["ds2"]][["ds1"]] <- character(0)
  parents(jk) <- list(ds1 = "ds2")
  expect_error(assert_parent_child(jk))

  jk2 <- join_keys()
  jk2[["ds2"]][["ds1"]] <- "key1"
  jk2[["ds1"]][["ds2"]] <- character(0)
  parents(jk2) <- list(ds1 = "ds2")
  expect_error(assert_parent_child(jk2))
})

test_that("assert_parent_child will skip empty join_keys", {
  jk <- join_keys()
  expect_silent(assert_parent_child(jk))
})

testthat::test_that("parents<- throws error when overwriting the parent value with a different value", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_error(parents(jk) <- list(df1 = character(0), df2 = "df5"))
})

testthat::test_that("parents<- works when overwriting the parent value with the same value", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
})

testthat::test_that("parent returns the parent name of the dataset", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_identical(parent(jk, "df1"), character(0))
  testthat::expect_identical(parent(jk, "df2"), "df1")
})

testthat::test_that("parent returns NULL when dataset is not found or not passed", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_null(parent(jk))
  testthat::expect_null(parent(jk, "df3"))
})

testthat::test_that("get_parents returns a list of all parents", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_identical(parents(jk), list(df1 = character(0), df2 = "df1"))
})

testthat::test_that("parents returns an empty list when no parents are present", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_identical(parents(jk), list())
})

testthat::test_that("parents throws error when dataname input is provided", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  testthat::expect_error(parents(jk, "df1"), "unused argument \\(\"df1\"\\)")
})

# -----------------------------------------------------------------------------
#
# update_keys_given_parents
#

testthat::test_that("update_keys_given_parents does not update the join_keys when no presents are present", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "id")))
  jk <- update_keys_given_parents(jk)
  testthat::expect_equal(jk, join_keys(join_key("df1", "df2", c("id" = "id"))))
})

testthat::test_that("update_keys_given_parents updates the join_keys when presents are present", {
  jk <- join_keys()

  join_keys(jk) <- list(
    join_key("df1", "df1", c("id", "id2")),
    join_key("df1", "df2", c("id" = "id")),
    join_key("df1", "df3", c("id" = "id"))
  )

  parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
  jk <- update_keys_given_parents(jk)

  expected_jk <- join_keys(
    join_key("df1", "df1", c("id", "id2")),
    join_key("df1", "df2", c("id" = "id")),
    join_key("df1", "df3", c("id" = "id")),
    join_key("df2", "df2", c("id", "id2")),
    join_key("df2", "df3", c("id", "id2")),
    join_key("df3", "df3", c("id", "id2"))
  )
  parents(expected_jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
  testthat::expect_equal(jk, expected_jk)
})

# -----------------------------------------------------------------------------
#
# assert_parent_child

testthat::test_that("assert_parent_child does nothing if no parents are present", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df1", c("id" = "id")))
  testthat::expect_identical(parents(jk), list())
  testthat::expect_silent(assert_parent_child(jk))
})

testthat::test_that("assert_parent_child throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df1", c("id" = "id")))
  parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
  testthat::expect_error(
    assert_parent_child(jk),
    "No join keys from df2 to its parent \\(df1\\) and vice versa"
  )
})

testthat::test_that("assert_parent_child throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(
    join_key("df1", "df1", c("id" = "id"))
  )
  # Change class as trick to allow for corrupt join_keys
  class(jk) <- "list"
  jk[["df2"]][["df1"]] <- "id"
  class(jk) <- class(new_join_keys())
  parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
  testthat::expect_error(
    assert_parent_child(jk),
    "No join keys from df2 parent name \\(df1\\) to df2"
  )
})

testthat::test_that("assert_parent_child throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(
    join_key("df1", "df1", c("id" = "id"))
  )
  class(jk) <- "list"
  jk[["df1"]][["df2"]] <- "id"
  class(jk) <- class(new_join_keys())
  parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1")
  testthat::expect_error(
    assert_parent_child(jk),
    "No join keys from df2 to its parent \\(df1\\)"
  )
})
