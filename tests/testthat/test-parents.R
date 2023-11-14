# -----------------------------------------------------------------------------
#
# parents()
#

test_that("parents will return empty list when empty/not set", {
  jk <- join_keys()
  expect_identical(parents(jk), list())
})

test_that("parents will return empty NULL when there is no parent", {
  jk <- join_keys()
  expect_null(parents(jk)[["d1"]])
})

testthat::test_that("parents returns a list of all parents", {
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
# parents<-
#

test_that("parents<- does nothing with empty value", {
  jk <- join_keys()
  jk2 <- `parents<-`(jk)

  expect_length(parents(jk2), 0)
  expect_equal(jk, jk2)
})

test_that("parents<- will fail if datasets don't exist", {
  jk <- join_keys()
  expect_error(parents(jk)["d1"] <- "d2")
  expect_error(parents(jk)["d3"] <- "d4")
})

test_that("parents<- will add to parents attribute using `[` notation", {
  jk <- join_keys(
    join_key("d1", "d2", "k"),
    join_key("d3", "d4", "q")
  )
  parents(jk)["d1"] <- "d2"
  parents(jk)["d3"] <- "d4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(d1 = "d2", d3 = "d4"))
})

test_that("parents<- will add to parents attribute using `[[` notation", {
  jk <- join_keys(
    join_key("d1", "d2", "k"),
    join_key("d3", "d4", "q")
  )
  parents(jk)[["d1"]] <- "d2"
  parents(jk)[["d3"]] <- "d4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(d1 = "d2", d3 = "d4"))
})

test_that("parents<- will add to parents attribute using list", {
  jk <- join_keys(
    join_key("d1", "d2", "k"),
    join_key("d3", "d4", "q")
  )
  parents(jk) <- list(d1 = "d2", "d3" = "d4")

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(d1 = "d2", d3 = "d4"))
})

test_that("parents<- ensures it is a directed acyclical graph (DAG)", {
  cyclic_jk <- join_keys(
    join_key("a", "b", "id"),
    join_key("b", "c", "id"),
    join_key("c", "a", "id")
  )
  expect_error(
    parents(cyclic_jk) <- list(
      a = "b",
      b = "c",
      c = "a"
    ),
    "Cycle detected"
  )
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

# -----------------------------------------------------------------------------
#
# parent()
#

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

# -----------------------------------------------------------------------------
#
# assert_parent_child errors

test_that("parents<-.join_keys (assert_parent_child) will detect empty keys", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- character(0)
  expect_error(
    parents(jk) <- list(d1 = "d2"),
    "No join keys from .* to its parent .* and vice versa"
  )
})

test_that("parents<-.join_keys (assert_parent_child) will detect invalid key pairs", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- "key1"
  jk[["d2"]][["d1"]] <- character(0)
  expect_error(
    parents(jk) <- list(d1 = "d2"),
    "No join keys from .* to its parent .* and vice versa"
  )

  jk2 <- join_keys()
  jk2[["d2"]][["d1"]] <- "key1"
  jk2[["d1"]][["d2"]] <- character(0)
  expect_error(
    parents(jk2) <- list(d1 = "d2"),
    "No join keys from .* to its parent .* and vice versa"
  )
})

testthat::test_that("parents<-.join_keys (assert_parent_child) throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df1", c("id" = "id")))
  testthat::expect_error(
    parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1"),
    "No join keys from df2 to its parent \\(df1\\) and vice versa"
  )
})

testthat::test_that("parents<-.join_keys (assert_parent_child) throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(
    join_key("df1", "df1", c("id" = "id"))
  )
  # Change class as trick to allow for corrupt join_keys
  class(jk) <- "list"
  jk[["df2"]][["df1"]] <- "id"
  class(jk) <- class(new_join_keys())
  testthat::expect_error(
    parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1"),
    "No join keys from df2 parent name \\(df1\\) to df2"
  )
})

testthat::test_that("parents<-.join_keys (assert_parent_child) throws error if no join_keys exist for child-parent", {
  jk <- join_keys()
  join_keys(jk) <- list(
    join_key("df1", "df1", c("id" = "id"))
  )
  class(jk) <- "list"
  jk[["df1"]][["df2"]] <- "id"
  class(jk) <- class(new_join_keys())
  expect_error(
    parents(jk) <- list(df1 = character(0), df2 = "df1", df3 = "df1"),
    "No join keys from df2 to its parent \\(df1\\)"
  )
})
