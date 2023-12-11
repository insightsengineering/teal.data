# get parents -----------------------------------------------------------------------------
testthat::test_that("parents will return empty list when empty/not set", {
  jk <- join_keys()
  testthat::expect_identical(parents(jk), list())
})

testthat::test_that("parents returns the same list as used in parents<-", {
  jk <- join_keys(join_key("a", "b", "ab"))
  parents <- list(b = "a")
  parents(jk) <- parents
  testthat::expect_identical(parents(jk), parents)
})

# set parents ----------------------------------------------------------------------------
testthat::test_that("parents<- accepts a named list containing (non-empty, non-missing) character", {
  jk <- join_keys(join_key("a", "b", "test"))
  testthat::expect_no_error(parents(jk) <- list(b = "a"))
})

testthat::test_that("parents<- single parent can be changed utilizing list functionality with [[<-", {
  jk <- join_keys(
    join_key("a", "b", "ab", parent = "none"),
    join_key("c", "d", "cd", parent = "none")
  )
  parents(jk)[["a"]] <- "b"
  parents(jk)[["c"]] <- "d"
  testthat::expect_identical(parents(jk), list(a = "b", c = "d"))
})

testthat::test_that("parents<- dataset can't be own parent", {
  jk <- join_keys(
    join_key("a", "b", "ab", parent = "none"),
    join_key("c", "d", "cd", parent = "none")
  )
  testthat::expect_error(parents(jk) <- list(a = "a"))
})

testthat::test_that("parents<- setting parent-child relationship fails when no foreign keys between datasets", {
  jk <- join_keys(
    join_key("a", "1", "aa", parent = "none"),
    join_key("b", "b", "bb", parent = "none")
  )
  testthat::expect_error(parents(jk) <- list(a = "b"))
})

testthat::test_that("parents<- ensures it is a directed acyclical graph (DAG)", {
  cyclic_jk <- join_keys(
    join_key("a", "b", "id", parent = "none"),
    join_key("b", "c", "id", parent = "none"),
    join_key("c", "a", "id", parent = "none")
  )
  testthat::expect_error(
    parents(cyclic_jk) <- list(a = "b", b = "c", c = "a"),
    "Cycle detected"
  )
})

testthat::test_that("parents<- single parent can be changed utilizing list functionality with [[<-", {
  jk <- join_keys(
    join_key("a", "b", "ab", parent = "none"),
    join_key("c", "d", "cd", parent = "none")
  )
  parents(jk)[["a"]] <- "b"
  parents(jk)[["c"]] <- "d"

  testthat::expect_identical(parents(jk), list(a = "b", c = "d"))
})

testthat::test_that("parents<- fails when value isn't a list (non-empty, non-missing) character", {
  jk <- join_keys(join_key("a", "b", "test", parent = "none"))
  testthat::expect_error(parents(jk) <- list(b = 1), "May only contain the following types")
  testthat::expect_error(parents(jk) <- list(b = NA_character_), "May not contain")
  testthat::expect_error(parents(jk) <- list(b = NULL), "May only contain the following types")
  testthat::expect_error(parents(jk) <- NULL, "Must be of type 'list'")
})

testthat::test_that("parents<- setting parents again overwrites previous state", {
  jk <- join_keys(join_key("a", "b", "test"), join_key("c", "d", "test"))
  parents(jk) <- list(a = "b")
  parents(jk) <- list(b = "a")
  testthat::expect_identical(parents(jk), list(b = "a"))
})

testthat::test_that("parents<- sets parent datasets to join_keys kept in teal_data", {
  td <- teal_data(
    a = data.frame(),
    b = data.frame(),
    join_keys = join_keys(join_key("a", "b", "test"))
  )
  parents(td) <- list(b = "a")
  testthat::expect_identical(parents(td), list(b = "a"))
})

testthat::test_that("parents<- setting parents changes join_keys object", {
  jk <- join_keys(join_key("a", "b", "ab"))
  jk2 <- jk
  parents <- list(a = "b")
  parents(jk) <- parents

  testthat::expect_failure(testthat::expect_identical(jk, jk2))
  # Relaxed comparison also fails
  testthat::expect_failure(testthat::expect_equal(jk, jk2))
})
