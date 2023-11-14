# -----------------------------------------------------------------------------
#
# parents()
#
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

# -----------------------------------------------------------------------------
#
# parents<-
#
testthat::test_that("parents<- accepts a named list containing (non-empty, non-missing) character", {
  jk <- join_keys(join_key("a", "b", "test"))
  testthat::expect_no_error(parents(jk) <- list(b = "a"))
})

testthat::test_that("parents<- single parent can be changed utilizing list functionality with [[<-", {
  jk <- join_keys(
    join_key("a", "b", "ab"),
    join_key("c", "d", "cd")
  )
  parents(jk)[["a"]] <- "b"
  parents(jk)[["c"]] <- "d"
  testthat::expect_identical(parents(jk), list(a = "b", c = "d"))
})

testthat::test_that("parents<- dataset can't be own parent", {
  jk <- join_keys(
    join_key("a", "b", "ab"),
    join_key("c", "d", "cd")
  )
  testthat::expect_error(parents(jk) <- list(a = "a"))
})

testthat::test_that("parents<- ensures it is a directed acyclical graph (DAG)", {
  cyclic_jk <- join_keys(
    join_key("a", "b", "id"),
    join_key("b", "c", "id"),
    join_key("c", "a", "id")
  )
  testthat::expect_error(
    parents(cyclic_jk) <- list(a = "b", b = "c", c = "a"),
    "Cycle detected"
  )
})

testthat::test_that("parents<- single parent can be changed utilizing list functionality with [[<-", {
  jk <- join_keys(
    join_key("a", "b", "ab"),
    join_key("c", "d", "cd")
  )
  parents(jk)[["a"]] <- "b"
  parents(jk)[["c"]] <- "d"

  testthat::expect_identical(parents(jk), list(a = "b", c = "d"))
})

testthat::test_that("parents<- fails when value isn't a list (non-empty, non-missing) character", {
  jk <- join_keys(join_key("a", "b", "test"))
  # testthat::expect_error(parents(jk) <- list(b = character(0))) # todo: make an assert
  testthat::expect_error(parents(jk) <- list(b = 1))
  testthat::expect_error(parents(jk) <- list(b = NA_character_))
  testthat::expect_error(parents(jk) <- list(b = NULL))
  testthat::expect_error(parents(jk) <- NULL)
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
