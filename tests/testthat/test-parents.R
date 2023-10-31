test_that("parents will return empty list when empty/not set", {
  jk <- new_join_keys()
  expect_identical(parents(jk), list())
})

test_that("parents will return empty NULL when there is no parent", {
  jk <- new_join_keys()
  expect_null(parents(jk)[["ds1"]])
})

test_that("parents<- will add to parents attribute using `[` notation", {
  jk <- new_join_keys()
  parents(jk)["ds1"] <- "ds2"
  parents(jk)["ds3"] <- "ds4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4"))
})

test_that("parents<- will add to parents attribute using `[[` notation", {
  jk <- new_join_keys()
  parents(jk)[["ds1"]] <- "ds2"
  parents(jk)[["ds3"]] <- "ds4"

  expect_length(parents(jk), 2)
  expect_identical(parents(jk), list(ds1 = "ds2", ds3 = "ds4"))
})

test_that("check_parent_child will detect empty keys", {
  jk <- new_join_keys()
  jk["ds1", "ds2"] <- character(0)
  parents(jk) <- list(ds1 = "ds2")
  expect_error(check_parent_child(jk))
})

test_that("check_parent_child will detect invalid key pairs", {
  jk <- new_join_keys()
  jk[["ds1"]][["ds2"]] <- "key1"
  jk[["ds2"]][["ds1"]] <- character(0)
  parents(jk) <- list(ds1 = "ds2")
  expect_error(check_parent_child(jk))

  jk <- new_join_keys()
  jk[["ds2"]][["ds1"]] <- "key1"
  jk[["ds1"]][["ds2"]] <- character(0)
  parents(jk) <- list(ds1 = "ds2")
  expect_error(check_parent_child(jk))
})

test_that("check_parent_child will skip empty JoinKeys", {
  jk <- new_join_keys()
  expect_silent(check_parent_child(jk))
})
