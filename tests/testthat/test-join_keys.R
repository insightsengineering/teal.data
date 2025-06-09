# join_keys --------------------------------------------------------------------
testthat::test_that("join_keys creates empty join_keys object by default", {
  testthat::expect_s3_class(join_keys(), "join_keys")
})

testthat::test_that("join_keys only accepts teal_data and join_key arguments", {
  key <- join_key("a", "b", "test")
  testthat::expect_no_error(join_keys(key))
  testthat::expect_no_error(join_keys(teal_data()))
  testthat::expect_error(
    join_keys("a"),
    "Assertion .* failed: May only contain the following types:"
  )
})

testthat::test_that("join_keys doesn't accept a list which is identical to output of join_key function", {
  key <- join_key("a", "b", "test")
  testthat::expect_error(
    join_keys(unclass(key)),
    "Assertion .* failed: May only contain the following types:"
  )
})

testthat::test_that("join_keys.join_key creates join_keys", {
  testthat::expect_s3_class(
    join_keys(
      join_key("d1", keys = "test"),
      join_key("d2", keys = "test")
    ),
    c("join_keys", "list")
  )
})

testthat::test_that("join_keys accepts duplicated join_key", {
  testthat::expect_no_error(
    join_keys(join_key("d1", "d2", "a"), join_key("d1", "d2", "a"))
  )
})

testthat::test_that("join_keys accepts duplicated join_key (undirected)", {
  testthat::expect_no_error(
    join_keys(join_key("d1", "d2", "a", directed = FALSE), join_key("d1", "d2", "a", directed = FALSE))
  )
})

testthat::test_that("join_keys is a collection of join_key, ie named list with named list with named char vector", {
  key1 <- join_key("d1", keys = "test")
  key2 <- join_key("d2", keys = "test")
  jk <- join_keys(key1, key2)

  testthat::expect_identical(
    jk,
    structure(c(key1, key2), class = c("join_keys", "list"))
  )

  testthat::expect_identical(
    jk,
    structure(
      list(
        d1 = list(d1 = c(test = "test")),
        d2 = list(d2 = c(test = "test"))
      ),
      class = c("join_keys", "list"),
      "parents" = list()
    )
  )

  testthat::expect_identical(
    jk,
    structure(
      list(
        d1 = list(d1 = c(test = "test")),
        d2 = list(d2 = c(test = "test"))
      ),
      parents = list(),
      class = c("join_keys", "list")
    )
  )
})

testthat::test_that("join_keys cannot create acyclical graph", {
  testthat::expect_error(
    join_keys(
      join_key("d1", "d2", "A"),
      join_key("d2", "d1", "A")
    ),
    "Cycle detected in a parent and child dataset graph"
  )
})

testthat::test_that("join_keys.teal_data returns join_keys object from teal_data", {
  obj <- teal_data(join_keys = join_keys(join_key("d1", "d1", "a")))
  testthat::expect_identical(obj@join_keys, join_keys(obj))
})

testthat::test_that("join_keys.join_keys returns itself", {
  obj <- join_keys(join_key("d1", "d1", "a"))
  testthat::expect_identical(obj, join_keys(obj))
})

testthat::test_that("join_keys constructor adds symmetric keys on given (unnamed) foreign key", {
  my_keys <- join_keys(join_key("d1", "d2", "a"))
  testthat::expect_identical(
    my_keys,
    structure(
      list(
        d1 = list(d2 = c(a = "a")),
        d2 = list(d1 = c(a = "a"))
      ),
      parents = list(d2 = "d1"),
      class = c("join_keys", "list")
    )
  )
})

testthat::test_that("join_keys constructor adds symmetric keys on given (named) foreign key", {
  my_keys <- join_keys(join_key("d2", "d1", c(b = "a"), directed = FALSE))
  parents(my_keys) <- list(d2 = "d1")

  testthat::expect_equal(
    my_keys,
    structure(
      list(
        d2 = list(d1 = c(b = "a")),
        d1 = list(d2 = c(a = "b"))
      ),
      parents = list(d2 = "d1"),
      class = c("join_keys", "list")
    )
  )
})

# join_keys.<- ----------------------------------------------------------------
testthat::test_that("join_keys<-.join_keys overwrites existing join_keys", {
  my_keys <- join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  join_keys(my_keys) <- join_keys(join_key("d1", "d1", "test"))
  testthat::expect_identical(my_keys, join_keys(join_key("d1", "d1", "test")))
})

testthat::test_that("join_keys<-.teal_data overwrites existing join_keys", {
  td <- teal_data(
    d1 = data.frame(), d2 = data.frame(),
    join_keys = join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )

  jk2 <- join_keys(join_key("d1", "d1", "test"))
  join_keys(td) <- jk2
  testthat::expect_identical(join_keys(td), jk2)
})

testthat::test_that("join_keys<-.teal_data overwrites existing join_keys", {
  td <- teal_data(
    d1 = data.frame(), d2 = data.frame(),
    join_keys = join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )

  jk2 <- join_keys(join_key("d1", "d1", "test"))
  join_keys(td) <- jk2
  testthat::expect_identical(join_keys(td), jk2)
})

testthat::test_that("join_keys()[]<-.join_keys with empty name is changed to the key value", {
  jk <- join_keys()
  join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", "C")
  testthat::expect_equal(jk[["d1"]][["d2"]], c(A = "B", C = "C"))
})

testthat::test_that("join_keys()[]<-.join_keys with named empty valued is changed to its name", {
  jk <- join_keys()
  join_keys(jk)[["d1"]][["d2"]] <- c(A = "B", C = "")
  testthat::expect_equal(jk[["d1"]][["d2"]], c(A = "B", C = "C"))
})

testthat::test_that("join_keys()[]<-.join_keys with empty value in a named vector are ignored ", {
  jk <- join_keys()
  testthat::expect_message(join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  testthat::expect_equal(jk[["d1"]][["d2"]], c(A = "B"))
})
