# [.join_keys -----------------------------------------------------------------
testthat::test_that("[[.join_keys returns keys for given pair", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_identical(my_keys["b", "a"], c(`child-parent` = "child-parent"))
})

testthat::test_that("[[.join_keys doesn't return keys for given a pair without explicit join_key", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_null(my_keys[["b"]][["c"]])
})

testthat::test_that("[[.join_keys infer keys between children by equal (unordered) foreign keys to parent", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", sample(letters[1:5])),
    join_key("c", "a", sample(letters[1:5]))
  )
  parents(my_keys) <- list("b" = "a", "c" = "a")
  testthat::expect_identical(my_keys["b", "c"], setNames(letters[1:5], letters[1:5]))
})

testthat::test_that(
  "[[.join_keys infer keys between children by foreign keys to common parent. ",
  {
    my_keys <- join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb"),
      join_key("c", "c", "cc"),
      join_key("b", "a", c(bb = "aa")),
      join_key("c", "a", c(cc = "aa"))
    )
    parents(my_keys) <- list("b" = "a", "c" = "a")
    testthat::expect_identical(my_keys["b", "c"], c(bb = "cc"))
  }
)

# [.join_keys -----------------------------------------------------------------
testthat::test_that("[.join_keys returns join_keys object when i is missing", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_identical(my_keys[], my_keys)
})

testthat::test_that("[.join_keys returns join_keys object with keys for given datasets", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_identical(
    my_keys[c("d1", "d2")],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

testthat::test_that("[.join_keys returns join_keys object with keys for given index", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_identical(
    my_keys[c(1, 2)],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

testthat::test_that("[.join_keys returns join_keys object for given dataset including its parent", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ab"),
    join_key("d3", "d1", "ac")
  )
  parents(my_keys) <- list("d2" = "d1", "d3" = "d1")

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d2", "d1", "ab")
  )
  parents(expected) <- list("d2" = "d1")

  testthat::expect_equal(my_keys["d2"], expected)
})

testthat::test_that("[.join_keys returns join_keys object for given dataset and doesn't include its children", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ab"),
    join_key("d3", "d1", "ac")
  )
  parents(my_keys) <- list("d2" = "d1", "d3" = "d1")

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d2", "d1", "ab")
  )
  parents(expected) <- list("d2" = "d1")

  testthat::expect_equal(my_keys["d2"], expected)
})

testthat::test_that("[.join_keys returns empty join_keys for inexisting dataset", {
  my_keys <- join_keys(join_key("d1", "d1", "a"))
  testthat::expect_length(my_keys["d2"], 0)
})

testthat::test_that("[.join_keys ignores duplicate indexes - return only first occurrence", {
  jk <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d2", "b")
  )
  testthat::expect_identical(
    jk[c("d1", "d2", "d1")],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

# [<-.join_keys and [[<-.join_keys ------------------------------------------------
testthat::test_that("[[<-.join_keys accepts named list where each containing character", {
  jk <- join_keys()
  testthat::expect_no_error(
    jk[["d1"]] <- list(d1 = c("a", "b", "c"), d2 = c(b = "c", "d" = "d"))
  )
})

testthat::test_that("[[<-.join_keys accepts integerish as index", {
  jk <- join_keys(join_key("a", "a", "aa"))
  testthat::expect_no_error(
    jk[[1]][[1]] <- "bb"
  )
})

testthat::test_that("[[<-.join_keys accepts unnamed vector", {
  jk <- join_keys()
  testthat::expect_no_error(
    jk[["d1"]] <- list(d1 = c("a", "b", "c"))
  )
})

testthat::test_that("[[<-.join_keys doesn't accepts other list than named containing character", {
  jk <- join_keys()
  testthat::expect_error(jk[["d1"]] <- list(d1 = 1:5, d2 = c(b = "c", "d" = "d")))
  testthat::expect_error(jk[["d1"]] <- list(d1 = list(a = "a")))
  testthat::expect_error(jk[["d1"]] <- list(d1 = NULL))
})

testthat::test_that("[[<-.join_keys doesn't accepts other list than named containing character", {
  jk <- join_keys()
  testthat::expect_error(jk[["d1"]] <- list(d1 = 1:5, d2 = c(b = "c", "d" = "d")))
  testthat::expect_error(jk[["d1"]] <- list(d1 = list(a = "a")))
  testthat::expect_error(jk[["d1"]] <- list(d1 = NULL))
  testthat::expect_error(jk[["d1"]] <- "test")
})

testthat::test_that("[[<-.join_keys adds join_keys specified as named list to the list of keys", {
  jk <- join_keys()
  jk[["d1"]] <- list(d1 = "a")
  testthat::expect_identical(jk, join_keys(join_key("d1", "d1", "a")))
})

testthat::test_that("[[<-.join_keys assigning NULL drops a key", {
  jk <- join_keys(join_key("d1", "d1", "a"))
  jk[["d1"]] <- NULL
  testthat::expect_null(jk[["d1"]])
})

testthat::test_that("[[<-.join_keys adds symmetrical change to the foreign dataset", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")

  testthat::expect_identical(
    jk,
    join_keys(
      join_key("d1", "d2", c("A" = "B", "C" = "C")),
      join_key("d2", "d1", c("B" = "A", "C" = "C"))
    )
  )
})

testthat::test_that("[<-.join_keys throws when assigning anything", {
  jk_expected <- join_keys()
  testthat::expect_error(jk_expected["a"] <- join_key("a", "b", "test"), "Can't use `\\[<-`")
})

testthat::test_that("[[<- can mutate existing keys", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys[["d1"]][["d2"]] <- "B"
  testthat::expect_identical(my_keys, join_keys(join_key("d1", "d2", "B")))
})

testthat::test_that("[[<- mutating non-existing keys adds them", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys[["d2"]][["d3"]] <- "B"
  testthat::expect_identical(
    my_keys,
    join_keys(
      join_key("d1", "d2", "A"),
      join_key("d2", "d3", "B")
    )
  )
})

testthat::test_that("[[<- setting a key to character(0) drops the key", {
  my_keys <- join_keys(
    join_key("d1", "d2", "A"),
    join_key("d2", "d3", "B")
  )

  my_keys[["d1"]][["d2"]] <- character(0)

  testthat::expect_identical(
    my_keys,
    join_keys(join_key("d2", "d3", "B"))
  )
})

testthat::test_that("[[<-.join_keys removes keys with NULL", {
  my_keys <- join_keys(
    join_key("d1", "d1", "A"),
    join_key("d2", "d2", "B")
  )
  my_keys[["d2"]][["d2"]] <- NULL

  testthat::expect_identical(
    my_keys,
    join_keys(
      join_key("d1", "d1", "A")
    )
  )
})

testthat::test_that("[[<-.join_keys removes keys with NULL and applies summetrical changes", {
  my_keys <- join_keys(
    join_key("d1", "d2", "A"),
    join_key("d2", "d1", "A"),
    join_key("d2", "d3", "B"),
    join_key("d3", "d2", "B")
  )
  my_keys[["d1"]][["d2"]] <- NULL

  testthat::expect_identical(
    my_keys,
    join_keys(
      join_key("d2", "d3", "B"),
      join_key("d3", "d2", "B")
    )
  )
})

testthat::test_that("[[<-.join_keys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))
})

testthat::test_that("[[<-.join_keys with empty value is set to its name", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))
})

testthat::test_that("[[<-.join_keys passing key unnamed 'empty' value is ignored", {
  # set key on empty variable name equal to ""
  jk <- join_keys()
  testthat::expect_message(jk[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  testthat::expect_equal(jk[["d1"]][["d2"]], c(A = "B"))
})

testthat::test_that("[[<-.join_keys fails when provided foreign key pairs for same datasets, but different keys", {
  jk <- join_keys()
  testthat::expect_error(
    jk[["ds1"]] <- list(ds2 = "new", ds2 = "new_but_different"),
    "cannot specify multiple different join keys between datasets"
  )
})

testthat::test_that("[[<-.join_keys allows when provided foreign key pairs for same datasets and same keys", {
  jk <- join_keys()
  testthat::expect_silent(jk[["ds1"]] <- list(ds2 = "new", ds2 = c("new" = "new")))
  testthat::expect_identical(jk, join_keys(join_key("ds1", "ds2", "new")))
})
