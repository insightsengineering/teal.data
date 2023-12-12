# join_keys[i] -----------------------------------------------------------------
testthat::test_that("join_keys[i] returns join_keys object when i and j is missing", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_identical(my_keys[], my_keys)
})

testthat::test_that("join_keys[i] returns empty join_keys when i or j are NULL", {
  my_keys <- join_keys(join_key("d1", "d1", "a"))
  testthat::expect_identical(my_keys[NULL], join_keys())
  testthat::expect_identical(my_keys[, NULL], join_keys())
})

testthat::test_that("join_keys[i] subsets join_keys object to specific datasets", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_equal(
    my_keys[c("d1", "d2")],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

testthat::test_that("join_keys[i] returns join_keys object with keys for given index", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_equal(
    my_keys[c(1, 2)],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

testthat::test_that("join_keys[-i] drops keys for given index", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_equal(
    my_keys[-3],
    join_keys(join_key("d1", "d1", "a"), join_key("d2", "d2", "b"))
  )
})

testthat::test_that("join_keys[i] returns join_keys object for given dataset including its parent", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d1", "d2", "ab"),
    join_key("d1", "d3", "ac")
  )

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab")
  )

  testthat::expect_equal(my_keys["d2"], expected)
})

testthat::test_that("join_keys[i] returns join_keys object for given dataset and doesn't include its children", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d1", "d2", "ab"),
    join_key("d1", "d3", "ac")
  )

  expected <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d1", "d2", "ab")
  )

  testthat::expect_equal(my_keys["d2"], expected)
})

testthat::test_that("join_keys[i] returns empty join_keys for inexisting dataset", {
  my_keys <- join_keys(join_key("d1", "d1", "a"))
  testthat::expect_length(my_keys["d2"], 0)
})

testthat::test_that("join_keys[i] ignores duplicate indexes - return only first occurrence", {
  jk <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d2", "d3", "b")
  )
  testthat::expect_equal(
    jk[c("d1", "d2", "d1")],
    join_keys(
      join_key("d1", "d1", "a"),
      join_key("d2", "d2", "b")
    )
  )
})

testthat::test_that("join_keys[,j] returns the same as join_keys[i,]", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c")
  )
  testthat::expect_identical(
    my_keys[, c("d1", "d2")],
    my_keys[c("d1", "d2")]
  )
})

# join_keys[i, j]  -----------------------------------------------------------------
testthat::test_that("join_keys[i,j] returns keys for given pair", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_identical(my_keys["b", "a"], c(`child-parent` = "child-parent"))
})

testthat::test_that("join_keys[i,j] returns keys for pair given by numeric indices", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_identical(my_keys[2, 1], c(`child-parent` = "child-parent"))
})

testthat::test_that("join_keys[i,j] return NULL for given pair when no such key and no common parent", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_null(my_keys["b", "c"])
})

testthat::test_that(
  "join_keys[i,j] doesn't infer keys between children if they don't have common key to parent",
  {
    my_keys <- join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb"),
      join_key("c", "c", "cc"),
      join_key("a", "b", c("a1" = "aa")),
      join_key("a", "c", c("a2" = "aa"))
    )
    testthat::expect_null(my_keys["b", "c"])
  }
)

testthat::test_that(
  "join_keys[i,j] doesn't infer keys between grandchildren",
  {
    my_keys <- join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb"),
      join_key("c", "c", "cc"),
      join_key("a", "b", "child-parent"),
      join_key("a", "c", "child-parent"),
      join_key("b", "d", "grandchild-child"),
      join_key("c", "e", "grandchild-child")
    )
    testthat::expect_null(my_keys["d", "e"])
  }
)

testthat::test_that(
  "join_keys[i,j ] infer keys between children through foreign keys to parent. ",
  {
    my_keys <- join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb"),
      join_key("c", "c", "cc"),
      join_key("a", "b", c("aa" = "bb")),
      join_key("a", "c", c("aa" = "cc"))
    )
    # "bb" and "cc" are the names in child datasets, "aa" is the name in parent dataset
    testthat::expect_identical(my_keys["b", "c"], c(bb = "cc"))
  }
)

testthat::test_that("join_keys[i,j] returns NULL for inexisting key pair (can't even infer)", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc")
  )
  testthat::expect_null(my_keys["inexisting", "inexisting"])
})

testthat::test_that("join_keys[i,j] throws when one of the indices is longer than 1", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc")
  )
  testthat::expect_error(my_keys[c("a", "b"), "c"], "Can't extract keys for multiple pairs.")
})

# [<-.join_keys ------------------------------------------------
testthat::test_that("join_keys[i]<- throws when assigning anything", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys["a"] <- join_key("a", "b", "test"), "specify both indices to set a key pair.")
})

testthat::test_that("join_keys[i]<- throws when no index specified", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys[] <- join_key("a", "b", "test"), "specify both indices to set a key pair.")
})

testthat::test_that("join_keys[i,j]<- can set new value for existing pair", {
  my_keys <- join_keys(join_key("a", "a", "aa"))
  testthat::expect_no_error(my_keys["a", "a"] <- "new key")
  testthat::expect_identical(my_keys, join_keys(join_key("a", "a", "new key")))
})

testthat::test_that("join_keys[i,j]<- sets a new keys for inexisting pair", {
  my_keys <- join_keys(join_key("a", "a", "aa"))
  testthat::expect_no_error(my_keys["b", "c"] <- "new key")
  testthat::expect_identical(my_keys, join_keys(join_key("a", "a", "aa"), join_key("b", "c", "new key")))
})

testthat::test_that("join_keys[i,j]<- throws when assigning to inspecific index", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys[, "b"] <- join_key("a", "b", "test"))
})

testthat::test_that("join_keys[i,j]<- throws when assigning to j only", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys[, "b"] <- join_key("a", "b", "test"))
})

testthat::test_that("join_keys[i,j]<- throws when i or j are NULL", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys[NULL, 1] <- join_key("a", "b", "test"), "NULL")
  testthat::expect_error(my_keys[1, NULL] <- join_key("a", "b", "test"), "NULL")
})

testthat::test_that("join_keys[i,j]<- throws when i or j are longer than 1", {
  my_keys <- join_keys()
  testthat::expect_error(my_keys[c("a", "b"), "a"] <- "new key")
  testthat::expect_error(my_keys["a", c("a", "b")] <- "new key")
})

testthat::test_that("join_keys[i,j]<- removes keys with NULL", {
  my_keys <- join_keys(
    join_key("d1", "d1", "A"),
    join_key("d2", "d2", "B"),
    join_key("d1", "d2", c("A" = "B"))
  )
  my_keys["d2", "d1"] <- NULL

  testthat::expect_equal(
    my_keys,
    join_keys(
      join_key("d1", "d1", "A"),
      join_key("d2", "d2", "B")
    )
  )
})

# [[<-.join_keys ------------------------------------------------
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
  testthat::expect_equal(jk, join_keys(join_key("d1", "d1", "a")))
})

testthat::test_that("[[<-.join_keys assigning NULL drops a key", {
  jk <- join_keys(join_key("d1", "d1", "a"))
  jk[["d1"]] <- NULL
  testthat::expect_null(jk[["d1"]])
})

testthat::test_that("[[<-.join_keys adds symmetrical change without parents to the foreign dataset", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")

  testthat::expect_equal(
    jk,
    structure(
      list(
        d1 = list(
          d2 = c(c("A" = "B", "C" = "C"))
        ),
        d2 = list(
          d1 = c("B" = "A", "C" = "C")
        )
      ),
      class = c("join_keys", "list")
    )
  )
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
      join_key("d2", "d3", "B", parent = "none") # [[<- doesn't set parent
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

testthat::test_that("[[<-.join_keys removes keys with NULL and applies symmetrical changes", {
  my_keys <- join_keys(
    join_key("d1", "d2", "A"),
    join_key("d2", "d3", "B")
  )
  my_keys[["d1"]][["d2"]] <- NULL

  expect_null(my_keys["d1", "d2"])
  expect_null(my_keys["d2", "d1"])

  expect_equal(
    my_keys,
    join_keys(join_key("d2", "d3", "B"))
  )
})

testthat::test_that("[[<-.join_keys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C")
  expect_equal(jk[["d1"]][["d2"]], stats::setNames(c("B", "C"), c("A", "C")))
})

testthat::test_that("[[<-.join_keys with empty value is set to its name", {
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "")
  expect_equal(jk[["d1"]][["d2"]], stats::setNames(c("B", "C"), c("A", "C")))
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
  testthat::expect_equal(jk, join_keys(join_key("ds1", "ds2", "new", parent = "none")))
})
