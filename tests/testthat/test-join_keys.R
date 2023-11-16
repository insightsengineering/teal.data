# join_keys --------------------------------------------------------------------
testthat::test_that("join_keys creates empty join_keys object by default", {
  testthat::expect_s3_class(join_keys(), "join_keys")
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
      class = c("join_keys", "list")
    )
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

testthat::test_that("join_keys accepts duplicated join_key", {
  testthat::expect_no_error(
    join_keys(join_key("d1", "d2", "a"), join_key("d2", "d1", "a"))
  )
})

testthat::test_that("join_keys doesn't accept other objects than teal_data, TealData and join_key", {
  testthat::expect_error(join_keys("a")) # todo: add expected error message
})

testthat::test_that("join_keys doesn't accept a list which is identical to output of join_key function", {
  key <- join_key("a", "b", "test")
  testthat::expect_error(join_keys(unclass(key)))
})

testthat::test_that("join_keys fails when provided foreign key pairs have incompatible values", {
  testthat::expect_error(
    join_keys(join_key("d1", "d2", "a"), join_key("d2", "d1", "b")),
    "cannot specify multiple different join keys between datasets"
  )
  testthat::expect_error(
    join_keys(join_key("d1", "d2", c(a = "b")), join_key("d2", "d1", c(a = "b"))),
    "cannot specify multiple different join keys between datasets"
  )

  testthat::expect_error(
    join_keys(
      join_keys(
        join_key("q", "b", "d"),
        join_key("a", "b", "c")
      ),
      join_key("a", "q", "e"),
      join_key("a", "b", "f")
    ),
    "cannot specify multiple different join keys between datasets"
  )
})

testthat::test_that("join_keys constructor adds symmetric keys on given (unnamed) foreign key", {
  my_keys <- join_keys(join_key("d1", "d2", "a"))
  testthat::expect_identical(
    my_keys,
    join_keys(join_key("d1", "d2", "a"), join_key("d2", "d1", "a"))
  )
})

testthat::test_that("join_keys constructor adds symmetric keys on given (named) foreign key", {
  testthat::expect_identical(
    join_keys(
      join_key("d1", "d2", c(a = "b"))
    ),
    join_keys(
      join_key("d1", "d2", c(a = "b")),
      join_key("d2", "d1", c(b = "a"))
    )
  )
})
# [.join_keys -----------------------------------------------------------------
testthat::test_that("[[.join_keys returns keys for given pair", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  testthat::expect_identical(my_keys[["b"]][["a"]], c(`child-parent` = "child-parent"))
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

testthat::test_that("[[.join_keys infer keys between child by shared foreign keys to parent ", {
  my_keys <- join_keys(
    join_key("a", "a", "aa"),
    join_key("b", "b", "bb"),
    join_key("c", "c", "cc"),
    join_key("b", "a", "child-parent"),
    join_key("c", "a", "child-parent")
  )
  parents(my_keys) <- list("b" = "a", "c" = "a")
  testthat::expect_identical(my_keys[["b"]][["c"]], c(`child-parent` = "child-parent"))
})

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

testthat::test_that("[.join_keys returns join_keys for given dataset including those connected with foreign keys", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ab"),
    join_key("d3", "d1", "ac")
  )
  testthat::expect_identical(
    my_keys["d2", keep_all_foreign_keys = TRUE],
    join_keys(
      join_key("d2", "d2", "b"),
      join_key("d2", "d1", "ab")
    )
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

testthat::test_that("[.join_keys returns join_keys object for given dataset and doesn't include its childs", {
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
  expect_equal(jk[["d1"]][["d2"]], c(A = "B", C = "C"))
})

testthat::test_that("join_keys()[]<-.join_keys with named empty valued is changed to its name", {
  jk <- join_keys()
  join_keys(jk)[["d1"]][["d2"]] <- c(A = "B", C = "")
  expect_equal(jk[["d1"]][["d2"]], c(A = "B", C = "C"))
})

testthat::test_that("join_keys()[]<-.join_keys with empty value in a named vector are ignored ", {
  jk <- join_keys()
  testthat::expect_message(join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  testthat::expect_equal(jk[["d1"]][["d2"]], c(A = "B"))
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

# -----------------------------------------------------------------------------
#
# Setting names (names<-join_keys)
#
testthat::test_that("names<-.join_keys will replace names at all levels of the join_keys list", {
  jk <- join_keys(
    join_key("a", "a", "a"),
    join_key("a", "b", "ab"),
    join_key("a", "c", "ac"),
    join_key("d", "b", "db")
  )

  names(jk)[1:2] <- c("x", "y")

  testthat::expect_identical(
    jk,
    join_keys(
      join_key("x", "x", "a"),
      join_key("x", "y", "ab"),
      join_key("x", "c", "ac"),
      join_key("d", "y", "db")
    )
  )
})

testthat::test_that("names<-.join_keys will replace names at all levels of the join_keys list when parents set", {
  jk <- join_keys(
    join_key("a", "a", "a"),
    join_key("b", "a", "ba"),
    join_key("c", "a", "ca"),
    join_key("d", "b", "db")
  )
  parents(jk) <- list(b = "a", c = "a", d = "b")

  expected <- join_keys(
    join_key("a", "a", "a"),
    join_key("B", "a", "ba"),
    join_key("c", "a", "ca"),
    join_key("d", "B", "db")
  )
  parents(expected) <- list(B = "a", c = "a", d = "B")

  names(jk)[2] <- "B"
  testthat::expect_identical(jk, expected)
})

# -----------------------------------------------------------------------------
#
# Merging join_keys (c.join_keys)

testthat::test_that("c.join_keys joins join_keys object with join_key objects", {
  obj <- join_keys()
  obj <- c(obj, join_key("a", "a", "aa"), join_key("b", "b", "bb"))
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys duplicated keys are ignored", {
  obj <- join_keys()
  obj <- c(obj, join_key("a", "a", "aa"), join_key("a", "a", "aa"))
  testthat::expect_identical(
    obj,
    join_keys(join_key("a", "a", "aa"))
  )
})

testthat::test_that("c.join_keys joins join_keys object with join_keys objects", {
  obj <- join_keys()
  obj <- c(
    obj,
    join_keys(join_key("a", "a", "aa")),
    join_keys(join_key("b", "b", "bb"))
  )
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys joins join_keys object with join_keys and join_key objects", {
  obj <- join_keys()
  obj <- c(
    obj,
    join_keys(join_key("a", "a", "aa")),
    join_key("b", "b", "bb")
  )
  testthat::expect_identical(
    obj,
    join_keys(
      join_key("a", "a", "aa"),
      join_key("b", "b", "bb")
    )
  )
})

testthat::test_that("c.join_keys throws when joining with a list", {
  obj <- join_keys()
  testthat::expect_error(c(
    obj,
    list(c = list(c = "cc"))
  ))
})

testthat::test_that("c.join_keys doesn't throw when second object is empty join_keys", {
  x <- join_keys(join_key("a", "a", "aa"))
  y <- join_keys()
  testthat::expect_no_error(c(x, y))
})

testthat::test_that("c.join_keys throws on conflicting join_keys_set objects", {
  obj <- join_keys()
  testthat::expect_error(
    c(
      obj,
      join_keys(join_key("a", "b", "aa")),
      join_keys(join_key("b", "a", "bb"))
    ),
    "cannot specify multiple different join keys between datasets"
  )

  testthat::expect_error(
    c(
      obj,
      join_key("a", "b", "aa"),
      join_key("b", "a", "bb")
    ),
    "cannot specify multiple different join keys between datasets"
  )
})

testthat::test_that("c.join_key_set throws on conflicting join_keys_set objects", {
  testthat::expect_error(
    c(
      join_key("a", "b", "aa"),
      join_key("a", "b", "ca"),
      join_key("a", "b", "cc")
    ),
    "cannot specify multiple different join keys between datasets"
  )
})

# -----------------------------------------------------------------------------
#
# print.join_keys

testthat::test_that("format.join_keys for empty set", {
  jk <- join_keys()
  testthat::expect_identical(format(jk), "An empty join_keys object.")
})

testthat::test_that("format.join_keys with empty parents", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ba"),
    join_key("d3", "d2", "ca")
  )
  testthat::expect_identical(
    format(my_keys),
    paste(
      "A join_keys object containing foreign keys between 3 datasets:",
      "d1: [a]", "  <-> d2: [ba]", "d2: [b]", "  <-> d1: [ba]", "  <-> d3: [ca]",
      "d3: [c]", "  <-> d2: [ca]",
      sep = "\n"
    )
  )
})

testthat::test_that("format.join_keys for parents", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ba"),
    join_key("d3", "d2", "ca")
  )
  parents(my_keys) <- list("d2" = "d1", "d3" = "d2")
  testthat::expect_identical(
    format(my_keys),
    paste(
      "A join_keys object containing foreign keys between 3 datasets:",
      "d1: [a]", "  <-- d2: [ba]", "d2: [b]", "  --> d1: [ba]", "  <-- d3: [ca]",
      "d3: [c]", "  --> d2: [ca]",
      sep = "\n"
    )
  )
})

testthat::test_that("print.join_keys produces output same as format", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "ba"),
    join_key("d3", "d2", "ca")
  )
  testthat::expect_output(print(my_keys), format(my_keys), fixed = TRUE)
})
