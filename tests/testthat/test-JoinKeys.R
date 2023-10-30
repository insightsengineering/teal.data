test_that("join_key throws error with invalid keys arguments", {
  # invalid types
  expect_error(join_key("d1", "d2", keys = NULL))
  expect_error(join_key("d1", "d2", keys = 1:10))

  # not fully named
  expect_error(join_key("d1", "d2", keys = c("X" = "A", "B")), NA)
  keys <- c("A", "C" = "B")
  names(keys)[1] <- ""
  expect_error(join_key("d1", "d2", keys), NA)

  # duplicates in names or values
  expect_error(join_key("d1", "d2", keys = c("A" = "A", "A" = "B")))
  expect_error(join_key("d1", "d2", keys = c("C" = "A", "D" = "A")))

  # names(keys)!= keys if datasets are the same
  expect_error(join_key("d1", "d1", keys = c("B" = "A", "A" = "B")))
  expect_error(join_key("d1", keys = c("B" = "A", "A" = "B")))
})

test_that("key empty name is changed to the key value", {
  keys <- JoinKeys$new()

  # set empty key name
  keys$mutate("d1", "d2", c("A" = "B", "C"))
  expect_equal(keys$get()$d1$d2, setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  keys$mutate("d1", "d2", c("A" = "B", "C" = ""))
  expect_equal(keys$get()$d1$d2, setNames(c("B", ""), c("A", "C")))

  # set key on empty variable name equal to ""
  keys$mutate("d1", "d2", c("A" = "B", ""))
  expect_equal(keys$get()$d1$d2, setNames(c("B", ""), c("A", "")))
})

test_that("join_key throws error with invalid dataset arguments", {
  # missing
  expect_error(join_key("d1", as.character(NA), keys = c("A" = "B", "C" = "D")))
  # invalid type
  expect_error(join_key("d1", 5, keys = c("A" = "B", "C" = "D")))
  # invalid length
  expect_error(join_key("d1", c("d1", "d2"), keys = c("A" = "B", "C" = "D")))
})

test_that("join_key does not throw error with valid arguments", {
  # keys of length 0
  expect_silent(join_key("d1", "d2", keys = character(0)))
  # keys of length 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B")))
  # keys of length > 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B", "C" = "D")))
  # dataset_1 and dataset_2 can be the same if keys match
  expect_silent(join_key("d1", "d1", keys = c("A" = "A", "B" = "B")))

  expect_silent(join_key("d1", keys = c("A" = "A", "B" = "B")))
})

test_that("cannot set join_keys with incompatible keys", {
  # different keys
  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D", "E" = "F"))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = character(0))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d2", "d1", keys = character(0))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = character(0)),
      join_key("d2", "d1", keys = c("A" = "B", "C" = "D"))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("a" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D"))
    )
  )
})

test_that("can create join_keys with compatible information", {
  # different datasets
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d3", keys = c("A" = "B", "C" = "D"))
    )
  )

  # same keys
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D"))
    )
  )

  # reordering keys still matches
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("C" = "D", "A" = "B"))
    )
  )

  # can match with empty
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = character(0)),
      join_key("d1", "d2", keys = character(0))
    )
  )

  expect_silent(
    join_keys(
      join_key("d2", "d1", keys = character(0)),
      join_key("d2", "d1", keys = character(0))
    )
  )

  # swapping dataset order still matches
  expect_silent(
    join_keys(
      join_key("d2", "d1", keys = c("B" = "A", "D" = "C")),
      join_key("d1", "d2", keys = c("C" = "D", "A" = "B"))
    )
  )
})


test_that("cannot create JoinKeys with invalid arguments", {
  # not using join_key
  expect_error(join_keys("d1", "d2", "A"))
  # key sets with the same pair of datasets but different values
  expect_error(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d1", "B")))
  expect_error(join_keys(join_key("d1", "d2", c("A" = "X")), join_key("d2", "d1", c("A" = "X"))))
})

test_that("can create JoinKeys with valid arguments", {
  # no keys
  expect_silent(join_keys())
  # list of keys
  expect_silent(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d3", "B")))
  # single key out of list
  expect_silent(join_keys(join_key("d1", "d2", "A")))
  # key sets with the same pair of datasets and the same values
  expect_silent(join_keys(join_key("d1", "d2", c("A" = "C")), join_key("d2", "d1", c("C" = "A"))))
  expect_silent(join_keys(join_key("d1", "d2", "X"), join_key("d2", "d1", "X")))
})


test_that("cannot set keys in JoinKeys if they have already been set", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_error(my_keys$set(join_key("d1", "d3", "A")))
})


test_that("creating join keys with d1 -> d2 also creates the key d2 - > d1", {
  my_keys <- join_keys(join_key("d1", "d2", c("A" = "C")))
  expect_equal(my_keys$get("d2", "d1"), c("C" = "A"))
})


test_that("can get all keys for a given dataset", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )
  expect_equal(my_keys$get(dataset_1 = "d1"), list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys$get(dataset_2 = "d1"), list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys$get(dataset_1 = "d3"), list(d1 = c("B" = "A", "T" = "S"), d2 = c("U" = "C", "M" = "L")))
})


test_that("can get all keys from JoinKeys", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )

  all_keys <- my_keys$get()
  expect_equal(names(all_keys), c("d1", "d2", "d3"))
  expect_equal(my_keys$get(dataset_1 = "d1"), all_keys[["d1"]])
})

test_that("join_key with unamed keys vector creates a JoinKeys with the same column names for both datasets ", {
  test_keys <- join_keys(join_key("d1", "d2", keys = c("A", "B")))
  expect_equal(unname(test_keys$get("d1", "d2")), names(test_keys$get("d1", "d2")))
})


test_that("if no keys between pair of datasets then getting them returns character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_equal(my_keys$get("d1", "d3"), character(0))
  expect_equal(my_keys$get("d1", "d4"), character(0))
})

test_that("can mutate existing keys", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys$mutate("d1", "d2", c("X" = "Y"))
  expect_equal(my_keys$get("d1", "d2"), c("X" = "Y"))
})

test_that("mutating non-existing keys adds them", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys$mutate("d2", "d3", c("X" = "Y"))
  expect_equal(my_keys$get("d3", "d2"), c("Y" = "X"))
})

test_that("can remove keys by setting them to character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"), join_key("d3", "d4", c("A" = "B", "C" = "D")))
  my_keys$mutate("d1", "d2", character(0))
  expect_equal(my_keys$get("d1", "d2"), character(0))
})

testthat::test_that("JoinKeys$split method returns empty list when object itself is empty", {
  x <- JoinKeys$new()
  testthat::expect_identical(x$split(), list())
})

testthat::test_that("JoinKeys$split method returns a named list of JoinKeys objects with an element for each dataset", {
  x <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  res <- x$split()
  testthat::expect_true(inherits(res, "list"))
  testthat::expect_equal(length(res), 5)
  testthat::expect_equal(names(res), c("A", "B", "C", "Z", "Y"))
  checkmate::expect_list(res, types = "JoinKeys")

  testthat::expect_equal(names(res$A$get()), c("A", "B", "C"))
  testthat::expect_equal(names(res$B$get()), c("B", "A"))
  testthat::expect_equal(names(res$C$get()), c("C", "A"))
  testthat::expect_equal(names(res$Z$get()), c("Z", "Y"))
  testthat::expect_equal(names(res$Y$get()), c("Y", "Z"))
})

testthat::test_that(
  "JoinKeys$split method returns an updated list after the state of the object is modified by JoinKeys$mutate()",
  {
    x <- JoinKeys$new()
    x$set(
      list(
        join_key("A", "B", c("a" = "b")),
        join_key("A", "C", c("a" = "c", "aa" = "cc")),
        join_key("Z", "Y", c("z" = "y"))
      )
    )
    res <- x$split()

    x$mutate("A", "B", c("a" = "b", "aa" = "bb"))
    res2 <- x$split()

    testthat::expect_false(identical(res, res2))
    testthat::expect_identical(res2$A$get()$A$B, c("a" = "b", "aa" = "bb"))

    # adding new datasets
    x$mutate("D", "G", c("d" = "g"))
    res3 <- x$split()
    testthat::expect_false(identical(res, res3))
    testthat::expect_false(identical(res2, res3))
    testthat::expect_identical(res3$D$get()$D$G, c("d" = "g"))
    testthat::expect_identical(res3$D$get()$G$D, c("g" = "d"))
    testthat::expect_identical(names(res3$D$get()), c("D", "G"))
  }
)

testthat::test_that("JoinKeys$split method does not modify self", {
  x <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_self <- x$clone()
  no_use_output <- x$split()
  testthat::expect_equal(previous_self, x)
})


testthat::test_that("JoinKeys$merge can handle edge case: calling object is empty", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  testthat::expect_silent(x$merge(y))
  testthat::expect_identical(x$get(), y$get())
})

testthat::test_that("JoinKeys$merge can handle edge case: argument is an empty object", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(x))
  testthat::expect_identical(previous_output, y$get())
})

testthat::test_that("JoinKeys$merge can handle edge case: argument is a list of empty objects", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(list(x, x$clone())))
  testthat::expect_identical(previous_output, y$get())

  testthat::expect_silent(y$merge(list(x, x$clone(), x$clone())))
  testthat::expect_identical(previous_output, y$get())
})

testthat::test_that(
  "JoinKeys$merge throws error when improper argument is passed in without modifying the caller",
  {
    y <- JoinKeys$new()
    y$set(
      list(
        join_key("A", "B", c("a" = "b")),
        join_key("A", "C", c("a" = "c", "aa" = "cc")),
        join_key("Z", "Y", c("z" = "y"))
      )
    )
    previous_output <- y$get()
    testthat::expect_error(y$merge())
    testthat::expect_identical(previous_output, y$get())

    testthat::expect_error(y$merge(1))
    testthat::expect_identical(previous_output, y$get())

    testthat::expect_error(y$merge("A"))
    testthat::expect_identical(previous_output, y$get())

    testthat::expect_error(y$merge(list()))
    testthat::expect_identical(previous_output, y$get())

    testthat::expect_error(y$merge(list(1)))
    testthat::expect_identical(previous_output, y$get())

    testthat::expect_error(y$merge(list("A")))
    testthat::expect_identical(previous_output, y$get())
  }
)

testthat::test_that("JoinKeys$merge does nothing when argument is a JoinKeys object with identical data", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(x))
  testthat::expect_identical(previous_output, y$get())
})

testthat::test_that("JoinKeys$merge does nothing when argument is a list of one JoinKeys object with identical data", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(list(x)))
  testthat::expect_identical(previous_output, y$get())

  testthat::expect_silent(y$merge(list(x, x$clone())))
  testthat::expect_identical(previous_output, y$get())
})

testthat::test_that("JoinKeys$merge does nothing when argument is a list of many JoinKeys object with identical data", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(list(x, x, x, x, x, x, x, x)))
  testthat::expect_identical(previous_output, y$get())
})

testthat::test_that("JoinKeys$merge clones data when argument is a list of one JoinKeys object that is a superset", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y")),
      join_key("ZZ", "YY", c("zz" = "yy"))
    )
  )
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- y$get()
  testthat::expect_silent(y$merge(list(x)))
  testthat::expect_false(identical(previous_output, y$get()))
  testthat::expect_identical(x$get(), y$get())
})

testthat::test_that("JoinKeys$merge does nothing when argument is a list of one JoinKeys object that is a subset", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y")),
      join_key("ZZ", "YY", c("zz" = "yy"))
    )
  )
  y$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  previous_output <- x$get()
  testthat::expect_silent(x$merge(list(y)))
  testthat::expect_identical(previous_output, x$get())
})

testthat::test_that("JoinKeys$merge merges mutually exclusive data", {
  x <- JoinKeys$new()
  y <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b"))
    )
  )
  y$set(
    list(
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  z <- JoinKeys$new()
  z$merge(list(x, y))
  testthat::expect_identical(c(x$get(), y$get()), z$get())

  x$merge(y)
  y$merge(x)

  testthat::expect_identical(x$get(), z$get())
  testthat::expect_true(all(y$get() %in% z$get()) && all(z$get() %in% y$get()))
  testthat::expect_true(all(y$get() %in% x$get()) && all(x$get() %in% y$get()))

  testthat::expect_identical(names(z$get()), c("A", "B", "Z", "Y"))
  testthat::expect_equal(length(z$get()), 4)
  testthat::expect_identical(z$get()$A$B, c("a" = "b"))
  testthat::expect_identical(z$get()$B$A, c("b" = "a"))
  testthat::expect_identical(z$get()$Z$Y, c("z" = "y"))
  testthat::expect_identical(z$get()$Y$Z, c("y" = "z"))
})

testthat::test_that("JoinKeys$print for empty set", {
  jk <- JoinKeys$new()
  testthat::expect_output(
    print(jk),
    "An empty JoinKeys object."
  )
})

testthat::test_that("JoinKeys$print for a non-empty set", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("DF1", "DF2", c("id" = "fk"))))
  testthat::expect_output(
    print(jk),
    "A JoinKeys object containing foreign keys between 2 datasets:"
  )
})

testthat::test_that("JoinKeys$set_parents sets the parents of datasets when they are empty", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "fk"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_identical(
    ss <- jk$get_parents(),
    list(df1 = character(0), df2 = "df1")
  )
})

testthat::test_that("JoinKeys$set_parents throws error when overwriting the parent value with a different value", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_error(jk$set_parents(list(df1 = character(0), df2 = "df5")))
})

testthat::test_that("JoinKeys$set_parents works when overwriting the parent value with the same value", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
})

testthat::test_that("JoinKeys$get_parent returns the parent name of the dataset", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_identical(jk$get_parent("df1"), character(0))
  testthat::expect_identical(jk$get_parent("df2"), "df1")
})

testthat::test_that("JoinKeys$get_parent returns NULL when dataset is not found or not passed", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_null(jk$get_parent())
  testthat::expect_null(jk$get_parent("df3"))
})

testthat::test_that("JoinKeys$get_parents returns a list of all parents", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_silent(jk$set_parents(list(df1 = character(0), df2 = "df1")))
  testthat::expect_identical(jk$get_parents(), list(df1 = character(0), df2 = "df1"))
})

testthat::test_that("JoinKeys$get_parents returns an empty list when no parents are present", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_identical(jk$get_parents(), list())
})

testthat::test_that("JoinKeys$get_parents throws error when dataname input is provided", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  testthat::expect_error(jk$get_parents("df1"), "unused argument \\(\"df1\"\\)")
})

testthat::test_that("JoinKeys$update_keys_given_parents does not update the join_keys when no presents are present", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df2", c("id" = "id"))))
  jk$update_keys_given_parents()
  testthat::expect_equal(jk, join_keys(join_key("df1", "df2", c("id" = "id"))))
})

testthat::test_that("JoinKeys$update_keys_given_parents updates the join_keys when presents are present", {
  jk <- JoinKeys$new()
  jk$set(list(
    join_key("df1", "df1", c("id", "id2")),
    join_key("df1", "df2", c("id" = "id")),
    join_key("df1", "df3", c("id" = "id"))
  ))
  jk$set_parents(list(df1 = character(0), df2 = "df1", df3 = "df1"))
  jk$update_keys_given_parents()
  expected_jk <- join_keys(
    join_key("df1", "df1", c("id", "id2")),
    join_key("df1", "df2", c("id" = "id")),
    join_key("df1", "df3", c("id" = "id")),
    join_key("df2", "df2", c("id", "id2")),
    join_key("df2", "df3", c("id", "id2")),
    join_key("df3", "df3", c("id", "id2"))
  )
  expected_jk$set_parents(list(df1 = character(0), df2 = "df1", df3 = "df1"))
  testthat::expect_equal(jk, expected_jk)
})

testthat::test_that("JoinKeys$check_parent_child does nothing if no parents are present", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df1", c("id" = "id"))))
  testthat::expect_identical(jk$get_parents(), list())
  testthat::expect_silent(jk$.__enclos_env__$private$check_parent_child())
})

testthat::test_that("JoinKeys$check_parent_child throws error if no join_keys exist for chuld-parent", {
  jk <- JoinKeys$new()
  jk$set(list(join_key("df1", "df1", c("id" = "id"))))
  jk$set_parents(list(df1 = character(0), df2 = "df1", df3 = "df1"))
  testthat::expect_error(
    jk$.__enclos_env__$private$check_parent_child(),
    "No join keys from df2 to its parent \\(df1\\) and vice versa"
  )
})

test_that("cdisc_join_keys will generate JoinKeys for named list with non-named elements", {
  new_dataset <- cdisc_join_keys("ADSL", ADTTE = rADTTE)
  jk <- get_join_keys(new_dataset)

  expect_identical(unname(jk$get("ADSL", "ADSL")), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk$get("ADTTE", "ADTTE")), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk$get("ADSL", "ADTTE")), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk$get("ADTTE", "ADSL")), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate JoinKeys for character list", {
  new_dataset <- cdisc_join_keys("ADSL", "ADTTE")
  jk <- get_join_keys(new_dataset)

  expect_identical(unname(jk$get("ADSL", "ADSL")), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk$get("ADTTE", "ADTTE")), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk$get("ADSL", "ADTTE")), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk$get("ADTTE", "ADSL")), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate JoinKeys for named list", {
  new_dataset <- cdisc_join_keys(ADSL = rADSL, ADTTE = rADTTE)
  jk <- get_join_keys(new_dataset)

  expect_identical(unname(jk$get("ADSL", "ADSL")), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk$get("ADTTE", "ADTTE")), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk$get("ADSL", "ADTTE")), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk$get("ADTTE", "ADSL")), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will retrieve ADTTE primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  internal_keys <- default_cdisc_keys[["ADTTE"]]
  jk <- cdisc_join_keys("ADTTE")
  primary_keys <- unname(jk$get("ADTTE", "ADTTE"))

  expect_equal(primary_keys, internal_keys$primary)

  foreign_keys <- unname(jk$get("ADTTE", internal_keys$parent))
  expect_equal(foreign_keys, internal_keys$foreign)
})

test_that("cdisc_join_keys will retrieve known primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  vapply(
    datasets,
    function(.x) {
      internal_keys <- default_cdisc_keys[[.x]]
      jk <- cdisc_join_keys(.x)
      primary_keys <- unname(jk$get(.x, .x))
      expect_equal(primary_keys, internal_keys$primary)
      if (!is.null(internal_keys$foreign)) {
        foreign_keys <- unname(jk$get(.x, internal_keys$parent))
        expect_equal(foreign_keys, internal_keys$foreign)
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
      expect_equal(unname(jk[.x]), get_cdisc_keys(.x))
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
  expect_length(get_join_keys(cdisc_join_keys(adae_cdc))$get(), 0)
})

test_that("[.JoinKeys returns the primary key if arguments only have 1 dataset", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  expect_failure(expect_identical(jk$get("ds1"), jk["ds1"]))
  checkmate::expect_character(jk["ds1"])
})

test_that("[.JoinKeys subsets relationship pair successfully", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  expect_identical(jk$get("ds1", "ds1"), jk["ds1"])
})

test_that("[<-.JoinKeys assigns new relationship pair", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  expect_length(jk$get("ds1", "ds2"), 0)

  jk["ds1", "ds2"] <- c("id")
  expect_identical(jk$get("ds1", "ds2"), c(id = "id"))
  expect_identical(jk$get("ds1", "ds2"), jk["ds1", "ds2"])
})

test_that("[<-.JoinKeys modifies existing relationship pair", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  jk["ds1", "ds1"] <- c("Species")
  expect_failure(expect_identical(jk$get("ds1", "ds1"), c(id = "id")))
  expect_identical(jk$get("ds1", "ds1"), c(Species = "Species"))
})
