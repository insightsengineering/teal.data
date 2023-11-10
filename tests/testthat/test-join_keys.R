test_that("join_keys.teal_data will successfully obtain object from teal_data", {
  obj <- helper_generator_teal_data()

  expect_identical(obj@join_keys, join_keys(obj))
  helper_test_getter_join_keys(obj, "ds1")
})

test_that("join_keys.join_keys will return itself", {
  obj <- helper_generator_join_keys()

  expect_identical(obj, join_keys(obj))
  helper_test_getter_join_keys(obj, "ds1")
})

test_that("join_keys<-.teal_data shared test to setter (in mass)", {
  obj <- helper_generator_teal_data()
  helper_test_setter_mass_join_keys_add(obj)
})

test_that("join_keys<-.join_keys shared test to setter  (in mass)", {
  obj <- helper_generator_join_keys()
  helper_test_setter_mass_join_keys_add(obj)
})

test_that("join_keys<-.teal_data shared test to getter and setter", {
  obj <- helper_generator_teal_data()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

test_that("join_keys<-.join_keys shared test to getter and setter", {
  obj <- helper_generator_join_keys()
  helper_test_getter_join_keys_add(obj, "ds1", "ds2")
})

test_that("join_keys<-.join_keys to set via a join_key_set object", {
  obj <- join_keys()
  join_keys(obj) <- c(obj, join_key("ds1", "ds2", "id"))
  expect_equal(obj$ds1, list("ds2" = c("id" = "id")))
  expect_equal(obj$ds2, list("ds1" = c("id" = "id")))
})

test_that("c.join_keys to set via multiple lists that progressively merge object", {
  obj <- join_keys()
  obj <- c(obj, join_key("ds1", "ds2", "id"))
  obj <- c(obj, join_key("ds3", "ds4", "id_id"), join_key("ds5", "ds6", "id_id"))
  obj <- c(obj, join_key("ds7", "ds8", "id_id_id"))

  expect_length(obj, 8)
})

# -----------------------------------------------------------------------------
#
# [[ and [[<-
#
test_that("[[<-.join_keys creates symmetric relationship", {
  jk <- join_keys()

  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")

  expect_identical(
    jk,
    structure(
      list(
        d1 = list(d2 = c("A" = "B", "C" = "C")),
        d2 = list(d1 = c("B" = "A", "C" = "C"))
      ),
      class = c("join_keys", "list")
    )
  )
})

test_that("[[<-.join_keys is equivalent to using the constructor (single subscript)", {
  jk <- join_keys(
    join_key("d1", "d2", c("A" = "B", "C" = "C")),
    join_key("d3", "d4", c("D", "E")),
    join_key("d5", "d6", c("F", "K" = "k"))
  )

  jk2 <- join_keys()

  jk2[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")
  jk2[["d3"]][["d4"]] <- c("D" = "D", "E" = "E")
  jk2[["d5"]][["d6"]] <- c("F" = "F", "K" = "k")

  expect_identical(jk, jk2)
})

test_that("[<-.join_keys is equivalent to using the constructor", {
  jk <- join_keys(
    join_key("d1", "d2", c("A", "B")),
    join_key("d3", "d4", c("C", "D")),
    join_key("d5", "d6", c("E", "F"))
  )

  jk2 <- join_keys()

  jk2["d1"] <- list(d2 = c("A", "B"))
  jk2["d3"] <- list(d4 = c("C", "D"))
  jk2["d5"] <- list(d6 = c("E", "F"))

  expect_identical(jk, jk2)
})

test_that("[.join_keys can subscript multiple values by index or name", {
  jk <- join_keys(
    join_key("d1", "d2", c("A" = "B", "C")),
    join_key("d3", "d4", c("D", "E")),
    join_key("d5", "d6", c("F", "K" = "k"))
  )

  expect_length(jk[1:2], 2)
  expect_length(jk[c("d1", "d5")], 2)

  expect_identical(
    jk[c("d1", "d5")],
    structure(
      list(d1 = jk[["d1"]], d5 = jk[["d5"]]),
      class = c("join_keys", "list")
    )
  )

  expect_identical(jk[2], structure(list(d2 = jk[["d2"]]), class = c("join_keys", "list")))
  expect_identical(jk[c(1, 3)], structure(list(d1 = jk[["d1"]], d3 = jk[["d3"]]), class = c("join_keys", "list")))
})

test_that("[<-.join_keys cannot subscript multiple values", {
  jk <- join_keys(
    join_key("d1", "d2", c("A" = "B", "C")),
    join_key("d2", "d3", c("D", "E")),
    join_key("d4", "d3", c("F", "K" = "k")),
    join_key("d4", "d1", c("F", "K" = "k"))
  )

  jk[1:2] <- NULL

  expect_length(jk, 2)
  expect_identical(jk[["d4"]][["d3"]], c("F" = "F", "K" = "k"))
  expect_identical(jk[["d3"]][["d4"]], c("F" = "F", "k" = "K"))
})

test_that("[[<- can mutate existing keys", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys[["d1"]][["d2"]] <- c("X" = "Y")
  expect_equal(my_keys[["d1"]][["d2"]], c("X" = "Y"))
  expect_equal(my_keys[["d2"]][["d1"]], c("Y" = "X"))
})

test_that("[[<- mutating non-existing keys adds them", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys[["d2"]][["d3"]] <- c("X" = "Y")
  expect_equal(my_keys[["d2"]][["d3"]], c("X" = "Y"))
  expect_equal(my_keys[["d3"]][["d2"]], c("Y" = "X"))
})

test_that("[[<- can remove keys by setting them to character(0)", {
  my_keys <- join_keys(
    join_key("d1", "d2", "A"),
    join_key("d3", "d4", c("A" = "B", "C" = "D"))
  )
  my_keys[["d1"]][["d2"]] <- character(0)
  expect_equal(my_keys[["d1"]][["d2"]], character(0))
})

test_that("[[<-.join_keys removes keys with NULL", {
  jk <- join_keys()

  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")
  jk[["d1"]][["d2"]] <- NULL

  expect_identical(
    jk,
    structure(
      list(
        d1 = structure(list(), names = character(0)),
        d2 = structure(list(), names = character(0))
      ),
      class = c("join_keys", "list")
    )
  )
})

test_that("[[<-.join_keys removes keys with NULL and keeps existing", {
  jk <- join_keys()

  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "C")
  jk[["d2"]][["d3"]] <- c("A" = "B", "C" = "C")
  jk[["d1"]][["d4"]] <- c("A" = "B", "C" = "C")
  jk[["d1"]][["d2"]] <- NULL

  expect_null(jk[["d1"]][["d2"]])
  expect_null(jk[["d2"]][["d1"]])

  expect_failure(expect_null(jk[["d2"]][["d3"]]))
  expect_failure(expect_null(jk[["d3"]][["d2"]]))
  expect_failure(expect_null(jk[["d1"]][["d4"]]))
  expect_failure(expect_null(jk[["d4"]][["d1"]]))

  expect_length(jk, 4)
})


# -----------------------------------------------------------------------------
#
# mutate_join_keys (empty value name)
#

test_that("[[<-.join_keys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- join_keys()
  jk[["d1"]][["d2"]] <- c("A" = "B", "C" = "")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- join_keys()
  expect_message(jk[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B"), c("A")))
})

test_that("join_keys()[]<-.join_keys with empty name is changed to the key value", {
  # set empty key name
  jk <- join_keys()
  join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", "C")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  jk <- join_keys()
  join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", "C" = "")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on empty variable name equal to ""
  jk <- join_keys()
  expect_message(join_keys(jk)[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  expect_equal(jk[["d1"]][["d2"]], setNames(c("B"), c("A")))
})

test_that("join_keys()[]<-.teal_data with empty name is changed to the key value", {
  # set empty key name
  td <- teal_data()
  join_keys(td)[["d1"]][["d2"]] <- c("A" = "B", "C")
  expect_equal(join_keys(td)[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on non-empty variable name equal to ""
  td <- teal_data()
  join_keys(td)[["d1"]][["d2"]] <- c("A" = "B", "C" = "")
  expect_equal(join_keys(td)[["d1"]][["d2"]], setNames(c("B", "C"), c("A", "C")))

  # set key on empty variable name equal to ""
  td <- teal_data()
  expect_message(join_keys(td)[["d1"]][["d2"]] <- c("A" = "B", ""), "are ignored")
  expect_equal(join_keys(td)[["d1"]][["d2"]], setNames(c("B"), c("A")))
})

# -----------------------------------------------------------------------------

test_that("join_keys constructor creates symmetric relationship", {
  jk <- join_keys(join_key("d1", "d2", c("A" = "B", "C" = "C")))

  expect_identical(
    jk,
    structure(
      list(
        d1 = list(d2 = c("A" = "B", "C" = "C")),
        d2 = list(d1 = c("B" = "A", "C" = "C"))
      ),
      class = c("join_keys", "list")
    )
  )
})


test_that("join_keys cannot set join_keys with incompatible keys", {
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

test_that("join_keys can create join_keys with compatible information", {
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

test_that("join_keys cannot create join_keys with invalid arguments", {
  # not using join_key
  expect_error(join_keys("d1", "d2", "A"))
  # key sets with the same pair of datasets but different values
  expect_error(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d1", "B")))
  expect_error(join_keys(join_key("d1", "d2", c("A" = "X")), join_key("d2", "d1", c("A" = "X"))))
})

test_that("join_keys can create join_keys with valid arguments", {
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

test_that("join_keys creating join keys with d1 -> d2 also creates the key d2 - > d1", {
  my_keys <- join_keys(join_key("d1", "d2", c("A" = "C")))
  expect_equal(my_keys[["d2"]][["d1"]], c("C" = "A"))
})

test_that("join_keys[ can get all keys for a given dataset", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )
  expect_equal(
    my_keys[dataset_1 = "d1"],
    structure(
      list("d1" = list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T"))),
      class = c("join_keys", "list")
    )
  )
  expect_equal(
    my_keys[dataset_1 = "d3"],
    structure(
      list("d3" = list(d1 = c("B" = "A", "T" = "S"), d2 = c("U" = "C", "M" = "L"))),
      class = c("join_keys", "list")
    )
  )
})

test_that("join_keys can get all keys from join_keys", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )

  all_keys <- my_keys
  expect_equal(names(all_keys), c("d1", "d2", "d3"))
  expect_equal(
    my_keys[dataset_1 = "d1"],
    structure(
      list(d1 = all_keys[["d1"]]),
      class = c("join_keys", "list")
    )
  )
})

test_that(
  "join_keys join_key with unamed keys vector creates a join_keys with the same column names for both datasets ",
  {
    test_keys <- join_keys(join_key("d1", "d2", keys = c("A", "B")))
    expect_equal(unname(test_keys[["d1"]][["d2"]]), names(test_keys[["d1"]][["d2"]]))
  }
)

test_that("join_keys if no keys between pair of datasets then getting them returns NULL", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_equal(my_keys[["d1"]][["d3"]], NULL)
  expect_equal(my_keys[["d1"]][["d4"]], NULL)
})

# -----------------------------------------------------------------------------
#
# merge_join_keys

testthat::test_that("merge_join_keys can handle edge case: calling object is empty", {
  x <- join_keys()
  y <- join_keys()

  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  testthat::expect_silent(merge_join_keys(x, y))
  testthat::expect_identical(join_keys(x), join_keys(x))
})

testthat::test_that("c.join_keys can handle edge case: argument is an empty object", {
  x <- join_keys()
  y <- join_keys()
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- join_keys(y)
  testthat::expect_silent(merge_join_keys(y, x))
  testthat::expect_identical(previous_output, join_keys(y))
})

testthat::test_that("c.join_keys can handle edge case: argument is a list of empty objects", {
  x <- join_keys()
  y <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- y
  testthat::expect_silent(c(y, x, x))
  testthat::expect_identical(previous_output, y)

  testthat::expect_silent(c(y, x, x, x))
  testthat::expect_identical(previous_output, y)
})

testthat::test_that(
  "merge_join_keys throws error when improper argument is passed in without modifying the caller",
  {
    y <- join_keys(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )

    previous_output <- join_keys(y)

    testthat::expect_error(y <- merge_join_keys())
    testthat::expect_identical(previous_output, y)

    testthat::expect_error(y <- merge_join_keys(y, 1))
    testthat::expect_identical(previous_output, y)

    testthat::expect_error(y <- merge_join_keys(y, "A"))
    testthat::expect_identical(previous_output, y)

    testthat::expect_error(y <- merge_join_keys(y, list()))
    testthat::expect_identical(previous_output, y)

    testthat::expect_error(y <- merge_join_keys(list(1)))
    testthat::expect_identical(previous_output, y)

    testthat::expect_error(y <- merge_join_keys(y, "A"))
    testthat::expect_identical(previous_output, y)
  }
)

testthat::test_that("merge_join_keys does nothing when argument is a join_keys object with identical data", {
  x <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  y <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- y
  testthat::expect_silent(merge_join_keys(y, x))
  testthat::expect_identical(previous_output, y)
})

testthat::test_that(
  "merge_join_keys does nothing when argument is a list of one join_keys object with identical data",
  {
    x <- join_keys(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
    y <- join_keys(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )

    previous_output <- y
    testthat::expect_silent(c(y, x))
    testthat::expect_identical(previous_output, y)

    testthat::expect_silent(c(y, x, x))
    testthat::expect_identical(previous_output, y)
  }
)

testthat::test_that(
  "merge_join_keys does nothing when argument is a list of many join_keys object with identical data",
  {
    x <- join_keys(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
    y <- join_keys(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )

    previous_output <- y
    testthat::expect_silent(c(y, x, x, x, x, x, x, x, x))
    testthat::expect_identical(previous_output, y)
  }
)

testthat::test_that("merge_join_keys clones data when argument is a list of one join_keys object that is a superset", {
  x <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y")),
    join_key("ZZ", "YY", c("zz" = "yy"))
  )
  y <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  previous_output <- y
  testthat::expect_silent(y <- c(y, x))
  testthat::expect_false(identical(previous_output, y))
  testthat::expect_identical(x, y)
})

testthat::test_that("merge_join_keys does nothing when argument is a list of one join_keys object that is a subset", {
  x <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y")),
    join_key("ZZ", "YY", c("zz" = "yy"))
  )
  y <- join_keys(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- join_keys(x)
  testthat::expect_silent(x <- c(x, y))
  testthat::expect_identical(previous_output, join_keys(x))
})

testthat::test_that("merge_join_keys merges mutually exclusive data", {
  x <- join_keys(
    join_key("A", "B", c("a" = "b"))
  )
  y <- join_keys(
    join_key("Z", "Y", c("z" = "y"))
  )

  z <- join_keys()
  z <- c(z, x, y)
  manual_join <- c(x, y)
  class(manual_join) <- class(new_join_keys())
  testthat::expect_identical(manual_join, z)

  x <- c(x, y)
  y <- c(y, x)

  testthat::expect_identical(x, z)
  testthat::expect_true(all(y %in% z) && all(z %in% y))
  testthat::expect_true(all(y %in% x) && all(x %in% y))

  testthat::expect_identical(sort(names(z)), c("A", "B", "Y", "Z"))
  testthat::expect_equal(length(z), 4)
  testthat::expect_identical(z$A$B, c("a" = "b"))
  testthat::expect_identical(z$B$A, c("b" = "a"))
  testthat::expect_identical(z$Z$Y, c("z" = "y"))
  testthat::expect_identical(z$Y$Z, c("y" = "z"))
})

# -----------------------------------------------------------------------------
#
# print.join_keys

testthat::test_that("print.join_keys for empty set", {
  jk <- join_keys()
  testthat::expect_output(
    print(jk),
    "An empty join_keys object."
  )
})

testthat::test_that("print.join_keys for a non-empty set", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("DF1", "DF2", c("id" = "fk")))
  testthat::expect_output(
    print(jk),
    "A join_keys object containing foreign keys between 2 datasets:"
  )
})

testthat::test_that("parents<- sets the parents of datasets when they are empty", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "fk")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_identical(
    ss <- parents(jk),
    list(df1 = character(0), df2 = "df1")
  )
})
