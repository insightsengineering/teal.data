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

test_that("join_keys cannot create JoinKeys with invalid arguments", {
  # not using join_key
  expect_error(join_keys("d1", "d2", "A"))
  # key sets with the same pair of datasets but different values
  expect_error(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d1", "B")))
  expect_error(join_keys(join_key("d1", "d2", c("A" = "X")), join_key("d2", "d1", c("A" = "X"))))
})

test_that("join_keys can create JoinKeys with valid arguments", {
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
  expect_equal(my_keys["d2", "d1"], c("C" = "A"))
})


test_that("join_keys[ can get all keys for a given dataset", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )
  expect_equal(my_keys[dataset_1 = "d1"], list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys[dataset_2 = "d1"], list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys[dataset_1 = "d3"], list(d1 = c("B" = "A", "T" = "S"), d2 = c("U" = "C", "M" = "L")))
})


test_that("join_keys can get all keys from JoinKeys", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )

  all_keys <- my_keys
  expect_equal(names(all_keys), c("d1", "d2", "d3"))
  expect_equal(my_keys[dataset_1 = "d1"], all_keys[["d1"]])
})

test_that("join_keys join_key with unamed keys vector creates a JoinKeys with the same column names for both datasets ", {
  test_keys <- join_keys(join_key("d1", "d2", keys = c("A", "B")))
  expect_equal(unname(test_keys["d1", "d2"]), names(test_keys["d1", "d2"]))
})

test_that("join_keys if no keys between pair of datasets then getting them returns character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_equal(my_keys["d1", "d3"], character(0))
  expect_equal(my_keys["d1", "d4"], character(0))
})

# -----------------------------------------------------------------------------
#
# mutate_join_keys

test_that("mutate_join_keys.JoinKeys can mutate existing keys", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  new_keys <- mutate_join_keys(my_keys, "d1", "d2", c("X" = "Y"))
  expect_equal(new_keys["d1", "d2"], c("X" = "Y"))
})

test_that("mutate_join_keys.JoinKeys mutating non-existing keys adds them", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  new_keys <- mutate_join_keys(my_keys, "d2", "d3", c("X" = "Y"))
  expect_equal(new_keys["d3", "d2"], c("Y" = "X"))
})

test_that("mutate_join_keys.JoinKeys can remove keys by setting them to character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"), join_key("d3", "d4", c("A" = "B", "C" = "D")))
  new_keys <- mutate_join_keys(my_keys, "d1", "d2", character(0))
  expect_equal(new_keys["d1", "d2"], character(0))
})

# -----------------------------------------------------------------------------
#
# split_join_keys

testthat::test_that("split_join_keys method returns empty list when object itself is empty", {
  x <- join_keys()
  testthat::expect_identical(split_join_keys(x), list())
})

testthat::test_that("split_join_keys method returns a named list of JoinKeys objects with an element for each dataset", {
  x <- join_keys()
  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  res <- split_join_keys(x)
  testthat::expect_true(inherits(res, "list"))
  testthat::expect_equal(length(res), 5)
  testthat::expect_equal(names(res), c("A", "B", "C", "Z", "Y"))
  checkmate::expect_list(res, types = "JoinKeys")

  testthat::expect_equal(names(res$A), c("A", "B", "C"))
  testthat::expect_equal(names(res$B), c("B", "A"))
  testthat::expect_equal(names(res$C), c("C", "A"))
  testthat::expect_equal(names(res$Z), c("Z", "Y"))
  testthat::expect_equal(names(res$Y), c("Y", "Z"))
})

testthat::test_that(
  "split_join_keys method returns an updated list after the state of the object is modified by mutate_join_keys",
  {
    x <- join_keys()
    join_keys(x) <- list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
    res <- split_join_keys(x)

    x2 <- mutate_join_keys(x, "A", "B", c("a" = "b", "aa" = "bb"))
    res2 <- split_join_keys(x2)

    testthat::expect_false(identical(res, res2))
    testthat::expect_identical(res2$A$A$B, c("a" = "b", "aa" = "bb"))

    # adding new datasets
    x3 <- mutate_join_keys(x2, "D", "G", c("d" = "g"))
    res3 <- split_join_keys(x3)
    testthat::expect_false(identical(res, res3))
    testthat::expect_false(identical(res2, res3))
    testthat::expect_identical(res3$D$D$G, c("d" = "g"))
    testthat::expect_identical(res3$D$G$D, c("g" = "d"))
    testthat::expect_identical(names(res3$D), c("D", "G"))
  }
)

testthat::test_that("split_join_keys method does not modify self", {
  x <- join_keys()
  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  previous_self <- x
  no_use_output <- split_join_keys(x)
  testthat::expect_equal(previous_self, x)
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

testthat::test_that("merge_join_keys can handle edge case: argument is an empty object", {
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

testthat::test_that("merge_join_keys can handle edge case: argument is a list of empty objects", {
  x <- join_keys()
  y <- join_keys()

  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- join_keys(y)
  testthat::expect_silent(merge_join_keys(y, list(x, x)))
  testthat::expect_identical(previous_output, join_keys(y))

  testthat::expect_silent(merge_join_keys(y, list(x, x, x)))
  testthat::expect_identical(previous_output, join_keys(y))
})

testthat::test_that(
  "merge_join_keys throws error when improper argument is passed in without modifying the caller",
  {
    y <- join_keys()
    join_keys(y) <- list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )

    previous_output <- join_keys(y)
    testthat::expect_error(y <- merge_join_keys(y))
    testthat::expect_identical(previous_output, join_keys(y))

    testthat::expect_error(y <- merge_join_keys(y, 1))
    testthat::expect_identical(previous_output, join_keys(y))

    testthat::expect_error(y <- merge_join_keys(y, "A"))
    testthat::expect_identical(previous_output, join_keys(y))

    testthat::expect_error(y <- merge_join_keys(y, list()))
    testthat::expect_identical(previous_output, join_keys(y))

    testthat::expect_error(y <- merge_join_keys(list(1)))
    testthat::expect_identical(previous_output, join_keys(y))

    testthat::expect_error(y <- merge_join_keys(y, list("A")))
    testthat::expect_identical(previous_output, join_keys(y))
  }
)

testthat::test_that("merge_join_keys does nothing when argument is a JoinKeys object with identical data", {
  x <- join_keys()
  y <- join_keys()
  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- join_keys(y)
  testthat::expect_silent(merge_join_keys(y, x))
  testthat::expect_identical(previous_output, join_keys(y))
})

testthat::test_that("merge_join_keys does nothing when argument is a list of one JoinKeys object with identical data", {
  x <- join_keys()
  y <- join_keys()

  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  previous_output <- join_keys(y)
  testthat::expect_silent(merge_join_keys(y, list(x)))
  testthat::expect_identical(previous_output, join_keys(y))

  testthat::expect_silent(merge_join_keys(y, list(x, x)))
  testthat::expect_identical(previous_output, join_keys(y))
})

testthat::test_that("merge_join_keys does nothing when argument is a list of many JoinKeys object with identical data", {
  x <- join_keys()
  y <- join_keys()

  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  previous_output <- join_keys(y)
  testthat::expect_silent(merge_join_keys(y, list(x, x, x, x, x, x, x, x)))
  testthat::expect_identical(previous_output, join_keys(y))
})

testthat::test_that("merge_join_keys clones data when argument is a list of one JoinKeys object that is a superset", {
  x <- join_keys()
  y <- join_keys()

  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y")),
    join_key("ZZ", "YY", c("zz" = "yy"))
  )
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )

  previous_output <- join_keys(y)
  testthat::expect_silent(y <- merge_join_keys(y, list(x)))
  testthat::expect_false(identical(previous_output, join_keys(y)))
  testthat::expect_identical(join_keys(x), join_keys(y))
})

testthat::test_that("merge_join_keys does nothing when argument is a list of one JoinKeys object that is a subset", {
  x <- join_keys()
  y <- join_keys()

  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y")),
    join_key("ZZ", "YY", c("zz" = "yy"))
  )
  join_keys(y) <- list(
    join_key("A", "B", c("a" = "b")),
    join_key("A", "C", c("a" = "c", "aa" = "cc")),
    join_key("Z", "Y", c("z" = "y"))
  )
  previous_output <- join_keys(x)
  testthat::expect_silent(x <- merge_join_keys(x, list(y)))
  testthat::expect_identical(previous_output, join_keys(x))
})

testthat::test_that("merge_join_keys merges mutually exclusive data", {
  x <- join_keys()
  y <- join_keys()

  join_keys(x) <- list(
    join_key("A", "B", c("a" = "b"))
  )
  join_keys(y) <- list(
    join_key("Z", "Y", c("z" = "y"))
  )

  z <- join_keys()
  z <- merge_join_keys(z, list(x, y))
  manual_join <- c(join_keys(x), join_keys(y))
  class(manual_join) <- class(new_join_keys())
  testthat::expect_identical(manual_join, join_keys(z))

  x <- merge_join_keys(x, y)
  y <- merge_join_keys(y, x)

  testthat::expect_identical(join_keys(x), join_keys(z))
  testthat::expect_true(all(join_keys(y) %in% join_keys(z)) && all(join_keys(z) %in% join_keys(y)))
  testthat::expect_true(all(join_keys(y) %in% join_keys(x)) && all(join_keys(x) %in% join_keys(y)))

  testthat::expect_identical(names(join_keys(z)), c("A", "B", "Z", "Y"))
  testthat::expect_equal(length(join_keys(z)), 4)
  testthat::expect_identical(join_keys(z)$A$B, c("a" = "b"))
  testthat::expect_identical(join_keys(z)$B$A, c("b" = "a"))
  testthat::expect_identical(join_keys(z)$Z$Y, c("z" = "y"))
  testthat::expect_identical(join_keys(z)$Y$Z, c("y" = "z"))
})

# -----------------------------------------------------------------------------
#
# print.JoinKeys

testthat::test_that("print.JoinKeys for empty set", {
  jk <- join_keys()
  testthat::expect_output(
    print(jk),
    "An empty JoinKeys object."
  )
})

testthat::test_that("print.JoinKeys for a non-empty set", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("DF1", "DF2", c("id" = "fk")))
  testthat::expect_output(
    print(jk),
    "A JoinKeys object containing foreign keys between 2 datasets:"
  )
})

testthat::test_that("JoinKeys$set_parents sets the parents of datasets when they are empty", {
  jk <- join_keys()
  join_keys(jk) <- list(join_key("df1", "df2", c("id" = "fk")))
  testthat::expect_silent(parents(jk) <- list(df1 = character(0), df2 = "df1"))
  testthat::expect_identical(
    ss <- parents(jk),
    list(df1 = character(0), df2 = "df1")
  )
})

# -----------------------------------------------------------------------------
#
# cdisc_join_keys

test_that("cdisc_join_keys will generate JoinKeys for named list with non-named elements", {
  new_dataset <- cdisc_join_keys("ADSL", ADTTE = rADTTE)
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk["ADSL", "ADSL"]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk["ADTTE", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk["ADSL", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk["ADTTE", "ADSL"]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate JoinKeys for character list", {
  new_dataset <- cdisc_join_keys("ADSL", "ADTTE")
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk["ADSL", "ADSL"]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk["ADTTE", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk["ADSL", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk["ADTTE", "ADSL"]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will generate JoinKeys for named list", {
  new_dataset <- cdisc_join_keys(ADSL = rADSL, ADTTE = rADTTE)
  jk <- join_keys(new_dataset)

  expect_identical(unname(jk["ADSL", "ADSL"]), default_cdisc_keys[["ADSL"]]$primary)
  expect_identical(unname(jk["ADTTE", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$primary)

  expect_identical(unname(jk["ADSL", "ADTTE"]), default_cdisc_keys[["ADTTE"]]$foreign)
  expect_identical(unname(jk["ADTTE", "ADSL"]), default_cdisc_keys[["ADTTE"]]$foreign)
})

test_that("cdisc_join_keys will retrieve ADTTE primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  internal_keys <- default_cdisc_keys[["ADTTE"]]
  jk <- cdisc_join_keys("ADTTE")
  primary_keys <- unname(jk["ADTTE", "ADTTE"])

  expect_equal(primary_keys, internal_keys$primary)

  foreign_keys <- unname(jk["ADTTE", internal_keys$parent])
  expect_equal(foreign_keys, internal_keys$foreign)
})

test_that("cdisc_join_keys will retrieve known primary and foreign keys", {
  datasets <- names(default_cdisc_keys)

  vapply(
    datasets,
    function(.x) {
      internal_keys <- default_cdisc_keys[[.x]]
      jk <- cdisc_join_keys(.x)
      primary_keys <- unname(jk[.x, .x])
      expect_equal(primary_keys, internal_keys$primary)
      if (!is.null(internal_keys$foreign)) {
        foreign_keys <- unname(jk[.x, internal_keys$parent])
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
      expect_equal(unname(jk[.x, .x]), get_cdisc_keys(.x))
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
  expect_length(join_keys(cdisc_join_keys(adae_cdc)), 0)
})

# -----------------------------------------------------------------------------
#
# Subset-JoinKeys

test_that("[<-.JoinKeys assigns new relationship pair", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  expect_length(jk["ds1", "ds2"], 0)

  jk["ds1", "ds2"] <- c("id")
  expect_identical(jk["ds1", "ds2"], c(id = "id"))
  expect_identical(get_join_key(jk, "ds1", "ds2"), jk["ds1", "ds2"])
})

test_that("[<-.JoinKeys modifies existing relationship pair", {
  jk <- join_keys(join_key("ds1", keys = c("id")))

  jk["ds1", "ds1"] <- c("Species")
  expect_failure(expect_identical(jk["ds1", "ds1"], c(id = "id")))
  expect_identical(get_join_key(jk, "ds1", "ds1"), c(Species = "Species"))
})
