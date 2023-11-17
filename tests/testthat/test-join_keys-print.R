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

testthat::test_that("format.join_keys print inferred keys for children sharing parent", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d2", "d1", "child-a"),
    join_key("d3", "d1", "child-a")
  )
  parents(my_keys) <- list("d2" = "d1", "d3" = "d1")
  testthat::expect_identical(
    format(my_keys),
    paste(
      "A join_keys object containing foreign keys between 3 datasets:",
      "d1: [a]", "  <-- d2: [child-a]", "  <-- d3: [child-a]",
      "d2: [b]", "  --> d1: [child-a]", "  --* (implicit via parent with): d3",
      "d3: [c]", "  --> d1: [child-a]", "  --* (implicit via parent with): d2",
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
