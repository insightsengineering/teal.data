testthat::test_that("format.join_keys for empty set", {
  jk <- join_keys()
  testthat::expect_identical(format(jk), "An empty join_keys object.")
})

testthat::test_that("format.join_keys with empty parents", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a", directed = FALSE),
    join_key("d2", "d2", "b", directed = FALSE),
    join_key("d3", "d3", "c", directed = FALSE),
    join_key("d1", "d2", "ab", directed = FALSE),
    join_key("d2", "d3", "ac", directed = FALSE)
  )
  testthat::expect_identical(
    format(my_keys),
    paste(
      "A join_keys object containing foreign keys between 3 datasets:",
      "d1: [a]", "  <-> d2: [ab]", "d2: [b]", "  <-> d1: [ab]", "  <-> d3: [ac]",
      "d3: [c]", "  <-> d2: [ac]",
      sep = "\n"
    )
  )
})

testthat::test_that("format.join_keys for parents", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d1", "d2", "ab"),
    join_key("d2", "d3", "ac")
  )

  testthat::expect_identical(
    format(my_keys),
    paste(
      "A join_keys object containing foreign keys between 3 datasets:",
      "d1: [a]", "  <-- d2: [ab]", "d2: [b]", "  --> d1: [ab]", "  <-- d3: [ac]",
      "d3: [c]", "  --> d2: [ac]",
      sep = "\n"
    )
  )
})

testthat::test_that("format.join_keys print inferred keys for children sharing parent", {
  my_keys <- join_keys(
    join_key("d1", "d1", "a"),
    join_key("d2", "d2", "b"),
    join_key("d3", "d3", "c"),
    join_key("d1", "d2", "child-a"),
    join_key("d1", "d3", "child-a")
  )

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
    join_key("d1", "d2", "ab"),
    join_key("d2", "d3", "ac")
  )
  testthat::expect_output(print(my_keys), format(my_keys), fixed = TRUE)
})
