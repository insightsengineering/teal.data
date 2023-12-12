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
    join_key("a", "b", "ba"),
    join_key("a", "c", "ca"),
    join_key("b", "d", "db")
  )

  expected <- join_keys(
    join_key("a", "a", "a"),
    join_key("B", "a", "ba", parent = "none"),
    join_key("c", "a", "ca", parent = "none"),
    join_key("d", "B", "db", parent = "none")
  )
  parents(expected) <- list(B = "a", c = "a", d = "B")

  names(jk)[2] <- "B"
  testthat::expect_equal(jk, expected)
})
