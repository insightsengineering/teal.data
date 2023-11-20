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
