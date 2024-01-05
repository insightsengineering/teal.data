testthat::test_that("col_labels accepts an empty data.frame", {
  testthat::expect_no_error(col_labels(data.frame()))
})

testthat::test_that("col_labels returns empty character vector for a data.frame with no columns", {
  testthat::expect_equal(col_labels(data.frame()[1:5, ], fill = TRUE), character(0))
})

testthat::test_that("col_labels returns a named vector of NA when fill = FALSE and there are no labels", {
  testthat::expect_equal(
    col_labels(iris, fill = FALSE),
    stats::setNames(rep(NA_character_, times = ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("col_labels returns a vector of column names when fill = TRUE and there are no labels", {
  testthat::expect_equal(
    col_labels(iris, fill = TRUE),
    stats::setNames(colnames(iris), nm = colnames(iris))
  )
})

test_that("col_relabel correctly changes column labels in a data frame", {
  iris_df <- iris
  iris_df <- col_relabel(iris_df, Sepal.Length = "Sepal Length of iris flower")
  testthat::expect_identical(
    col_labels(iris_df),
    stats::setNames(c("Sepal Length of iris flower", rep(NA, 4)), nm = names(iris_df))
  )
})

test_that("col_relabel throws an error for non-existent columns", {
  testthat::expect_error(
    col_relabel(iris, NonExistentColumn = "Label"),
    "variables: NonExistentColumn not found"
  )
})

test_that("col_relabel returns the original data.frame when no new labels are specified", {
  iris_df <- col_relabel(iris)
  testthat::expect_equal(iris_df, iris)
})
