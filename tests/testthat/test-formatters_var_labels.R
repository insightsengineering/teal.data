# col_labels ----
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

testthat::test_that("col_labels works with labels having additional attributes (including names)", {
  x <- iris
  attr(x$Species, "label") <- structure("Label for Species", names = "blah", foo = "bar")
  testthat::expect_identical(col_labels(x)[["Species"]], "Label for Species")
)}

testthat::test_that("col_labels returns only 'names' attribute and ignores all the rest", {
  x <- iris
  attr(x$Species, "label") <- structure("Label for Species", names = "blah", foo = "bar")
  testthat::expect_identical(attributes(col_labels(x)["Species"]), list(names = "Species"))
})

# col_labels ----
testthat::test_that("col_labels<- value accepts character vector", {
  iris_df <- utils::head(iris, 2)
  testthat::expect_error(
    col_labels(iris_df) <- 1:5,
    "Assertion on 'value' failed: Must be of type 'character'"
  )
  testthat::expect_no_error(col_labels(iris_df) <- as.character(1:5))
})

testthat::test_that("col_labels<- value must be same length as x", {
  iris_df <- utils::head(iris, 2)
  testthat::expect_error(
    col_labels(iris_df) <- as.character(1:4),
    "Assertion on 'Length of value is equal to the number of columns' failed"
  )
})

testthat::test_that("col_labels<- value accepts named vector", {
  iris_df <- utils::head(iris, 2)
  testthat::expect_no_error(col_labels(iris_df) <- stats::setNames(as.character(1:5), names(iris)))
})

testthat::test_that("col_labels<- value names must be same as variable names", {
  iris_df <- utils::head(iris, 2)
  testthat::expect_error(
    col_labels(iris_df) <- stats::setNames(as.character(1:5), toupper(names(iris_df))),
    "Assertion on 'column names' failed: Must be a permutation of set"
  )
})

testthat::test_that("col_labels<- sets variable labels when passed unnamed character vector", {
  iris_df <- utils::head(iris, 2)
  labels <- paste("label for", names(iris_df))
  col_labels(iris_df) <- labels
  testthat::expect_identical(
    col_labels(iris_df),
    stats::setNames(labels, names(iris_df))
  )
})

testthat::test_that("col_labels<- sets variable labels when passed named character vector", {
  iris_df <- utils::head(iris, 2)
  labels <- stats::setNames(paste("label for", names(iris_df)), names(iris_df))
  col_labels(iris_df) <- labels
  testthat::expect_identical(
    col_labels(iris_df),
    labels
  )
})

testthat::test_that("col_labels<- matches labels to variables by names of values argument", {
  iris_df <- utils::head(iris, 2)
  labels <- stats::setNames(paste("label for", names(iris_df)), names(iris_df))
  col_labels(iris_df) <- rev(labels)
  testthat::expect_identical(
    col_labels(iris_df),
    labels
  )
})


# col_relabel ----
testthat::test_that("col_relabel correctly changes column labels in a data frame", {
  iris_df <- utils::head(iris, 2)
  iris_df <- col_relabel(iris_df, Sepal.Length = "Sepal Length of iris flower")
  testthat::expect_identical(
    col_labels(iris_df),
    stats::setNames(c("Sepal Length of iris flower", rep(NA, 4)), nm = names(iris_df))
  )
})

testthat::test_that("col_relabel throws an error for non-existent columns", {
  testthat::expect_error(
    col_relabel(iris, NonExistentColumn = "Label"),
    "Must be a subset of \\{.*\\}, but has additional elements \\{'NonExistentColumn'\\}"
  )
})

testthat::test_that("col_relabel returns the original data.frame when no new labels are specified", {
  iris_df <- col_relabel(iris)
  testthat::expect_equal(iris_df, iris)
})
