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
  iris_df <- utils::head(iris, 2)
  attr(iris_df$Species, "label") <- structure("Label for Species", names = "blah", foo = "bar")
  testthat::expect_identical(col_labels(iris_df)[["Species"]], "Label for Species")
})

testthat::test_that("col_labels returns only 'names' attribute and ignores all the rest", {
  iris_df <- utils::head(iris, 2)
  attr(iris_df$Species, "label") <- structure("Label for Species", names = "blah", foo = "bar")
  testthat::expect_identical(attributes(col_labels(iris_df)["Species"]), list(names = "Species"))
})

testthat::test_that("col_labels throws if label is not a character", {
  iris_df <- utils::head(iris, 2)
  attr(iris_df$Species, "label") <- structure(1, names = "blah", foo = "bar")
  testthat::expect_error(
    col_labels(iris_df),
    "Assertion on '\"label\" attribute of column \"Species\"' failed",
    fixed = TRUE
  )
})

testthat::test_that("col_labels throws if label is not a character of length 1", {
  iris_df <- utils::head(iris, 2)
  attr(iris_df$Species, "label") <- structure(c("a", "b"), names = "blah", foo = "bar")
  testthat::expect_error(
    col_labels(iris_df),
    "Assertion on '\"label\" attribute of column \"Species\"' failed",
    fixed = TRUE
  )
})

# col_labels<- ----
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
    "Assertion on 'names of value' failed: Must be a permutation of set"
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

testthat::test_that("col_labels<- sets variable labels when passed partially named character vector", {
  x <- data.frame(a = 1, b = 2, c = 3)
  col_labels(x) <- c(a = "A", "B", "C")
  testthat::expect_identical(
    col_labels(x),
    c(a = "A", b = "B", c = "C")
  )
})

testthat::test_that("col_labels<- sets variable labels when passed partially named, unordered character vector", {
  x <- data.frame(a = 1, b = 2, c = 3)
  col_labels(x) <- c(b = "B", "A", "C")
  testthat::expect_identical(
    col_labels(x),
    c(a = "A", b = "B", c = "C")
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

testthat::test_that("col_labels<- removes labels on NA_character_", {
  x <- data.frame(a = 1, b = 2, c = 3)
  col_labels(x) <- c("A", "B", "C")
  col_labels(x) <- c(b = NA, "AA", NA)
  testthat::expect_identical(
    col_labels(x),
    c(a = "AA", b = NA, c = NA)
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

testthat::test_that("col_relabel removes labels on NA_character_", {
  x <- data.frame(a = 1, b = 2, c = 3)
  col_labels(x) <- c("A", "B", "C")
  x <- col_relabel(x, b = NA_character_)
  testthat::expect_identical(
    col_labels(x),
    c(a = "A", b = NA, c = "C")
  )
})
