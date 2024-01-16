warning_message <- "warning('Code was not verified for reproducibility.')"

testthat::test_that("get_code with datanames handles empty @code slot", {
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = character(0)), datanames = "a"),
    warning_message
  )
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = ""), datanames = "a"),
    paste0(warning_message, "\n")
  )
})

testthat::test_that("get_code with datanames handles code without symbols in RHS", {
  code <- c(
    "1 + 1",
    "a <- 5",
    "501"
  )

  testthat::expect_identical(
    get_code(teal_data(a = 5, code = code), datanames = "a"),
    paste(warning_message, "a <- 5", sep = "\n")
  )
})

testthat::test_that("get_code with datanames extracts code of a binding from character vector containing simple code", {
  code <- c(
    "a <- 1",
    "b <- 2"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code works for datanames of length > 1", {
  code <- c(
    "a <- 1",
    "b <- 2"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = c("a", "b")),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("get_code with datanames warns if binding doesn't exist in code", {
  code <- c("a <- 1")
  tdata <- eval_code(teal_data(), code)
  testthat::expect_warning(
    get_code(tdata, datanames = "c"),
    "Object\\(s\\) not found in code: c"
  )
})

testthat::test_that("get_code with datanames does not fall into a loop", {
  code <- c(
    "a <- 1",
    "b <- a",
    "c <- b",
    "a <- c"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    paste(code, collapse = "\n")
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste(code[1:2], collapse = "\n")
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "c"),
    paste(code[1:3], collapse = "\n")
  )
})


testthat::test_that(
  "get_code with datanames extracts code of a parent binding but only those evaluated before coocurence",
  {
    code <- c(
      "a <- 1",
      "b <- a",
      "a <- 2"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      paste("a <- 1", "b <- a", sep = "\n")
    )
  }
)

testthat::test_that("get_code with datanames extracts code of a parent binding if used as an arg in fun call", {
  code <- c(
    "a <- 1",
    "b <- identity(x = a)",
    "a <- 2"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("a <- 1", "b <- identity(x = a)", sep = "\n")
  )
})

testthat::test_that("get_code with datanames is possible to output the code for multiple objects", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "c <- 3"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = c("a", "b")),
    paste(code[1:2], collapse = "\n")
  )
})

testthat::test_that("get_code with datanames can extract the code for assign function", {
  code <- c(
    "a <- 1",
    "assign('b', 5)",
    "assign(value = 7, x = 'c')",
    "b <- b + 2",
    "c <- b"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("assign(\"b\", 5)", "b <- b + 2", sep = "\n")
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "c"),
    paste(
      "assign(\"b\", 5)",
      "assign(value = 7, x = \"c\")",
      "b <- b + 2",
      "c <- b",
      sep = "\n"
    )
  )
})

testthat::test_that(
  "get_code with datanames can extract the code for assign function where \"x\" is variable",
  {
    testthat::skip("We will tackle this some day!")
    code <- c(
      "x <- \"a\"",
      "assign(x, 5)",
      "b <- a"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      paste(code, sep = "\n")
    )
  }
)


testthat::test_that("@linksto tag indicate affected object if object is assigned anywhere in a code", {
  code <- c(
    "a <- 1",
    "assign('b', 5) # @linksto b",
    "b <- b + 2"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("assign(\"b\", 5)", "b <- b + 2", sep = "\n")
  )
})

testthat::test_that(
  "get_code with datanames can extract the code when function creates an object which is used only on rhs",
  {
    code <- c(
      "data(iris)",
      "iris2 <- head(iris)"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "iris2"),
      paste("data(iris)", "iris2 <- head(iris)", sep = "\n")
    )
  }
)

testthat::test_that("get_code with datanames can extract the code when using <<-", {
  code <- c(
    "a <- 1",
    "b <- a",
    "b <<- b + 2"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("a <- 1", "b <- a", "b <<- b + 2", sep = "\n")
  )
})

testthat::test_that("get_code with datanames detects every assign calls even if not evaluated", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "eval(expression({b <- b + 2}))"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("b <- 2", "eval(expression({\n    b <- b + 2\n}))", sep = "\n")
  )
})

testthat::test_that("get_code returns result of length for non-empty input", {
  tdata1 <- teal_data()
  tdata1 <- within(tdata1, {
    a <- 1
    b <- a^5
    c <- list(x = 2)
  })

  testthat::expect_length(get_code(tdata1, deparse = FALSE), 1)
  testthat::expect_length(get_code(tdata1, deparse = TRUE), 1)
})


# @linksto ---------------------------------------------------------------------------------------------------------

testthat::test_that("@linksto cause to return this line for affected binding", {
  code <- "
  a <- 1 # @linksto b
  b <- 2
  "
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("a <- 1", "b <- 2", sep = "\n")
  )
})

testthat::test_that(
  "@linksto returns this line for affected binding
  even if object is not specificed/created in the same eval_code",
  {
    code <- c(
      "a <- 1 # @linksto b",
      "b <- 2"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      paste("a <- 1", "b <- 2", sep = "\n")
    )
  }
)

testthat::test_that(
  "@linksto returns this line for affected binding
  if object is not specificed in the same element of code",
  {
    code <- c(
      "a <- 1 ",
      "b <- 2 # @linksto a"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      paste("a <- 1", "b <- 2", sep = "\n")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code with datanamesoutput",
  {
    code <- c(
      "a <- 1",
      "b <- a",
      "a <- 3"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      paste("a <- 1", "b <- a", sep = "\n")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code with datanamesoutput
  when using @linksto",
  {
    code <- c(
      "a <- 1 ",
      "b <- 2 # @linksto a",
      "a <- a + 1",
      "b <- b + 1"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      paste("a <- 1", "b <- 2", "a <- a + 1", sep = "\n")
    )
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      paste("b <- 2", "b <- b + 1", sep = "\n")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object",
  {
    code <- "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris2
      classes <- lapply(iris2, class)
    "
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "classes"),
      paste("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "classes <- lapply(iris2, class)", sep = "\n")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object of a dependent object",
  {
    code <- "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris3
      iris3 <- iris_head[1, ] # @linksto iris2
      classes <- lapply(iris2, class)
    "
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "classes"),
      paste("iris2 <- iris[1:5, ]",
        "iris_head <- head(iris)",
        "iris3 <- iris_head[1, ]",
        "classes <- lapply(iris2, class)",
        sep = "\n"
      )
    )
  }
)

# functions -------------------------------------------------------------------------------------------------------

testthat::test_that("get_code with datanames ignores occurrence in function definition", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) {\n    b <- b + 2\n}"
  )
})

testthat::test_that("get_code with datanames ignores occurrence in function definition in lapply", {
  code <- c(
    "a <- list(a = 1, b = 2, c = 3)",
    "b <- lapply(a, FUN = function(x) { x <- x + 1 })",
    "b <- Filter(function(x) x > 2, b)",
    "x <- 1",
    "print(x)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste("x <- 1", "print(x)", sep = "\n")
  )
})

testthat::test_that("get_code with datanames does not ignore occurrence in function body if object exsits in env", {
  skip("This is not urgent and can be ommitted with @linksto tag.")
  code <- c(
    "a <- list(a = 1, b = 2, c = 3)",
    "p <- 5", # This is not extracted, even though is used in the next line.
    "b <- lapply(a, FUN = function(x) { x <- x + p })",
    "b <- Filter(function(x) x > 2, b)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste(code, sep = "\n")
  )
})

testthat::test_that("get_code with datanames ignores occurrence in function definition without { curly brackets", {
  code <- c(
    "b <- 2",
    "foo <- function(b) b <- b + 2 "
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) b <- b + 2"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code with datanames returns custom function calls on object", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }",
    "foo(b)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "foo(b)", sep = "\n")
  )
})

testthat::test_that("get_code with datanames detects occurrence of the function object", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }",
    "b <- foo(a)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("a <- 1", "b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "b <- foo(a)", sep = "\n")
  )
})

testthat::test_that(
  "Can't detect occurrence of function definition when a formal is named the same as a function",
  {
    code <- c(
      "x <- 1",
      "foo <- function(foo = 1) 'text'",
      "a <- foo(x)"
    )
    tdata <- eval_code(teal_data(), code)
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      paste("x <- 1", "foo <- function(foo = 1) \"text\"", "a <- foo(x)", sep = "\n")
    )
  }
)

# $ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("get_code with datanames understands $ usage and do not treat rhs of $ as objects (only lhs)", {
  code <- c(
    "x <- data.frame(a = 1:3)",
    "a <- data.frame(y = 1:3)",
    "a$x <- a$y",
    "a$x <- a$x + 2",
    "a$x <- x$a"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    "x <- data.frame(a = 1:3)"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    paste("x <- data.frame(a = 1:3)",
      "a <- data.frame(y = 1:3)",
      "a$x <- a$y",
      "a$x <- a$x + 2",
      "a$x <- x$a",
      sep = "\n"
    )
  )
})

testthat::test_that("get_code with datanames detects cooccurrence properly even if all objects are on rhs", {
  code <- c(
    "a <- 1",
    "b <- list(c = 2)",
    "b[[a]] <- 3"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste("a <- 1", "b <- list(c = 2)", "b[[a]] <- 3", sep = "\n")
  )
})


# @ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("get_code with datanames understands @ usage and do not treat rhs of @ as objects (only lhs)", {
  code <- c(
    "setClass('aclass', slots = c(a = 'numeric', x = 'numeric', y = 'numeric')) # @linksto a x",
    "x <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a@x <- a@y",
    "a@x <- a@x + 2",
    "a@x <- x@a"
  )
  tdata <- teal_data(x = 1, a = 1, code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste(
      warning_message,
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    paste(
      warning_message,
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      'a <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      "a@x <- a@y",
      "a@x <- a@x + 2",
      "a@x <- x@a",
      sep = "\n"
    )
  )
})



# libraries -------------------------------------------------------------------------------------------------------

testthat::test_that("library() and require() are always returned", {
  code <- c(
    "set.seed(1)",
    "library(scda)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "x <- 5",
    "y <- 6"
  )
  tdata <- teal_data(x = 5, y = 6, code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste(
      warning_message,
      "library(scda)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "x <- 5",
      sep = "\n"
    )
  )
})


# data() ----------------------------------------------------------------------------------------------------------

testthat::test_that("get_call returns data call for a datanames specified asis", {
  code <- c(
    "set.seed(1)",
    "library(scda)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "data(miniACC, envir = environment())",
    "x <- miniACC"
  )
  tdata <- teal_data(x = 1, code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste(
      warning_message,
      "library(scda)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "data(miniACC, envir = environment())",
      "x <- miniACC",
      sep = "\n"
    )
  )
})

testthat::test_that("get_call data call is returned when data name is provided as character", {
  code <- c(
    "set.seed(1)",
    "library(scda)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "data('mtcars')",
    "z <- mtcars"
  )
  tdata <- teal_data(z = 1, code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "z"),
    paste(
      warning_message,
      "library(scda)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "data(\"mtcars\")",
      "z <- mtcars",
      sep = "\n"
    )
  )
})
