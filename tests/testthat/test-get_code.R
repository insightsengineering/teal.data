warning_message <- "warning('Code was not verified for reproducibility.')"

testthat::test_that("get_code with datanames does not brake for empty code", {
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = character(0)), datanames = "a"),
    warning_message
  )
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = ""), datanames = "a"),
    c(warning_message, "")
  )
})

testthat::test_that("get_code with datanames extracts code of a binding from a simple code put in a character", {
  code <- c(
    "a <- 1",
    "b <- 2"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code with datanames warns if binding doesn't exist in a code", {
  code <- c(
    "a <- 1",
    "b <- 2"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_error(
    get_code(tdata, datanames = "c"),
    "Assertion on 'datanames' failed"
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
    datanames(tdata) <- c("a", "b")
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      c("a <- 1", "b <- a")
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
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("a <- 1", "b <- identity(x = a)")
  )
})

testthat::test_that("get_code with datanames is possible to output the code for multiple objects", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "c <- 3"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b", "c")
  testthat::expect_identical(
    get_code(tdata, datanames = c("a", "b")),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that("get_code with datanames can't extract the code when no assign operator", {
  code <- c(
    "a <- 1",
    "assign('b', 5)",
    "b <- b + 2"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- b + 2"
  )
})

testthat::test_that("@linksto tag indicate affected object if object is assigned anywhere in a code", {
  code <- c(
    "a <- 1",
    "assign('b', 5) # @linksto b",
    "b <- b + 2"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("assign(\"b\", 5)", "b <- b + 2")
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
    datanames(tdata) <- c("iris2")
    testthat::expect_identical(
      get_code(tdata, datanames = "iris2"),
      c("data(iris)", "iris2 <- head(iris)")
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
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("a <- 1", "b <- a", "b <<- b + 2")
  )
})

testthat::test_that("get_code with datanames detects every assign calls even if not evaluated", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "eval(expression({b <- b + 2}))"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("b <- 2", "eval(expression({\n    b <- b + 2\n}))")
  )
})


# @linksto ---------------------------------------------------------------------------------------------------------

testthat::test_that("@linksto cause to return this line for affected binding", {
  code <- "
  a <- 1 # @linksto b
  b <- 2
  "
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("a <- 1", "b <- 2")
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
    datanames(tdata) <- c("a", "b")
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      c("a <- 1", "b <- 2")
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
    datanames(tdata) <- c("a", "b")
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      c("a <- 1", "b <- 2")
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
    datanames(tdata) <- c("a", "b")
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      c("a <- 1", "b <- a")
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
    datanames(tdata) <- c("a", "b")
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      c("a <- 1", "b <- 2", "a <- a + 1")
    )
    testthat::expect_identical(
      get_code(tdata, datanames = "b"),
      c("b <- 2", "b <- b + 1")
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
    datanames(tdata) <- c("iris2", "iris_head", "classes")
    testthat::expect_identical(
      get_code(tdata, datanames = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "classes <- lapply(iris2, class)")
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
    datanames(tdata) <- c("iris2", "iris_head", "iris3", "classes")
    testthat::expect_identical(
      get_code(tdata, datanames = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "iris3 <- iris_head[1, ]", "classes <- lapply(iris2, class)")
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
  datanames(tdata) <- c("b", "foo")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) {\n    b <- b + 2\n}"
  )
})

testthat::test_that("get_code with datanames ignores occurrence in function definition without { curly brackets", {
  code <- c(
    "b <- 2",
    "foo <- function(b) b <- b + 2 "
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("b", "foo")
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) b <- b + 2"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code with datanames ignores effect of the object which occurs in a function definition", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("b", "foo")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("b <- 2")
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
  datanames(tdata) <- c("a", "b", "foo")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("a <- 1", "b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "b <- foo(a)")
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
    datanames(tdata) <- c("x", "foo", "a")
    testthat::expect_identical(
      get_code(tdata, datanames = "a"),
      c("x <- 1", "foo <- function(foo = 1) \"text\"", "a <- foo(x)")
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
  datanames(tdata) <- c("x", "a")
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    c("x <- data.frame(a = 1:3)")
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    c("x <- data.frame(a = 1:3)", "a <- data.frame(y = 1:3)", "a$x <- a$y", "a$x <- a$x + 2", "a$x <- x$a")
  )
})

testthat::test_that("get_code with datanames detects cooccurrence properly even if all objects are on rhs", {
  code <- c(
    "a <- 1",
    "b <- list(c = 2)",
    "b[[a]] <- 3"
  )
  tdata <- eval_code(teal_data(), code)
  datanames(tdata) <- c("a", "b")
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    c("a <- 1", "b <- list(c = 2)", "b[[a]] <- 3")
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
  datanames(tdata) <- c("x", "a")
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    c(
      warning_message,
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)'
    )
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    c(
      warning_message,
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      'a <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      "a@x <- a@y",
      "a@x <- a@x + 2",
      "a@x <- x@a"
    )
  )
})
