warning_message <- "warning('Code was not verified for reproducibility.')"

testthat::test_that("handles empty @code slot", {
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = character(0)), datanames = "a"),
    warning_message
  )
  testthat::expect_identical(
    get_code(teal_data(a = 1, code = ""), datanames = "a"),
    paste0(warning_message, "\n")
  )
})

testthat::test_that("handles the code without symbols on rhs", {
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

testthat::test_that("handles the code included in curly brackets", {
  code <- "{1 + 1;a <- 5}"

  testthat::expect_identical(
    get_code(teal_data(a = 5, code = code), datanames = "a"),
    paste(warning_message, "a <- 5", sep = "\n")
  )
})

testthat::test_that("handles the code of length > 1 included in curly brackets", {
  tdata <- teal.data::teal_data(a = 5)
  tdata <- eval_code(td, code = "{a<-5}")
  tdata <- eval_code(td, code = "1+1")

  testthat::expect_identical(
    get_code(, datanames = "a"),
    paste(warning_message, "a <- 5", sep = "\n")
  )
})


testthat::test_that("extracts the code of a binding from character vector containing simple code", {
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

testthat::test_that("extracts the code without downstream usage", {
  code <- c(
    "a <- 1",
    "head(a)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    "a <- 1"
  )
})

testthat::test_that("works for datanames of length > 1", {
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

testthat::test_that("warns if binding doesn't exist in code", {
  code <- c("a <- 1")
  tdata <- eval_code(teal_data(), code)
  testthat::expect_warning(
    get_code(tdata, datanames = "c"),
    "Object\\(s\\) not found in code: c"
  )
})

testthat::test_that("does not fall into a loop", {
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


testthat::test_that("extracts code of a parent binding but only those evaluated before coocurence", {
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
})

testthat::test_that("extracts the code of a parent binding if used as an arg in a function call", {
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

testthat::test_that("extracts the code when using <<-", {
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

testthat::test_that("detects every assign calls even if not evaluated, if there is only one assignment in this line", {
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

testthat::test_that("returns result of length 1 for non-empty input", {
  tdata1 <- teal_data()
  tdata1 <- within(tdata1, {
    a <- 1
    b <- a^5
    c <- list(x = 2)
  })

  testthat::expect_length(get_code(tdata1, deparse = FALSE), 1)
  testthat::expect_length(get_code(tdata1, deparse = TRUE), 1)
})

testthat::test_that("does not break if code is separated by ;", {
  code <- c(
    "a <- 1;a <- a + 1"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "a"),
    gsub(";", "\n", code, fixed = TRUE)
  )
})

testthat::test_that("does not break if code uses quote()", {
  code <- c(
    "expr <- quote(x <- x + 1)",
    "x <- 0",
    "eval(expr)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    code[2]
  )
})

testthat::test_that("does not break if object is used in a function on lhs", {
  code <- c(
    "data(iris)",
    "iris2 <- iris",
    "names(iris) <- letters[1:5]"
  )
  tdata <- eval_code(teal_data(), code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "iris"),
    paste(code[c(1, 3)], collapse = "\n")
  )
})

testthat::test_that(
  "does not break if object is used in a function on lhs and influencers are both on lhs and rhs",
  {
    code <- c(
      "x <- 5",
      "y <- length(x)",
      "names(x)[y] <- y"
    )
    tdata <- eval_code(teal_data(), code = code)
    testthat::expect_identical(
      get_code(tdata, datanames = "x"),
      paste(code, collapse = "\n")
    )
  }
)

# assign ----------------------------------------------------------------------------------------------------------

testthat::test_that("extracts the code for assign() where \"x\" is a literal string", {
  code <- c(
    "a <- 1",
    "assign('b', 5)",
    "assign(value = 7, x = 'c')",
    "assign(value = 15, x = \"d\")",
    "b <- b + 2",
    "c <- b",
    "d <- d * 2"
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
  testthat::expect_identical(
    get_code(tdata, datanames = "d"),
    paste("assign(value = 15, x = \"d\")", "d <- d * 2", sep = "\n")
  )
})

testthat::test_that("extracts the code for assign() where \"x\" is variable", {
  testthat::skip("We will not resolve this, as this requires code evaluation.")
  code <- c(
    "x <- \"a\"",
    "assign(x, 5)",
    "b <- a"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("works for assign() detection no matter how many parametrers were provided in assignq()", {
  code <- c(
    "x <- 1",
    "assign(\"x\", 0, envir = environment())",
    "assign(inherits = FALSE, immediate = TRUE, \"z\", 5, envir = environment())",
    "y <- x + z",
    "y <- x"
  )

  tdata <- eval_code(teal_data(), code)

  testthat::expect_identical(
    get_code(tdata, datanames = "y"),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("detects function usage of the assignment operator", {
  code <- c(
    "x <- 1",
    "`<-`(y,x)"
  )
  code2 <- "`<-`(y, `<-`(x, 2))"

  tdata <- eval_code(teal_data(), code)
  tdata2 <- eval_code(teal_data(), code2)

  testthat::expect_identical(
    get_code(tdata, datanames = "y"),
    paste(c(code[1], "y <- x"), collapse = "\n")
  )
  testthat::expect_identical(
    get_code(tdata2, datanames = "y"),
    "y <- x <- 2"
  )
})


# @linksto ---------------------------------------------------------------------------------------------------------

testthat::test_that("get_code does not break if @linksto is put in the last line", {
  # In some cases R parses comment as a separate expression so the comment is not
  # directly associated with this line of code. This situation occurs when `eval` is in the last
  # line of the code. Other cases are not known but are highly probable.
  code <- c(
    "expr <- quote(x <- x + 1)",
    "x <- 0",
    "eval(expr) #@linksto x"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste(gsub(" #@linksto x", "", code, fixed = TRUE), collapse = "\n")
  )
})

testthat::test_that("@linksto makes a line being returned for an affected binding", {
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
  "@linksto returns the line for an affected binding
  even if the object did not exist in the same iteration of eval_code",
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
  "lines affecting parent evaluated after co-occurrence are not included in output when using @linksto",
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
  "@linksto gets extracted if it's a side-effect on a dependent object (even of a dependent object)",
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

testthat::test_that("ignores occurrence in a function definition", {
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

testthat::test_that("ignores occurrence in a function definition that has function in it", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { function(c) {b <- c + 2 }}"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) {\n    function(c) {\n        b <- c + 2\n    }\n}"
  )
})

testthat::test_that("ignores occurrence in a function definition if there is multiple function definitions", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { function(c) {b <- c + 2 }}",
    "b <- b + 1",
    "bar <- function(b) print(b)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    "b <- 2\nb <- b + 1"
  )
  testthat::expect_identical(
    get_code(tdata, datanames = "foo"),
    "foo <- function(b) {\n    function(c) {\n        b <- c + 2\n    }\n}"
  )
})

testthat::test_that("ignores occurrence in a function definition in lapply", {
  code <- c(
    "a <- list(a = 1, b = 2, c = 3)",
    "b <- lapply(a, FUN = function(x) { x <- x + 1 })",
    "b <- Filter(function(x) x > 2, b)",
    "x <- 1",
    "identity(x)"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    "x <- 1"
  )
})

testthat::test_that("does not ignore occurrence in function body if object exsits in env", {
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

testthat::test_that("ignores occurrence in function definition without { curly brackets", {
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

testthat::test_that("detects occurrence of the function object", {
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

testthat::test_that("detects occurrence of a function definition when a formal is named the same as a function", {
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
})

testthat::test_that("detects occurrence of a function definition with a @linksto usage", {
  code <- c(
    "
        foo <- function() {
          env <- parent.frame()
          env$x <- 0
        }",
    "foo() # @linksto x",
    "y <- x"
  )
  tdata <- teal_data(code = code)
  testthat::expect_identical(
    get_code(tdata, datanames = "x"),
    paste(
      warning_message,
      "foo <- function() {\n    env <- parent.frame()\n    env$x <- 0\n}\nfoo()",
      sep = "\n"
    )
  )
})
# $ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("understands $ usage and do not treat rhs of $ as objects (only lhs)", {
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

testthat::test_that("detects cooccurrence properly even if all objects are on lhs", {
  code <- c(
    "a <- 1",
    "b <- list(c = 2)",
    "b[[a]] <- 3"
  )
  tdata <- eval_code(teal_data(), code)
  testthat::expect_identical(
    get_code(tdata, datanames = "b"),
    paste(code, collapse = "\n")
  )
})


# @ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("understands @ usage and do not treat rhs of @ as objects (only lhs)", {
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

testthat::test_that("data() call is returned when data name is provided as is", {
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

testthat::test_that("data() call is returned when data name is provided as a character", {
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
