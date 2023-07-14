cc <- CodeClass$new()

test_that("Basic example CodeClass", {
  expect_true(inherits(cc$set_code(c("foo <- function() {1}", "foo2 <- function() {2}")), "CodeClass"))
  expect_identical(cc$get_code(), "foo <- function() {\n    1\n}\nfoo2 <- function() {\n    2\n}")
  expect_equal(
    cc$get_code(deparse = FALSE),
    list(rlang::expr(foo <- function() {
      1
    }), rlang::expr(foo2 <- function() {
      2
    }))
  )
})

cc$set_code(c("ADSL <- example_cdisc_data(\"ADSL\")", "ADSL$var <- 1"), "ADSL")
cc$set_code("ADSL$a <- foo()", "ADSL")

cc$set_code("ADSL_2 <- head(ADSL, 5)", "ADSL_2", deps = "ADSL")


cc$set_code("baz <- function() {2}")
cc$set_code("ADSL_2$a <- baz()", "ADSL_2")

test_that("example datasets", {
  expect_identical(
    cc$get_code("ADSL"),
    paste0(
      "foo <- function() {\n    1\n}\nfoo2 <- function() {\n    2\n}\n",
      "ADSL <- example_cdisc_data(\"ADSL\")\nADSL$var <- 1\nADSL$a <- foo()"
    )
  )
})

adsl_code <- cc$get_code("ADSL", deparse = FALSE)
adsl2_code <- cc$get_code("ADSL_2", deparse = FALSE)

test_that("example datasets deps", {
  expect_true(all(adsl_code %in% adsl2_code))
  expect_equal(adsl2_code[!adsl2_code %in% adsl_code], list(
    rlang::expr(ADSL_2 <- head(ADSL, 5)), # nolint
    rlang::expr(baz <- function() {
      2
    }),
    rlang::expr(ADSL_2$a <- baz()) # nolint
  ))
})

#########################################
#########################################
#########################################

x1 <- CodeClass$new()
x1$set_code("ADSL <- example_cdisc_data(\"ADSL\")", "ADSL")

x2 <- CodeClass$new()
x2$set_code("ADSL_2 <- head(ADSL, 5)", "ADSL_2", "ADSL")

x <- CodeClass$new()
x$append(x1)
x$append(x2)

test_that("CodeClass append", {
  expect_identical(x$get_code(), paste0(c(x1$get_code(), x2$get_code()), collapse = "\n"))
  expect_identical(x$get_code(deparse = FALSE), append(x1$get_code(deparse = FALSE), x2$get_code(deparse = FALSE)))
  expect_identical(
    x$get_code(c("ADSL", "ADSL_2")),
    paste0(c(x$get_code("ADSL"), x2$get_code("ADSL_2")), collapse = "\n")
  )
})


x3 <- CodeClass$new()
x3$set_code("ADRS <- example_cdisc_data(\"ADRS\")", "ADRS")
x$append(x3)


test_that("CodeClass append deps", {
  expect_identical(
    x$get_code(),
    paste0(
      "ADSL <- example_cdisc_data(\"ADSL\")\nADSL_2 <- head(ADSL, 5)\n",
      "ADRS <- example_cdisc_data(\"ADRS\")"
    )
  )
})

x$set_code("ADRS$x <- foo(ADSL$x)", c("ADRS"), deps = "ADSL")
x$set_code("", "ADRS")

test_that("CodeClass append deps", {
  expect_identical(
    x$get_code("ADRS"),
    paste0(
      "ADSL <- example_cdisc_data(\"ADSL\")\nADRS <- example_cdisc_data(\"ADRS\")\n",
      "ADRS$x <- foo(ADSL$x)\n"
    )
  )
  expect_equal(x$get_code("ADRS", deparse = FALSE), list(
    rlang::expr(ADSL <- example_cdisc_data("ADSL")), # nolint
    rlang::expr(ADRS <- example_cdisc_data("ADRS")), # nolint
    rlang::expr(ADRS$x <- foo(ADSL$x)) # nolint
  ))
  expect_identical(
    x$get_code("ADSL"),
    "ADSL <- example_cdisc_data(\"ADSL\")"
  )
})

test_that("Exception handling with dataname of *xyz", {
  x <- CodeClass$new()
  x$set_code("open_connection()", dataname = "*open")
  x$set_code("x1 <- foo()", dataname = "x1")
  x$set_code("x2 <- bar()", dataname = "x2")
  x$set_code("close_connection()", dataname = "*close")

  expect_identical(
    x$get_code(),
    "open_connection()\nx1 <- foo()\nx2 <- bar()\nclose_connection()"
  )
  expect_identical(
    x$get_code("x1"),
    "open_connection()\nx1 <- foo()\nclose_connection()"
  )
  expect_identical(
    x$get_code("x2"),
    "open_connection()\nx2 <- bar()\nclose_connection()"
  )

  # add mutation
  x$set_code("x1 <- baz(x1)", dataname = "x1")
  expect_identical(
    x$get_code(),
    "open_connection()\nx1 <- foo()\nx2 <- bar()\nclose_connection()\nx1 <- baz(x1)"
  )
  expect_identical(
    x$get_code("x1"),
    "open_connection()\nx1 <- foo()\nclose_connection()\nx1 <- baz(x1)"
  )
  expect_identical(
    x$get_code("x2"),
    "open_connection()\nx2 <- bar()\nclose_connection()"
  )
})


test_that("CodeClass list_to_code_class", {
  pull_adsl <- function(ADSL, n) ADSL <- head(teal.data::rADSL, n) # nolint
  adsl <- dataset_connector(
    dataname = "ADSL",
    pull_callable = callable_function(fun = pull_adsl) %>% # nolint
      set_args(list(ADSL = as.name("ADSL"))),
    keys = get_cdisc_keys("ADSL"),
    label = "ADSL connector"
  )

  pull_adae <- function(ADAE, n) ADSL <- head(teal.data::rADAE, n) # nolint
  adae <- dataset_connector(
    dataname = "ADAE",
    pull_callable = callable_function(fun = pull_adae) %>% # nolint
      set_args(list(ADAE = as.name("ADAE"))),
    keys = get_cdisc_keys("ADAE"),
    label = "ADAE connector"
  )

  adaem <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = adsl))
  adae <- dataset_connector(
    dataname = "ADAE",
    pull_callable = callable_function(fun = pull_adae) %>% # nolint
      set_args(list(ADAE = as.name("ADAE"))),
    keys = get_cdisc_keys("ADAE"),
    label = "ADAE connector"
  )
  adaem2 <- adae %>% mutate_dataset("ADAE$vv=nrow(ADSL); attr(ADSL$vv, 'label') <- 'vv'", vars = list(ADSL = ""))
  expect_true(inherits(adaem$get_code_class(), "CodeClass"))
  expect_true(inherits(adaem2$get_code_class(), "CodeClass"))
})

# Append duplicated code ====
test_that("Duplicated code is appended if it doesn't have a dataname", {
  cc1 <- CodeClass$new(code = "print('test')")
  cc2 <- CodeClass$new(code = "print('test')")
  cc1$append(cc2)
  expect_equal(
    cc1$get_code(),
    "print(\"test\")\nprint(\"test\")"
  )
})

test_that("Duplicated code is not appended if its dataname is duplicated", {
  cc1 <- CodeClass$new(code = "print('test')", dataname = "test")
  cc2 <- CodeClass$new(code = "print('test')", dataname = "test")
  expect_equal(
    cc1$get_code(),
    "print(\"test\")"
  )
})

test_that("Duplicated code is appended if its dataname is different", {
  cc1 <- CodeClass$new(code = "print('test')", dataname = "test1")
  cc2 <- CodeClass$new(code = "print('test')", dataname = "test2")
  cc1$append(cc2)
  expect_equal(
    cc1$get_code(),
    "print(\"test\")\nprint(\"test\")"
  )
})

test_that("list_to_code_class: assigning dataname to the object name inside of the list", {
  pull_fun <- callable_function(data.frame)
  pull_fun$set_args(args = list(head_letters = head(letters)))
  t_dc <- dataset_connector("test_dc", pull_fun)

  pull_fun2 <- callable_function(data.frame)
  pull_fun2$set_args(args = list(head_integers = 1:6))
  t_dc2 <- dataset_connector("test_dc2", pull_fun2)

  load_dataset(t_dc)
  load_dataset(t_dc2)

  mutate_dataset(t_dc, "t_dc2 <- NULL", vars = list(t_dc2 = t_dc2))
  expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "t_dc2 <- NULL"
    )
  )

  ds <- TealDataset$new("head_mtcars", x = head(mtcars), code = "head_mtcars <- head(mtcars)")

  mutate_dataset(t_dc, "test_dc$carb <- ds$carb", vars = list(ds = ds))
  expect_equal(
    pretty_code_string(t_dc$get_code()),
    c(
      "test_dc <- data.frame(head_letters = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"))",
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "t_dc2 <- test_dc2",
      "head_mtcars <- head(mtcars)",
      "ds <- head_mtcars",
      "t_dc2 <- NULL",
      "test_dc$carb <- ds$carb"
    )
  )

  ds2 <- TealDataset$new("head_iris", x = head(iris), code = "head_iris <- head(iris)")
  mutate_dataset(t_dc2, "test_dc2$Species <- head_iris$Species", vars = list(head_iris = ds2))
  expect_equal(
    pretty_code_string(t_dc2$get_code()),
    c(
      "test_dc2 <- data.frame(head_integers = 1:6)",
      "head_iris <- head(iris)",
      "test_dc2$Species <- head_iris$Species"
    )
  )
})
