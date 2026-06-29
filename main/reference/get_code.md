# Get code from `teal_data` object

Retrieve code from `teal_data` object.

## Usage

``` r
# S4 method for class 'teal_data'
get_code(
  object,
  deparse = TRUE,
  names = NULL,
  datanames = lifecycle::deprecated(),
  ...
)
```

## Arguments

- object:

  (`teal_data`)

- deparse:

  (`logical`) flag specifying whether to return code as `character`
  (`deparse = TRUE`) or as `expression` (`deparse = FALSE`).

- names:

  (`character`) Successor of `datanames`. Vector of dataset names to
  return the code for. For more details see the "Extracting
  dataset-specific code" section.

- datanames:

  **\[deprecated\]** (`character`) vector of dataset names to return the
  code for. For more details see the "Extracting dataset-specific code"
  section. Use `names` instead.

- ...:

  Parameters passed to internal methods. Currently, the only supported
  parameter is `check_names` (`logical(1)`) flag, which is `TRUE` by
  default. Function warns about missing objects, if they do not exist in
  `code` but are passed in `datanames`. To remove the warning, set
  `check_names = FALSE`.

## Value

Either a character string or an expression. If `names` is used to
request a specific dataset, only code that *creates* that dataset (not
code that uses it) is returned. Otherwise, all contents of `@code`.

## Details

Retrieve code stored in `@code`, which (in principle) can be used to
recreate all objects found in the environment (`@.xData`). Use `names`
to limit the code to one or more of the datasets enumerated in the
environment.

## Extracting dataset-specific code

When `names` is specified, the code returned will be limited to the
lines needed to *create* the requested datasets. The code stored in the
`@code` slot is analyzed statically to determine which lines the
datasets of interest depend upon. The analysis works well when objects
are created with standard infix assignment operators (see
[`?assignOps`](https://rdrr.io/r/base/assignOps.html)) but it can fail
in some situations.

Consider the following examples:

*Case 1: Usual assignments.*

    data <- teal_data() |>
      within({
        foo <- function(x) {
          x + 1
        }
        x <- 0
        y <- foo(x)
      })
    get_code(data, names = "y")

`x` has no dependencies, so `get_code(data, names = "x")` will return
only the second call.\
`y` depends on `x` and `foo`, so `get_code(data, names = "y")` will
contain all three calls.

*Case 2: Some objects are created by a function's side effects.*

    data <- teal_data() |>
      within({
        foo <- function() {
          x <<- x + 1
        }
        x <- 0
        foo()
        y <- x
      })
    get_code(data, names = "y")

Here, `y` depends on `x` but `x` is modified by `foo` as a side effect
(not by reassignment) and so `get_code(data, names = "y")` will not
return the `foo()` call.\
To overcome this limitation, code dependencies can be specified
manually. Lines where side effects occur can be flagged by adding
"`# @linksto <object name>`" at the end.\
Note that `within` evaluates code passed to `expr` as is and comments
are ignored. In order to include comments in code one must use the
`eval_code` function instead.

    data <- teal_data() |>
      eval_code("
        foo <- function() {
          x <<- x + 1
        }
        x <- 0
        foo() # @linksto x
        y <- x
      ")
    get_code(data, names = "y")

Now the `foo()` call will be properly included in the code required to
recreate `y`.

Note that two functions that create objects as side effects, `assign`
and `data`, are handled automatically.

Here are known cases where manual tagging is necessary:

- non-standard assignment operators, *e.g.* `%<>%`

- objects used as conditions in `if` statements: `if (<condition>)`

- objects used to iterate over in `for` loops: `for(i in <sequence>)`

- creating and evaluating language objects, *e.g.* `eval(<call>)`

## Examples

``` r
tdata1 <- teal_data()
tdata1 <- within(tdata1, {
  a <- 1
  b <- a^5
  c <- list(x = 2)
})
get_code(tdata1)
#> [1] "a <- 1\nb <- a^5\nc <- list(x = 2)"
get_code(tdata1, names = "a")
#> [1] "a <- 1"
get_code(tdata1, names = "b")
#> [1] "a <- 1\nb <- a^5"

tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
get_code(tdata2)
#> [1] "x1 <- iris"
get_code(verify(tdata2))
#> [1] "x1 <- iris"
```
