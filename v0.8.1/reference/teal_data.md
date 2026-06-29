# Comprehensive data integration function for `teal` applications

Initializes a data for `teal` application.

## Usage

``` r
teal_data(..., join_keys = teal.data::join_keys(), code = character(0))

# S3 method for class 'teal_data'
x[names]
```

## Arguments

- ...:

  any number of objects (presumably data objects) provided as
  `name = value` pairs.

- join_keys:

  (`join_keys` or single `join_key_set`) optional object with datasets
  column names used for joining. If empty then no joins between pairs of
  objects.

- code:

  (`character`, `language`) optional code to reproduce the datasets
  provided in `...`. Note this code is not executed and the `teal_data`
  may not be reproducible

  Use
  [`verify()`](https://insightsengineering.github.io/teal.data/reference/verify.md)
  to verify code reproducibility.

- x:

  (`teal_data`)

- names:

  (`character`) names of objects included in `teal_subset` to subset

## Value

A `teal_data` object.

## Details

A `teal_data` is meant to be used for reproducibility purposes. The
class inherits from
[`teal.code::qenv`](https://insightsengineering.github.io/teal.code/latest-tag/reference/qenv.html)
and we encourage to get familiar with
[teal.code](https://CRAN.R-project.org/package=teal.code) first.
`teal_data` has following characteristics:

- It inherits from the environment and methods such as
  [`$`](https://rdrr.io/r/base/Extract.html),
  [`get()`](https://rdrr.io/r/base/get.html),
  [`ls()`](https://rdrr.io/r/base/ls.html),
  [`as.list()`](https://rdrr.io/r/base/list.html),
  [`parent.env()`](https://rdrr.io/r/base/environment.html) work out of
  the box.

- `teal_data` is a locked environment, and data modification is only
  possible through the
  [`teal.code::eval_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html)
  and
  [`within.qenv()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/within.qenv.html)
  functions.

- It stores metadata about the code used to create the data (see
  [`get_code()`](https://insightsengineering.github.io/teal.data/reference/get_code.md)).

- It supports slicing (see
  [`teal.code::subset-qenv`](https://insightsengineering.github.io/teal.code/latest-tag/reference/subset-qenv.html))

- Is immutable which means that each code evaluation does not modify the
  original `teal_data` environment directly.

- It maintains information about relationships between datasets (see
  [`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)).

## Subsetting

`x[names]` subsets objects in `teal_data` environment and limit the code
to the necessary needed to build limited objects.

## See also

[`teal.code::eval_code`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html),
[`get_code()`](https://insightsengineering.github.io/teal.data/reference/get_code.md),
[`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md),
[`names.teal_data()`](https://insightsengineering.github.io/teal.data/reference/names.teal_data.md)

## Examples

``` r
teal_data(x1 = iris, x2 = mtcars)
#> ✖ code unverified
#> <environment: 0x56514ab68c48> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x1: [data.frame]
#> - x2: [data.frame]


# Subsetting
data <- teal_data()
data <- eval_code(data, "a <- 1;b<-2")
data["a"]
#> ✅︎ code verified
#> <environment: 0x5651460cf7a8> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - a: [numeric]
data[c("a", "b")]
#> ✅︎ code verified
#> <environment: 0x56514f0772d0> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - a: [numeric]
#> - b: [numeric]

join_keys(data) <- join_keys(join_key("a", "b", "x"))
join_keys(data["a"]) # should show empty keys
#> An empty join_keys object. 
join_keys(data["b"])
#> A join_keys object containing foreign keys between 2 datasets:
#> a: [no primary keys]
#>   <-- b: [x]
#> b: [no primary keys]
#>   --> a: [x] 
join_keys(data)["a"] # should show empty keys
#> An empty join_keys object. 
join_keys(data)["b"]
#> A join_keys object containing foreign keys between 2 datasets:
#> a: [no primary keys]
#>   <-- b: [x]
#> b: [no primary keys]
#>   --> a: [x] 
```
