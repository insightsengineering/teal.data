# Names of data sets in `teal_data` object

Functions to get the names of a `teal_data` object. The names are
obtained from the objects listed in the `qenv` environment.

## Usage

``` r
# S3 method for class 'teal_data'
names(x)
```

## Arguments

- x:

  A (`teal_data`) object to access or modify.

## Value

A character vector of names.

## Details

Objects named with a `.` (dot) prefix will be ignored and not returned.
To get the names of all objects, use `ls(x, all.names = TRUE)`, however,
it will not group the names by the join_keys topological structure.

In order to rename objects in the `teal_data` object, use base R
functions (see examples).

## Examples

``` r
td <- teal_data(iris = iris)
td <- within(td, mtcars <- mtcars)
names(td)
#> [1] "iris"   "mtcars"

# hidden objects with dot-prefix
td <- within(td, .CO2 <- CO2)
names(td) # '.CO2' will not be returned
#> [1] "iris"   "mtcars"

# rename objects
td <- teal_data(iris = iris)
td <- within(td, {
  new_iris <- iris
  rm(iris)
})
names(td) # only 'new_iris' will be returned
#> [1] "new_iris"
```
