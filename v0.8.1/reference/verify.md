# Verify code reproducibility

Checks whether code in `teal_data` object reproduces the stored objects.

## Usage

``` r
verify(x)
```

## Arguments

- x:

  `teal_data` object

## Value

Input `teal_data` object or error.

## Details

If objects created by code in the `@code` slot of `x` are `all_equal` to
the contents of the environment (`@.xData` slot), the function updates
the `@verified` slot to `TRUE` in the returned `teal_data` object. Once
verified, the slot will always be set to `TRUE`. If the `@code` fails to
recreate objects in `teal_data`'s environment, an error is raised.

## Examples

``` r
tdata1 <- teal_data()
tdata1 <- within(tdata1, {
  a <- 1
  b <- a^5
  c <- list(x = 2)
})
verify(tdata1)
#> ✅︎ code verified
#> <environment: 0x56514ac013b8> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - a: [numeric]
#> - b: [numeric]
#> - c: [list]

tdata2 <- teal_data(x1 = iris, code = "x1 <- iris")
verify(tdata2)
#> ✅︎ code verified
#> <environment: 0x5651464d16d8> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x1: [data.frame]
verify(tdata2)@verified
#> [1] TRUE
tdata2@verified
#> [1] FALSE

tdata3 <- teal_data()
tdata3 <- within(tdata3, {
  stop("error")
})
try(verify(tdata3)) # fails
#> Error : error 
#>  when evaluating qenv code:
#> stop("error")


a <- 1
b <- a + 2
c <- list(x = 2)
d <- 5
tdata4 <- teal_data(
  a = a, b = b, c = c, d = d,
  code = "a <- 1
          b <- a
          c <- list(x = 2)
          e <- 1"
)
tdata4
#> ✖ code unverified
#> <environment: 0x56514ea36c68> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - a: [numeric]
#> - b: [numeric]
#> - c: [list]
#> - d: [numeric]
if (FALSE) { # \dontrun{
verify(tdata4) # fails
} # }
```
