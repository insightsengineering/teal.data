# Show `teal_data` object

Prints `teal_data` object.

## Usage

``` r
# S4 method for class 'teal_data'
show(object)
```

## Arguments

- object:

  (`teal_data`)

## Value

Input `teal_data` object.

## Examples

``` r
teal_data()
#> ✅︎ code verified
#> <environment: 0x559c03d8dd90> 🔒 
#> Parent: <environment: devtools_shims> 
teal_data(x = iris, code = "x = iris")
#> ✖ code unverified
#> <environment: 0x559c037514e8> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x: [data.frame]
verify(teal_data(x = iris, code = "x = iris"))
#> ✅︎ code verified
#> <environment: 0x559c00f6a5f8> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x: [data.frame]
```
