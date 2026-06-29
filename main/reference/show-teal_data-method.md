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
#> <environment: 0x55baba25e318> 🔒 
#> Parent: <environment: devtools_shims> 
teal_data(x = iris, code = "x = iris")
#> ✖ code unverified
#> <environment: 0x55bab936a090> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x: [data.frame]
verify(teal_data(x = iris, code = "x = iris"))
#> ✅︎ code verified
#> <environment: 0x55bab989ca60> 🔒 
#> Parent: <environment: devtools_shims> 
#> Bindings:
#> - x: [data.frame]
```
