# Variable labels

Get or set variable labels in a `data.frame`.

## Usage

``` r
col_labels(x, fill = FALSE)

col_labels(x) <- value

col_relabel(x, ...)
```

## Source

These functions were taken from
[formatters](https://cran.r-project.org/package=formatters) package, to
reduce the complexity of the dependency tree and rewritten.

## Arguments

- x:

  (`data.frame` or `DataFrame`) data object

- fill:

  (`logical(1)`) specifying what to return if variable has no label

- value:

  (`character`) vector of variable labels of length equal to number of
  columns in `x`; if named, names must match variable names in `x` and
  will be used as key to set labels; use `NA` to remove label from
  variable

- ...:

  name-value pairs, where name corresponds to a variable name in `x` and
  value is the new variable label; use `NA` to remove label from
  variable

## Value

For `col_labels`, named character vector of variable labels, the names
being the corresponding variable names. If the `label` attribute is
missing, the vector elements will be the variable names themselves if
`fill = TRUE` and `NA` if `fill = FALSE`.

For `col_labels<-` and `col_relabel`, copy of `x` with variable labels
modified.

## Details

Variable labels can be stored as a `label` attribute set on individual
variables. These functions get or set this attribute, either on all
(`col_labels`) or some variables (`col_relabel`).

## Examples

``` r
x <- iris
col_labels(x)
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
#>           NA           NA           NA           NA           NA 
col_labels(x) <- paste("label for", names(iris))
col_labels(x)
#>             Sepal.Length              Sepal.Width             Petal.Length 
#> "label for Sepal.Length"  "label for Sepal.Width" "label for Petal.Length" 
#>              Petal.Width                  Species 
#>  "label for Petal.Width"      "label for Species" 
y <- col_relabel(x, Sepal.Length = "Sepal Length of iris flower")
col_labels(y)
#>                  Sepal.Length                   Sepal.Width 
#> "Sepal Length of iris flower"       "label for Sepal.Width" 
#>                  Petal.Length                   Petal.Width 
#>      "label for Petal.Length"       "label for Petal.Width" 
#>                       Species 
#>           "label for Species" 
```
