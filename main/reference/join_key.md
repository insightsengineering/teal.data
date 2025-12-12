# Create a relationship between a pair of datasets

Create a relationship between two datasets, `dataset_1` and `dataset_2`.
By default, this function establishes a directed relationship with
`dataset_1` as the parent. If `dataset_2` is not specified, the function
creates a primary key for `dataset_1`.

## Usage

``` r
join_key(dataset_1, dataset_2 = dataset_1, keys, directed = TRUE)
```

## Arguments

- dataset_1, dataset_2:

  (`character(1)`) Dataset names. When `dataset_2` is omitted, a primary
  key for `dataset_1` is created.

- keys:

  (optionally named `character`) Column mapping between the datasets,
  where `names(keys)` maps columns in `dataset_1` corresponding to
  columns of `dataset_2` given by the elements of `keys`.

  - If unnamed, the same column names are used for both datasets.

  - If any element of the `keys` vector is empty with a non-empty name,
    then the name is used for both datasets.

- directed:

  (`logical(1)`) Flag that indicates whether it should create a
  parent-child relationship between the datasets.

  - `TRUE` (default) `dataset_1` is the parent of `dataset_2`;

  - `FALSE` when the relationship is undirected.

## Value

object of class `join_key_set` to be passed into `join_keys` function.

## See also

[`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md),
[`parents()`](https://insightsengineering.github.io/teal.data/reference/parents.md)

## Examples

``` r
join_key("d1", "d2", c("A"))
#> $d1
#> $d1$d2
#>   A 
#> "A" 
#> 
#> 
#> attr(,"class")
#> [1] "join_key_set"
#> attr(,"parents")
#> attr(,"parents")$d2
#> [1] "d1"
#> 
join_key("d1", "d2", c("A" = "B"))
#> $d1
#> $d1$d2
#>   A 
#> "B" 
#> 
#> 
#> attr(,"class")
#> [1] "join_key_set"
#> attr(,"parents")
#> attr(,"parents")$d2
#> [1] "d1"
#> 
join_key("d1", "d2", c("A" = "B", "C"))
#> $d1
#> $d1$d2
#>   A   C 
#> "B" "C" 
#> 
#> 
#> attr(,"class")
#> [1] "join_key_set"
#> attr(,"parents")
#> attr(,"parents")$d2
#> [1] "d1"
#> 
```
