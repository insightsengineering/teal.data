# Get and set parents in `join_keys` object

`parents()` facilitates the creation of dependencies between datasets by
assigning a parent-child relationship.

## Usage

``` r
parents(x)

# S3 method for class 'join_keys'
parents(x)

# S3 method for class 'teal_data'
parents(x)

parents(x) <- value

# S3 method for class 'join_keys'
parents(x) <- value

# S3 method for class 'teal_data'
parents(x) <- value

parent(x, dataset_name)
```

## Arguments

- x:

  (`join_keys` or `teal_data`) object that contains "parents"
  information to retrieve or manipulate.

- value:

  (`named list`) of `character` vectors.

- dataset_name:

  (`character(1)`) Name of dataset to query on their parent.

## Value

a `list` of `character` representing the parents.

For `parent(x, dataset_name)` returns `NULL` if parent does not exist.

## Details

Each element is defined by a `list` element, where
`list("child" = "parent")`.

## Methods (by class)

- `parents(join_keys)`: Retrieves parents of `join_keys` object.

- `parents(teal_data)`: Retrieves parents of `join_keys` inside
  `teal_data` object.

## Functions

- `parents(x) <- value`: Assignment of parents in `join_keys` object.

- `parents(join_keys) <- value`: Assignment of parents of `join_keys`
  object.

- `parents(teal_data) <- value`: Assignment of parents of `join_keys`
  inside `teal_data` object.

- `parent()`: Getter for individual parent.

## See also

[`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)

## Examples

``` r
# Get parents of join_keys ---

jk <- default_cdisc_join_keys["ADEX"]
parents(jk)
#> $ADEX
#> [1] "ADSL"
#> 
# Get parents of join_keys inside teal_data object ---

td <- teal_data(
  ADSL = rADSL,
  ADTTE = rADTTE,
  ADRS = rADRS,
  join_keys = default_cdisc_join_keys[c("ADSL", "ADTTE", "ADRS")]
)
parents(td)
#> $ADTTE
#> [1] "ADSL"
#> 
#> $ADRS
#> [1] "ADSL"
#> 
# Assignment of parents ---

jk <- join_keys(
  join_key("ds1", "ds2", "id"),
  join_key("ds5", "ds6", "id"),
  join_key("ds7", "ds6", "id")
)

parents(jk) <- list(ds2 = "ds1")

# Setting individual parent-child relationship

parents(jk)["ds6"] <- "ds5"
parents(jk)["ds7"] <- "ds6"
# Assignment of parents of join_keys inside teal_data object ---

parents(td) <- list("ADTTE" = "ADSL") # replace existing
parents(td)["ADRS"] <- "ADSL" # add new parent
# Get individual parent ---

parent(jk, "ds2")
#> [1] "ds1"
parent(td, "ADTTE")
#> [1] "ADSL"
```
