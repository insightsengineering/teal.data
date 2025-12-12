# Manage relationships between datasets using `join_keys`

Facilitates the creation and retrieval of relationships between
datasets. `join_keys` class extends `list` and contains keys connecting
pairs of datasets. Each element of the list contains keys for specific
dataset. Each dataset can have a relationship with itself (primary key)
and with other datasets.

Note that `join_keys` list is symmetrical and assumes a default
direction, that is: when keys are set between `ds1` and `ds2`, it
defines `ds1` as the parent in a parent-child relationship and the
mapping is automatically mirrored between `ds2` and `ds1`.

## Usage

``` r
## Constructor, getter and setter
join_keys(...)

# Default S3 method
join_keys(...)

# S3 method for class 'join_keys'
join_keys(...)

# S3 method for class 'teal_data'
join_keys(...)

# S3 method for class 'join_keys'
x[i, j]

# S3 method for class 'join_keys'
x[i, j, directed = TRUE] <- value

# S3 method for class 'join_keys'
c(...)

# S3 method for class 'join_key_set'
c(...)

join_keys(x) <- value

# S3 method for class 'join_keys'
join_keys(x) <- value

# S3 method for class 'teal_data'
join_keys(x) <- value

# S3 method for class 'join_keys'
format(x, ...)

# S3 method for class 'join_keys'
print(x, ...)
```

## Arguments

- ...:

  optional,

  - either `teal_data` or `join_keys` object to extract `join_keys`

  - or any number of `join_key_set` objects to create `join_keys`

  - or nothing to create an empty `join_keys`

- x:

  (`join_keys`) empty object to set the new relationship pairs. `x` is
  typically an object of `join_keys` class. When called with the
  `join_keys(x)` or `join_keys(x) <- value` then it can also take a
  supported class (`teal_data`, `join_keys`)

- i, j:

  indices specifying elements to extract or replace. Index should be a a
  character vector, but it can also take numeric, logical, `NULL` or
  missing.

- directed:

  (`logical(1)`) Flag that indicates whether it should create a
  parent-child relationship between the datasets.

  - `TRUE` (default) `dataset_1` is the parent of `dataset_2`;

  - `FALSE` when the relationship is undirected.

- value:

  For `x[i, j, directed = TRUE] <- value` (named/unnamed `character`)
  Column mapping between datasets.

  For `join_keys(x) <- value`: (`join_key_set` or list of
  `join_key_set`) relationship pairs to add to `join_keys` list.

## Value

`join_keys` object.

## Methods (by class)

- `join_keys()`: Returns an empty `join_keys` object when called without
  arguments.

- `join_keys(join_keys)`: Returns itself.

- `join_keys(teal_data)`: Returns the `join_keys` object contained in
  `teal_data` object.

- `join_keys(...)`: Creates a new object with one or more `join_key_set`
  parameters.

## Functions

- `x[names]`: Returns a subset of the `join_keys` object for given
  `names`, including parent `names` and symmetric mirror keys between
  `names` in the result.

- `x[i, j]`: Returns join keys between datasets `i` and `j`, including
  implicit keys inferred from their relationship with a parent.

&nbsp;

- `x[i, j] <- value`: Assignment of a key to pair `(i, j)`.

- `x[i] <- value`: This (without `j` parameter) **is not** a supported
  operation for `join_keys`.

- `join_keys(x)[i, j] <- value`: Assignment to `join_keys` object stored
  in `x`, such as a `teal_data` object or `join_keys` object itself.

&nbsp;

- `join_keys(x) <- value`: Assignment of the `join_keys` in object with
  `value`. `value` needs to be an object of class `join_keys` or
  `join_key_set`.

## See also

[`join_key()`](https://insightsengineering.github.io/teal.data/reference/join_key.md)
for creating `join_keys_set`,
[`parents()`](https://insightsengineering.github.io/teal.data/reference/parents.md)
for parent operations,
[`teal_data()`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)
for `teal_data` constructor *and*
[default_cdisc_join_keys](https://insightsengineering.github.io/teal.data/reference/default_cdisc_join_keys.md)
for default CDISC keys.

## Examples

``` r
# Creating a new join keys ----

jk <- join_keys(
  join_key("ds1", "ds1", "pk1"),
  join_key("ds2", "ds2", "pk2"),
  join_key("ds3", "ds3", "pk3"),
  join_key("ds1", "ds2", c(pk1 = "pk2")),
  join_key("ds1", "ds3", c(pk1 = "pk3"))
)

jk
#> A join_keys object containing foreign keys between 3 datasets:
#> ds1: [pk1]
#>   <-- ds2: [pk2]
#>   <-- ds3: [pk3]
#> ds2: [pk2]
#>   --> ds1: [pk1]
#>   --* (implicit via parent with): ds3
#> ds3: [pk3]
#>   --> ds1: [pk1]
#>   --* (implicit via parent with): ds2 

# Getter for join_keys ---

jk["ds1", "ds2"]
#>   pk1 
#> "pk2" 

# Subsetting join_keys ----

jk["ds1"]
#> A join_keys object containing foreign keys between 1 datasets:
#> ds1: [pk1] 
jk[1:2]
#> A join_keys object containing foreign keys between 2 datasets:
#> ds1: [pk1]
#>   <-- ds2: [pk2]
#> ds2: [pk2]
#>   --> ds1: [pk1] 
jk[c("ds1", "ds2")]
#> A join_keys object containing foreign keys between 2 datasets:
#> ds1: [pk1]
#>   <-- ds2: [pk2]
#> ds2: [pk2]
#>   --> ds1: [pk1] 

# Setting a new primary key ---

jk["ds4", "ds4"] <- "pk4"
jk["ds5", "ds5"] <- "pk5"

# Setting a single relationship pair ---

jk["ds1", "ds4"] <- c("pk1" = "pk4")

# Removing a key ---

jk["ds5", "ds5"] <- NULL
# Merging multiple `join_keys` objects ---

jk_merged <- c(
  jk,
  join_keys(
    join_key("ds4", keys = c("pk4", "pk4_2")),
    join_key("ds3", "ds4", c(pk3 = "pk4_2"))
  )
)
# note: merge can be performed with both join_keys and join_key_set

jk_merged <- c(
  jk_merged,
  join_key("ds5", keys = "pk5"),
  join_key("ds1", "ds5", c(pk1 = "pk5"))
)
# Assigning keys via join_keys(x)[i, j] <- value ----

obj <- join_keys()
# or
obj <- teal_data()

join_keys(obj)["ds1", "ds1"] <- "pk1"
join_keys(obj)["ds2", "ds2"] <- "pk2"
join_keys(obj)["ds3", "ds3"] <- "pk3"
join_keys(obj)["ds1", "ds2"] <- c(pk1 = "pk2")
join_keys(obj)["ds1", "ds3"] <- c(pk1 = "pk3")

identical(jk, join_keys(obj))
#> [1] FALSE
# Setter for join_keys within teal_data ----

td <- teal_data()
join_keys(td) <- jk

join_keys(td)["ds1", "ds2"] <- "new_key"
join_keys(td) <- c(join_keys(td), join_keys(join_key("ds3", "ds2", "key3")))
join_keys(td)
#> A join_keys object containing foreign keys between 4 datasets:
#> ds1: [pk1]
#>   <-> ds2: [new_key]
#>   <-- ds3: [pk3]
#>   <-- ds4: [pk4]
#> ds3: [pk3]
#>   --> ds1: [pk1]
#>   <-- ds2: [key3]
#>   --* (implicit via parent with): ds4
#> ds2: [pk2]
#>   <-> ds1: [new_key]
#>   --> ds3: [key3]
#> ds4: [pk4]
#>   --> ds1: [pk1]
#>   --* (implicit via parent with): ds3 
```
