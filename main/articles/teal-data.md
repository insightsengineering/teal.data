# Introduction to teal.data

## Introduction

The `teal.data` package specifies the data format used in `teal`
applications.

A `teal_data` is meant to be used for reproducibility purposes. The
class inherits from
[`qenv`](https://insightsengineering.github.io/teal.code/latest-tag/articles/qenv.html)
and we encourage to get familiar with
[`teal.code`](https://insightsengineering.github.io/teal.code/latest-tag/)
first. `teal_data` has following characteristics:

- It inherits from the environment and methods such as `$`,
  [`get()`](https://rdrr.io/r/base/get.html),
  [`ls()`](https://rdrr.io/r/base/ls.html),
  [`as.list()`](https://rdrr.io/r/base/list.html) work out of the box.
- `teal_data` is a locked environment, and data modification is only
  possible through the
  [`teal.code::eval_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html)
  and `within.qenv()` functions.
- It stores metadata about the code used to create the data (see
  [reproducibility](#reproducibility)).
- It supports slicing by `[`.
- It is immutable which means that each code evaluation does not modify
  the original `teal_data` environment directly.
- It maintains information about relationships between datasets (see
  [Join Keys](#relational-data-models)).

### Quick Start

To create an object of class `teal_data`, use the `teal_data` function.
`teal_data` has a number of methods to interact with the object.

``` r

library(teal.data)

# create teal_data object
my_data <- teal_data()

# run code within teal_data to create data objects
my_data <- within(
  my_data,
  {
    data1 <- data.frame(id = 1:10, x = 11:20)
    data2 <- data.frame(id = 1:10, x = 21:30)
    data3 <- data.frame(id = 1:10, x = 31:40)
  }
)

# get objects stored in teal_data
my_data[["data1"]]
my_data[["data2"]]

# limit objects stored in teal_data
my_data[c("data1", "data3")]

# get reproducible code
get_code(my_data)

# get code just for specific object
get_code(my_data, names = "data2")

# get datanames
names(my_data)

# print
print(my_data)
```

#### Reproducibility

The primary function of `teal_data` is to provide reproducibility of
data. We recommend to initialize empty `teal_data`, which marks object
as *verified*, and create datasets by evaluating code in the object,
using `within` or `eval_code`. Read more in [teal_data
Reproducibility](https://insightsengineering.github.io/teal.data/articles/teal-data-reproducibility.md).

``` r

my_data <- teal_data()
my_data <- within(my_data, data <- data.frame(x = 11:20))
my_data <- within(my_data, data$id <- seq_len(nrow(data)))
my_data # is verified
```

    ## ✅︎ code verified
    ## <environment: 0x563d49fe1368> 🔒 
    ## Parent: <environment: package:teal.data> 
    ## Bindings:
    ## - data: [data.frame]

#### Relational data models

The `teal_data` class supports relational data. Relationships between
datasets can be described by joining keys and stored in a `teal_data`
object. These relationships can be read or set with the `join_keys`
function. See more in
[join_keys](https://insightsengineering.github.io/teal.data/articles/join-keys.md).

``` r

my_data <- teal_data()
my_data <- within(my_data, {
  data <- data.frame(id = 1:10, x = 11:20)
  child <- data.frame(id = 1:20, data_id = c(1:10, 1:10), y = 21:30)
})

join_keys(my_data) <- join_keys(
  join_key("data", "data", key = "id"),
  join_key("child", "child", key = "id"),
  join_key("child", "data", key = c("data_id" = "id"))
)

join_keys(my_data)
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## child: [id]
    ##   <-- data: [id]
    ## data: [id]
    ##   --> child: [data_id]

``` r

# join_keys for limited object
join_keys(my_data["child"])
```

    ## A join_keys object containing foreign keys between 1 datasets:
    ## child: [id]

#### Hidden objects

An object is hidden in `teal_data` if its name starts with a dot (`.`).
This can be used to pass auxiliary objects in the `teal_data` instance,
without being visible in the `teal` summary and filter panel.

``` r

my_data <- teal_data()
my_data <- within(my_data, {
  data <- data.frame(id = 1:10, x = 11:20)
  .data2 <- data.frame(id = 1:20, data_id = c(1:10, 1:10), y = 21:30)
})

ls(my_data)
```

    ## [1] "data"

``` r

names(my_data)
```

    ## [1] "data"
