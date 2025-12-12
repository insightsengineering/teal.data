# Join Keys

## Overview

The `teal.data` package provides a way to define primary keys for a
dataset and establish relationships with other datasets.

Each dataset *joining keys* can be characterized by:

- Columns constituting the primary key;
- Foreign/merge keys, analogous to `SQL` foreign keys.

Typically, an application developer specifies these keys manually.
However, for datasets following the ADaM standard, `teal.data` can
automatically assign keys using the `default_cdisc_join_keys` object.
Refer to the section [“Joining Keys with ADaM
Datasets”](#join-keys-in-adam-datasets) for details on using this object
to select specific datasets.

##### Uses of `join_keys` class in *teal* applications

The primary function of the `join_keys` class in `teal` applications is
to facilitate the seamless [merging of
datasets](https://insightsengineering.github.io/teal.transform/latest-tag/articles/data-merge.html)
using `teal.transform`.

Additionally, it plays a role in the data filtering using the *[Filter
Panel](https://insightsengineering.github.io/teal/latest-tag/articles/filter-panel.html)*
in a `teal` application. The filters applied to a (parent) dataset are
also applied to their children.

## Anatomy of `join_keys`

The `join_keys` object contains information about the foreign/primary
keys of multiple datasets. Each key is represented by a pair of datasets
(by name reference) and a named character vector that encodes the column
name mapping between the two datasets. In addition, a foreign key may
also contain a *parent-child* attribute that is used in the “Filter
Panel” as we mentioned above.

A new join keys can be created as an empty object, or by defining an
initial set of primary and foreign keys.

That initial object can be extended by adding/modifying/removing keys
and by establishing parent-child relationships between datasets.

##### `join_keys(...)`: Join Keys Constructor / Getter / Setter

Convenient function that is used both as the constructor and as the
getter for *join_keys* objects.

As the *Getter* it is used to retrieve the *joining keys* that are
contained in other objects, such as a `teal_data` object.

As the *Constructor* it is used to specify a collection of multiple
individual keys (via `join_key` function described below).

##### `join_key(dataset_1, dataset_2, key, parent)`: Single Join Key Constructor

Specifies a primary key or a relationship between two datasets.

- `dataset_1`, `dataset_2`: names of the datasets (if `dataset_2` is the
  same as `dataset_1` or is omitted, it creates a primary key);
- `key`: named `character` vector of column name mapping between
  datasets (unnamed vector assumes column names between datasets are the
  same);
- `directed` (optional): flag that indicates whether there is a
  *parent-child* directed relationship between `dataset_2` to
  `dataset_1` (latter as a parent).

Note that join keys are assumed to be symmetric, i.e.,
`join_key("ds1", "ds2", c("ds1_col" = "ds2_col"))` establishes a
relationship from “x” to “y” and vice versa.

By default, the new joining key will set the `dataset_1` as the parent.

##### Example & Output

``` r
library(teal.data)
jk <- join_keys(
  join_key("ds1", keys = "col_1"), # ds1: [col_1]
  join_key("ds2", keys = c("col_1", "col_2")), # ds2: [col_1, col_2]
  join_key("ds3", keys = c("col_1", "col_3")), # ds3: [col_1, col_3]
  join_key("ds1", "ds2", keys = "col_1"), # ds1 <-- ds2
  join_key("ds1", "ds3", keys = "col_1"), # ds1 <-- ds3
  join_key("ds4", "ds5", keys = c("col_4" = "col_5"), directed = FALSE) # ds4 <--> ds5
)

# The parent-child relationships are created automatically (unless 'parent' parameter is "none")
jk
```

[TABLE]

## Accessing and Modifying keys

The *subset* operator with 2 indices (`x[i, j]`) is used to retrieve the
primary/foreign keys. Both indices must be a string denoting the dataset
name.

``` r
# Using the jk object defined in "Anatomy of Join Keys"
jk
```

    ## A join_keys object containing foreign keys between 5 datasets:
    ## ds1: [col_1]
    ##   <-- ds2: [col_1]
    ##   <-- ds3: [col_1]
    ## ds2: [col_1, col_2]
    ##   --> ds1: [col_1]
    ##   --* (implicit via parent with): ds3
    ## ds3: [col_1, col_3]
    ##   --> ds1: [col_1]
    ##   --* (implicit via parent with): ds2
    ## ds4: [no primary keys]
    ##   <-> ds5: [col_5]
    ## ds5: [no primary keys]
    ##   <-> ds4: [col_4]

``` r
# Getting primary key of "ds1"
jk["ds1", "ds1"]
```

    ##   col_1 
    ## "col_1"

``` r
# Getting foreign keys between "ds4" and "ds5"
jk["ds4", "ds5"]
```

    ##   col_4 
    ## "col_5"

Note that there is a symmetry in the keys between `ds4` and `ds5`
relationship:

``` r
jk["ds5", "ds4"]
```

    ##   col_5 
    ## "col_4"

``` r
jk["ds5", "ds4"]
```

    ##   col_5 
    ## "col_4"

When only 1 argument is used this operator will return a `join_keys`
object that is filtered accordingly.

``` r
# Using the jk object defined in "Anatomy of Join Keys"
jk
```

    ## A join_keys object containing foreign keys between 5 datasets:
    ## ds1: [col_1]
    ##   <-- ds2: [col_1]
    ##   <-- ds3: [col_1]
    ## ds2: [col_1, col_2]
    ##   --> ds1: [col_1]
    ##   --* (implicit via parent with): ds3
    ## ds3: [col_1, col_3]
    ##   --> ds1: [col_1]
    ##   --* (implicit via parent with): ds2
    ## ds4: [no primary keys]
    ##   <-> ds5: [col_5]
    ## ds5: [no primary keys]
    ##   <-> ds4: [col_4]

``` r
# Getting primary key of "ds1"
jk["ds1", "ds1"]
```

    ##   col_1 
    ## "col_1"

``` r
# Getting keys of "ds1" and "ds2"
jk[c("ds1", "ds2")]
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## ds1: [col_1]
    ##   <-- ds2: [col_1]
    ## ds2: [col_1, col_2]
    ##   --> ds1: [col_1]

Modifying or adding a key uses the same notation with the assignment
operator `<-`. A symmetric relationship will be created automatically,
where the parent (by default) will be the dataset defined in the first
index. Assigning `NULL` value will delete the relationship.

``` r
# Adding a new ds5 <-- ds1 key
jk["ds1", "ds5"] <- "a_column"

# Removing an existing key
jk["ds4", "ds5"] <- NULL
```

## Merging Join Keys

The combining of multiple *joining keys* is achieved using the
[`c()`](https://rdrr.io/r/base/c.html) generic function, which generates
a symmetric and valid `join_keys` object. When encountering identical
relationship pairs, this operation retains the last occurrence of the
pair within the specified argument order.

For added convenience, the function also accommodates `join_key_set`
objects created through the `join_key` function. These objects can be
provided as the initial argument or in any other position as needed.

``` r
jk1 <- join_keys(join_key("ds1", "ds1", "col_1"))
jk2 <- join_keys(join_key("ds2", "ds2", "col_1"), join_key("ds1", "ds2", "col_1"))

# Merging
c(jk1, jk2)
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## ds1: [col_1]
    ##   <-- ds2: [col_1]
    ## ds2: [col_1]
    ##   --> ds1: [col_1]

``` r
# Keeping last occurence
c(jk1, jk2, join_keys(join_key("ds2", "ds2", "col_2"), join_key("ds1", "ds2", c("col_1" = "col_2"))))
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## ds1: [col_1]
    ##   <-- ds2: [col_2]
    ## ds2: [col_2]
    ##   --> ds1: [col_1]

``` r
# Merges join_key and join_key_set objects (from join_key function)
c(jk1, join_key("ds3", "ds3", "col_3"))
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## ds1: [col_1]
    ## ds3: [col_3]

## Join Keys Relationships

There are 2 types of relationships encoded with *joining keys* that are
described in the following sections. The *primary* and *foreign* keys
are created explicitly using the constructor for individual keys
(`join_key`).

Additionally, the `join_keys` object infers implicit relationships when
two datasets share foreign keys to a parent dataset, but not between
themselves. These implicit relationships are available just like another
foreign key and can be used to merge datasets, despite not being defined
by the user.

### Primary Key with `teal_data`

When using the `teal_data` function, the simplest method to define the
join keys is to use the `join_keys` argument. We can specify the
column(s) of the dataset that (together) uniquely identify rows in the
dataset.

``` r
library(teal.data)

td_pk <- within(
  teal_data(),
  ds1 <- transform(iris, id = seq_len(nrow(iris)))
)

join_keys(td_pk) <- join_keys(join_key("ds1", keys = "id"))

join_keys(td_pk)
```

    ## A join_keys object containing foreign keys between 1 datasets:
    ## ds1: [id]

We can extend the previous example and define primary keys for multiple
datasets:

``` r
td_pk <- within(
  td_pk,
  {
    ds2 <- data.frame(W = 10:1, V = 5:14, M = rep(1:5, 2))
    ds3 <- data.frame(V = 5:14, N = 4)
  }
)

join_keys(td_pk)["ds2", "ds2"] <- c("V", "W")
join_keys(td_pk)["ds3", "ds3"] <- c("V", "W")

join_keys(td_pk)
```

    ## A join_keys object containing foreign keys between 3 datasets:
    ## ds1: [id]
    ## ds2: [V, W]
    ## ds3: [V, W]

### Foreign Keys with `teal_data`

When passing multiple datasets to the `teal_data` function, dataset
relationships are set using `join_keys` and `join_key` functions, which
then can be used to merge datasets together within `teal` apps. For
users familiar with `SQL` database schema, these relationships are
symmetric and not as strict as `SQL` foreign key relationships as `teal`
does not validate whether the values defined as foreign key columns are
present in the table.

For example:

``` r
library(teal.data)

td_fk <- within(
  teal_data(),
  {
    ds1 <- data.frame(X = 1:10, Y = 21:30, Z = 1:10)
    ds2 <- data.frame(W = 10:1, V = 5:14, M = rep(1:5, 2))
    ds3 <- data.frame(V = 5:14, N = 4)
  }
)

join_keys(td_fk) <- join_keys(
  # Primary keys
  join_key("ds1", keys = c("X")),
  join_key("ds2", keys = c("V", "W")),
  join_key("ds3", keys = c("V")),
  # Foreign keys
  join_key("ds1", "ds2", c("X" = "W")),
  join_key("ds2", "ds3", c("V" = "V"))
)

join_keys(td_fk)
```

    ## A join_keys object containing foreign keys between 3 datasets:
    ## ds1: [X]
    ##   <-- ds2: [W]
    ## ds2: [V, W]
    ##   --> ds1: [X]
    ##   <-- ds3: [V]
    ## ds3: [V]
    ##   --> ds2: [V]

### Implicit Relationships

Two datasets that share common *foreign* keys to the same *parent*
dataset have an implicit relationship between them that is modeled and
accessible in *joining keys*.

This is a special relationship that is inferred from existing foreign
keys. It does not need to be explicitly defined but it can be accessed
and overwritten just as any other foreign key.

These implicit relationships can be used to merge 2 datasets together,
just as if they were defined manually.

``` r
library(teal.data)

td <- within(
  teal_data(),
  {
    ds1 <- data.frame(X = 1:10, Y = 21:30, Z = 1:10)
    ds2 <- data.frame(W = 10:1, V = 5:14, M = rep(1:5, 2))
    ds3 <- data.frame(V = 5:14, N = 4)
    ds4 <- data.frame(V = 5:14, R = rnorm(10))
  }
)

join_keys(td) <- join_keys(
  # Primary keys
  join_key("ds1", keys = c("X")),
  join_key("ds2", keys = c("V", "W")),
  join_key("ds3", keys = c("V")),
  join_key("ds4", keys = c("V")),
  # Foreign keys
  join_key("ds1", "ds2", c("X" = "W")),
  join_key("ds2", "ds3", c("V" = "V")),
  join_key("ds1", "ds4", c("X" = "B"))
)

join_keys(td)
```

    ## A join_keys object containing foreign keys between 4 datasets:
    ## ds1: [X]
    ##   <-- ds2: [W]
    ##   <-- ds4: [B]
    ## ds2: [V, W]
    ##   --> ds1: [X]
    ##   <-- ds3: [V]
    ##   --* (implicit via parent with): ds4
    ## ds3: [V]
    ##   --> ds2: [V]
    ## ds4: [V]
    ##   --> ds1: [X]
    ##   --* (implicit via parent with): ds2

``` r
join_keys(td)["ds2", "ds4"]
```

    ##   W 
    ## "B"

Note that the definition above contains no `join_key` for
`"ds2" <-> "ds4"`

## *Join Keys* in ADaM Datasets

`teal.data` provides a set of default join keys (primary and foreign)
for datasets named according to the ADaM standard. They are stored in
`default_cdisc_join_keys`.

``` r
names(default_cdisc_join_keys) |> sort()
```

    ##  [1] "ADAE"     "ADAETTE"  "ADCM"     "ADCSSRS"  "ADDV"     "ADEG"    
    ##  [7] "ADEQ5D5L" "ADEX"     "ADHY"     "ADLB"     "ADMH"     "ADQLQC"  
    ## [13] "ADQS"     "ADRS"     "ADSAFTTE" "ADSL"     "ADSUB"    "ADTTE"   
    ## [19] "ADVS"

When not all default keys are required users can select a smaller set of
datasets by subsetting (for example:
`default_cdisc_join_keys[c("ADSL", "ADTTE")]`). Please note, that for
every element that is selected, its parent will also be returned (if it
has one) as well as any pair-wise relationships in the resulting
selection.

``` r
default_cdisc_join_keys
```

    ## A join_keys object containing foreign keys between 19 datasets:
    ## ADSL: [STUDYID, USUBJID]
    ##   <-- ADAE: [STUDYID, USUBJID]
    ##   <-- ADEG: [STUDYID, USUBJID]
    ##   <-- ADTTE: [STUDYID, USUBJID]
    ##   <-- ADAETTE: [STUDYID, USUBJID]
    ##   <-- ADCM: [STUDYID, USUBJID]
    ##   <-- ADEX: [STUDYID, USUBJID]
    ##   <-- ADLB: [STUDYID, USUBJID]
    ##   <-- ADMH: [STUDYID, USUBJID]
    ##   <-- ADQS: [STUDYID, USUBJID]
    ##   <-- ADRS: [STUDYID, USUBJID]
    ##   <-- ADSAFTTE: [STUDYID, USUBJID]
    ##   <-- ADVS: [STUDYID, USUBJID]
    ##   <-- ADDV: [STUDYID, USUBJID]
    ##   <-- ADSUB: [STUDYID, USUBJID]
    ##   <-- ADHY: [STUDYID, USUBJID]
    ##   <-- ADQLQC: [STUDYID, USUBJID]
    ##   <-- ADCSSRS: [STUDYID, USUBJID]
    ##   <-- ADEQ5D5L: [STUDYID, USUBJID]
    ## ADAE: [STUDYID, USUBJID, ASTDTM, AETERM, AESEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADEG: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADTTE: [STUDYID, USUBJID, PARAMCD]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADAETTE: [STUDYID, USUBJID, PARAMCD]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADCM: [STUDYID, USUBJID, ASTDTM, CMSEQ, ATC1CD, ATC2CD, ATC3CD, ATC4CD]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADEX: [STUDYID, USUBJID, PARCAT1, PARAMCD, AVISITN, ASTDTM, EXSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADLB: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADMH: [STUDYID, USUBJID, ASTDTM, MHSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADQS: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADRS: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADSAFTTE: [STUDYID, USUBJID, PARAMCD]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADVS: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADDV: [STUDYID, USUBJID, ASTDT, DVTERM, DVSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADSUB, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADSUB: [STUDYID, USUBJID, PARAMCD, AVISITN, ADTM, SRCSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADHY, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADHY: [STUDYID, USUBJID, PARAMCD, AVISITN, ADTM, SRCSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADQLQC, ADCSSRS, ADEQ5D5L
    ## ADQLQC: [STUDYID, USUBJID, PARCAT1N, PARAMCD, BASETYPE, AVISITN, ATPTN, ADTM, QSSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADCSSRS, ADEQ5D5L
    ## ADCSSRS: [STUDYID, USUBJID, PARAMCD, BASETYPE, AVISITN, DTYPE, ADTM]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADEQ5D5L
    ## ADEQ5D5L: [STUDYID, USUBJID, PARCAT1N, PARAMCD, BASETYPE, AVISITN, ATPTN, ADTM, QSSEQ]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADAE, ADEG, ADTTE, ADAETTE, ADCM, ADEX, ADLB, ADMH, ADQS, ADRS, ADSAFTTE, ADVS, ADDV, ADSUB, ADHY, ADQLQC, ADCSSRS

``` r
default_cdisc_join_keys["ADSL"]
```

    ## A join_keys object containing foreign keys between 1 datasets:
    ## ADSL: [STUDYID, USUBJID]

``` r
default_cdisc_join_keys["ADTTE"]
```

    ## A join_keys object containing foreign keys between 2 datasets:
    ## ADSL: [STUDYID, USUBJID]
    ##   <-- ADTTE: [STUDYID, USUBJID]
    ## ADTTE: [STUDYID, USUBJID, PARAMCD]
    ##   --> ADSL: [STUDYID, USUBJID]

``` r
default_cdisc_join_keys[c("ADSL", "ADTTE", "ADRS")]
```

    ## A join_keys object containing foreign keys between 3 datasets:
    ## ADSL: [STUDYID, USUBJID]
    ##   <-- ADTTE: [STUDYID, USUBJID]
    ##   <-- ADRS: [STUDYID, USUBJID]
    ## ADTTE: [STUDYID, USUBJID, PARAMCD]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADRS
    ## ADRS: [STUDYID, USUBJID, PARAMCD, AVISIT]
    ##   --> ADSL: [STUDYID, USUBJID]
    ##   --* (implicit via parent with): ADTTE
