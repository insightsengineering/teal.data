# Data input for `teal` app

Function is a wrapper around
[`teal_data()`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)
and guesses `join_keys` for given datasets whose names match ADAM
datasets names.

## Usage

``` r
cdisc_data(
  ...,
  join_keys = teal.data::default_cdisc_join_keys[names(rlang::list2(...))],
  code = character(0)
)
```

## Arguments

- ...:

  any number of objects (presumably data objects) provided as
  `name = value` pairs.

- join_keys:

  (`join_keys` or single `join_key_set`) optional object with datasets
  column names used for joining. If empty then it would be automatically
  derived basing on intersection of datasets primary keys. For ADAM
  datasets it would be automatically derived.

- code:

  (`character`, `language`) optional code to reproduce the datasets
  provided in `...`. Note this code is not executed and the `teal_data`
  may not be reproducible

  Use
  [`verify()`](https://insightsengineering.github.io/teal.data/reference/verify.md)
  to verify code reproducibility.

## Value

A `teal_data` object.

## Details

This function checks if there were keys added to all data sets.

## Examples

``` r
data <- cdisc_data(
  join_keys = join_keys(
    join_key("ADSL", "ADTTE", c("STUDYID" = "STUDYID", "USUBJID" = "USUBJID"))
  )
)

data <- within(data, {
  ADSL <- example_cdisc_data("ADSL")
  ADTTE <- example_cdisc_data("ADTTE")
})
```
