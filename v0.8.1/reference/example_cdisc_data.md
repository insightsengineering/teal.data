# Generate sample CDISC datasets

Retrieves example CDISC datasets for use in examples and testing.

## Usage

``` r
example_cdisc_data(
  dataname = c("ADSL", "ADAE", "ADLB", "ADCM", "ADEX", "ADRS", "ADTR", "ADTTE", "ADVS")
)
```

## Arguments

- dataname:

  (`character(1)`) name of a CDISC dataset.

## Value

A CDISC dataset as a `data.frame`.

## Details

This function returns a dummy dataset and should only be used within
`teal.data`. Note that the datasets are not created and maintained in
`teal.data`, they are retrieved from
[`random.cdisc.data`](https://insightsengineering.github.io/random.cdisc.data/main/reference/random.cdisc.data-package.html)
package.
