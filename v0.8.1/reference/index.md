# Package index

## Reproducible data class

Functions to create and manage teal_data class.

- [`cdisc_data()`](https://insightsengineering.github.io/teal.data/reference/cdisc_data.md)
  :

  Data input for `teal` app

- [`datanames()`](https://insightsengineering.github.io/teal.data/reference/datanames.md)
  [`` `datanames<-`() ``](https://insightsengineering.github.io/teal.data/reference/datanames.md)
  [`` `names<-`( ``*`<teal_data>`*`)`](https://insightsengineering.github.io/teal.data/reference/datanames.md)
  **\[deprecated\]** :

  Names of data sets in `teal_data` object

- [`get_code(`*`<teal_data>`*`)`](https://insightsengineering.github.io/teal.data/reference/get_code.md)
  :

  Get code from `teal_data` object

- [`join_key()`](https://insightsengineering.github.io/teal.data/reference/join_key.md)
  : Create a relationship between a pair of datasets

- [`join_keys()`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`` `[`( ``*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`` `[<-`( ``*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`c(`*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`c(`*`<join_key_set>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`` `join_keys<-`() ``](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`format(`*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  [`print(`*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)
  :

  Manage relationships between datasets using `join_keys`

- [`names(`*`<teal_data>`*`)`](https://insightsengineering.github.io/teal.data/reference/names.teal_data.md)
  :

  Names of data sets in `teal_data` object

- [`` `names<-`( ``*`<join_keys>`*`)`](https://insightsengineering.github.io/teal.data/reference/names-set-.join_keys.md)
  :

  The names of a `join_keys` object

- [`parents()`](https://insightsengineering.github.io/teal.data/reference/parents.md)
  [`` `parents<-`() ``](https://insightsengineering.github.io/teal.data/reference/parents.md)
  [`parent()`](https://insightsengineering.github.io/teal.data/reference/parents.md)
  :

  Get and set parents in `join_keys` object

- [`show(`*`<teal_data>`*`)`](https://insightsengineering.github.io/teal.data/reference/show-teal_data-method.md)
  :

  Show `teal_data` object

- [`teal_data()`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)
  [`` `[`( ``*`<teal_data>`*`)`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)
  :

  Comprehensive data integration function for `teal` applications

- [`verify()`](https://insightsengineering.github.io/teal.data/reference/verify.md)
  : Verify code reproducibility

## Helpers

Other useful functions for users and developers.

- [`default_cdisc_join_keys`](https://insightsengineering.github.io/teal.data/reference/default_cdisc_join_keys.md)
  :

  List containing default joining keys for `CDISC` datasets

- [`col_labels()`](https://insightsengineering.github.io/teal.data/reference/col_labels.md)
  [`` `col_labels<-`() ``](https://insightsengineering.github.io/teal.data/reference/col_labels.md)
  [`col_relabel()`](https://insightsengineering.github.io/teal.data/reference/col_labels.md)
  : Variable labels

- [`example_cdisc_data()`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)
  : Generate sample CDISC datasets

## Built-in datasets

Random CDISC synthetic data generated from
[random.cdisc.data](https://insightsengineering.github.io/random.cdisc.data/)
package.

- [`rADSL`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADAE`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADCM`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADEX`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADLB`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADRS`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADTR`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADTTE`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  [`rADVS`](https://insightsengineering.github.io/teal.data/reference/random_cdisc_data.md)
  :

  Simulated `CDISC` Data for Examples
