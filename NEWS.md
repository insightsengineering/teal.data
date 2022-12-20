# teal.data 0.1.2.9007

* Added `ADQLQC` data set. 
* Examples now use `scda.2022` instead of `scda.2021`.
* Modified `teal.Dataset$print` method for a less cluttered output.
* Transferred data hashing step in `TealDataset` and `MAETealDataset` to `teal`.
* Fixed help files for `TealDataset` and `MAETealDataset`.

# teal.data 0.1.2

### Enhancements
* Updated the vignettes and the README content.

### Miscellaneous
* Exported `validate_metadata` function.
* Replaced argument `name` by `archive_name` to comply with the latest version of the `synthetic_cdisc_dataset` function.

### Bug fixes
* Fixed `get_raw_data` examples.

# teal.data 0.1.1

### Miscellaneous
* Added a template to the `pkgdown` site.
* Removed the usage of `.Globalenv` in `Python` code execution.
* Updated package authors.
* Added package vignettes.

# teal.data 0.1.0

* Initial release of `teal.data` a package for the data model used by `teal` applications.

## Changes (from behavior when functionality was part of `teal`)

### New features
* Added `metadata` field to `TealDataset` to store a named list of `metadata` items. It is available for module developers through `FilteredData$get_metadata("<<dataname>>")` and can be pulled or added directly to datasets derived from `TealDatasetConnectors`.

### Breaking changes
* `get_key_duplicates` returns a `data.frame` instead of a `tibble`.
* `get_call()` function of `CallableFunction` now returns call with namespace included.
* `MultiAssayExperiment` and `SummarizedExperiment` are now suggested packages, not required. Objects dependent on `MultiAssayExperiment` are changed to lazy-load these now suggested packages.
* Minor changes to the interface of `TealDataset`, for example some active fields should be replaced by explicit "get" calls.

### Miscellaneous
* As `reticulate` is in Suggests, added `requireNamespace` call whenever it is needed.
* `dplyr` moved from imports to suggests.
* Removed redundant calling of the `JoinKeys$mutate` method inside of `for-loops`.
* Removed `rtables` dependency from the package.
