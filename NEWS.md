# teal.data 0.6.0.9011

### Enhancements

- `datanames()`
    - if `join_keys` are provided, the `datanames()` are now sorted in topological way (`Kahn` algorithm),
    which means the parent dataset always precedes the child dataset.
    - are extended by the parent dataset name, if one of the child dataset exist in `datanames()` and
    the connection between child-parent is set through `join_keys` and `parent` exist in `teal_data` environment.
    - do not allow to set a dataset name that do not exist in `teal_data` environment.
    - `teal_data` no longer set default `datanames()` based on `join_keys` names - it uses only data names.
- Added `is_verified` method to return `TRUE` when objects stored in `teal_data` can be reproduced.

### Miscellaneous

- `get_code` no longer adds `warning` message about failed verification.

# teal.data 0.6.0

### Enhancements

* `col_relabel` supports `NA` to remove labels (similar to the `col_labels<-`).

### Bug fixes

* Fixed bug in `get_code` causing incorrect lines order of the returned code.
* Fixed bug in `col_labels` causing incorrect label names to be returned when input data contains named label attributes.

# teal.data 0.5.0

### Bug fixes

* Fix the `get_code_dependency` bug to detect the usage of objects in functions on both the left and right-hand sides for code reproducibility.
* Remove duplicate entries in the `code_graph`.

### Enhancements

* Extended `get_code.teal_data()` with a possibility to steer internal methods with `...` parameter.

# teal.data 0.4.0

### Enhancements

* Simplified `join_key` to better support primary keys.
* `JoinKey` `R6` object was removed in favor of a list-like object with class name `join_keys`. Subset operators and assignments are supported (`[`, `[[`, `[<-` and `[[<-`)
* `join_keys` function works as a constructor, getter and setter.

### Breaking changes

* `teal_data()` and `cdisc_data()` return now `teal_data` class object which replaces `TealData` class object. `teal_data` becomes a standard input for the entire `teal` framework.
* `TealDataset`, `TealDatasetConnector` or `TealDataConnector` classes have been removed. Delayed-data-loading is no longer supported by `teal.data`. So called connectors are now supported by `teal` package (see `?teal::teal_data_module`).
* `join_keys()` and `join_key()` return now `join_keys` object which replace `JoinKeys` class.

### Miscellaneous

* Specified minimal version of package dependencies.
* Upgraded `teal.code` dependency from `Imports` to `Depends`.
* Deprecated `get_labels` function and removed the supporting function `data_label`.

# teal.data 0.3.0

### Enhancements
* Removed `scda` package dependency from examples.
* Added `col_labels` function and removed `formatters` dependency.

### Miscellaneous
* Update installation instructions.

# teal.data 0.2.0

### Enhancements
* Added `ADQLQC`, `ADCSSRS`, and `ADEQ5D5L` as supported data sets.
* Improved error message in `get_cdisc_keys`.
* Examples now use `scda.2022` instead of `scda.2021`.
* Fixed help files for `TealDataset` and `MAETealDataset`.
* Added backstop for missing `reticulate` package in _teal.data with Python_ vignette.

### Miscellaneous
* Modified `teal.Dataset$print` method for a less cluttered output.
* Transferred data hashing step in `TealDataset` and `MAETealDataset` to `teal`.
* Removed `CDISCTealData` class and updated `TealData` to account for the removed functionality.
* Added datasets parents information to `JoinKeys` class.
* Updated `cdisc_data` and `teal_data` wrappers to handle `join_keys` creation and updating instead of `CDISCTealData` and `TealData`.
* Removed `join_keys` methods from `TealDataset`, `TealDatasetConnector`.

# teal.data 0.1.2

### Enhancements
* Updated the vignettes and the `README` content.

### Miscellaneous
* Exported `validate_metadata` function.
* Replaced argument `name` by `archive_name` to comply with the latest version of the `synthetic_cdisc_dataset` function.
* Replaced use of `scda` with `random.cdisc.data`

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
