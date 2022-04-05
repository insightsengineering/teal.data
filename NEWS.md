# teal.data 0.0.0.9014.5

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
