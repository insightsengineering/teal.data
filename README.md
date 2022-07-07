# teal.data

This package is used to create the data needed for `teal` applications. This data can be:

- Independent data frames
- CDISC data (for clinical trial reporting)
- Relational data
- `MultiAssayExperiment` objects

This package provides:

- the mechanism for pulling data from existing systems
- the ability to mutate (i.e. pre-process) the data
- record the operations used to create the data to enable reproducibility

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.data@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.data")` for usage of this package.
