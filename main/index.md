# teal.data

[![CRAN
Version](https://www.r-pkg.org/badges/version/teal.data?color=green)](https://cran.r-project.org/package=teal.data)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/teal.data?color=green)](https://cran.r-project.org/package=teal.data)
[![Last Month
Downloads](http://cranlogs.r-pkg.org/badges/last-month/teal.data?color=green)](https://cran.r-project.org/package=teal.data)
[![Last Week
Downloads](http://cranlogs.r-pkg.org/badges/last-week/teal.data?color=green)](https://cran.r-project.org/package=teal.data)

[![Check
🛠](https://github.com/insightsengineering/teal.data/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.data/main/unit-test-report/)
[![Docs
📚](https://github.com/insightsengineering/teal.data/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.data/)
[![Code Coverage
📔](https://raw.githubusercontent.com/insightsengineering/teal.data/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.data/main/coverage-report/)

![GitHub
forks](https://img.shields.io/github/forks/insightsengineering/teal.data?style=social)![GitHub
repo
stars](https://img.shields.io/github/stars/insightsengineering/teal.data?style=social)

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.data)![GitHub
contributors](https://img.shields.io/github/contributors/insightsengineering/teal.data)![GitHub
last
commit](https://img.shields.io/github/last-commit/insightsengineering/teal.data)![GitHub
pull
requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.data)![GitHub
repo
size](https://img.shields.io/github/repo-size/insightsengineering/teal.data)![GitHub
language
count](https://img.shields.io/github/languages/count/insightsengineering/teal.data)[![Project
Status: Active – The project has reached a stable, usable state and is
being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.data/main?color=purple&label=package%20version)](https://github.com/insightsengineering/teal.data/tree/main)
[![Open
Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.data?color=red&label=open%20issues)](https://github.com/insightsengineering/teal.data/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)

This package is used to create the data needed for `teal` applications.
This data can be:

- Independent data frames
- CDISC data (for clinical trial reporting)
- Relational data
- `MultiAssayExperiment` objects

This package provides:

- the mechanism for pulling data from existing systems
- the ability to mutate (i.e. pre-process) the data
- record the operations used to create the data to enable
  reproducibility

## Installation

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``'teal.data'``)`

Alternatively, you might want to use the development version.

\
`# install.packages("pak")`\
`pak``::`[`pak`](https://pak.r-lib.org/reference/pak.html)`(``"insightsengineering/teal.data"``)`

## Usage

To understand how to use this package, please refer to the [Introduction
to
`teal.data`](https://insightsengineering.github.io/teal.data/latest-tag/articles/teal-data.html)
article, which provides multiple examples of code implementation.

Below is the showcase of the example usage.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.data`](https://insightsengineering.github.io/teal.data/)`)`

\
`# quick start for clinical trial data`\
`my_data`` ``<-`` `[`cdisc_data`](https://insightsengineering.github.io/teal.data/reference/cdisc_data.md)`(`\
`  ADSL ``=`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADSL"``)``,`\
`  ADTTE ``=`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADTTE"``)``,`\
`  code ``=`` `[`quote`](https://rdrr.io/r/base/substitute.html)`(``{`\
`    ``ADSL`` ``<-`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADSL"``)`\
`    ``ADTTE`` ``<-`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADTTE"``)`\
`  ``}``)`\
`)`\
\
`# or`\
\
`my_data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)`(``)``, ``{`\
`  ``ADSL`` ``<-`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADSL"``)`\
`  ``ADTTE`` ``<-`` `[`example_cdisc_data`](https://insightsengineering.github.io/teal.data/reference/example_cdisc_data.md)`(``"ADTTE"``)`\
`}``)`\
`datanames`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADTTE"``)`\
[`datanames`](https://insightsengineering.github.io/teal.data/reference/datanames.md)`(``my_data``)`` ``<-`` ``datanames`\
[`join_keys`](https://insightsengineering.github.io/teal.data/reference/join_keys.md)`(``my_data``)`` ``<-`` ``default_cdisc_join_keys``[``datanames``]`

\
`# quick start for general data`\
`my_general_data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)`(``)``, ``{`\
`  ``iris`` ``<-`` ``iris`\
`  ``mtcars`` ``<-`` ``mtcars`\
`}``)`

\
`# reproducibility check`\
`data`` ``<-`` `[`teal_data`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)`(``iris ``=`` ``iris``, code ``=`` ``"iris <- mtcars"``)`\
[`verify`](https://insightsengineering.github.io/teal.data/reference/verify.md)`(``data``)`\
`#> Error: Code verification failed.`\
`#>  Object(s) recreated with code that have different structure in data:`\
`#>  • iris`

\
`# code extraction`\
`iris2_data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(`[`teal_data`](https://insightsengineering.github.io/teal.data/reference/teal_data.md)`(``)``, ``{``iris2`` ``<-`` ``iris``[``1``:``6``, ``]``}``)`\
[`get_code`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_code.html)`(``iris2_data``)`\
`#> "iris2 <- iris[1:6, ]"`

## Getting help

If you encounter a bug or have a feature request, please file an issue.
For questions, discussions, and staying up to date, please use the
`teal` channel in the [`pharmaverse` slack
workspace](https://pharmaverse.slack.com).
