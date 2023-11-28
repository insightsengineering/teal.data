# teal.data

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.data/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.data/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.data/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.data/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.data/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.data/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.data?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.data?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.data)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.data)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.data)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.data)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.data)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.data)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.data/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.data/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.data?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.data/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

This package is used to create the data needed for `teal` applications. This data can be:

- Independent data frames
- `CDISC` data (for clinical trial reporting)
- Relational data
- `MultiAssayExperiment` objects

This package provides:

- the mechanism for pulling data from existing systems
- the ability to mutate (i.e. pre-process) the data
- record the operations used to create the data to enable reproducibility

## Installation

```r
# stable versions
install.packages('teal.data', repos = c('https://insightsengineering.r-universe.dev', getOption('repos')))

# install.packages("pak")
pak::pak("insightsengineering/teal.data@*release")
```

Alternatively, you might want to use the development version available on [r-universe](https://r-universe.dev/).

```r
# beta versions
install.packages('teal.data', repos = c('https://pharmaverse.r-universe.dev', getOption('repos')))

# install.packages("pak")
pak::pak("insightsengineering/teal.data")
```

## Usage

To understand how to use this package, please refer to the [Introduction to teal.data](https://insightsengineering.github.io/teal.data/latest-tag/articles/teal-data.html) article, which provides multiple examples of code implementation.

Below is the showcase of the example usage

```r
library(teal.data)
```

```r
# quick start for clinical trial data
adsl <- teal.data::example_cdisc_data("ADSL")
adtte <- teal.data::example_cdisc_data("ADTTE")

my_data <- cdisc_data(
  cdisc_dataset("ADSL", adsl),
  cdisc_dataset("ADTTE", adtte)
)
```

```r
# quick start for general data
my_general_data <- teal_data(
  dataset("iris", iris),
  dataset("mtcars", mtcars)
)
```

```r
# reproducibility check
data <- teal_data(dataset("iris", iris, code = "iris <- mtcars"), check = TRUE)
#> Error in x$check_reproducibility() : Reproducibility check failed.

```

```r
# code extraction
iris2 <- iris[1:6, ]
iris2_data <- teal_data(dataset("iris2", iris2, code = "iris2 <- iris[1:6, ]"))
iris2_data$get_code()
#> "iris2 <- iris[1:6, ]"
```

## Getting help

If you encounter a bug or you have a feature request - please file an issue. For questions, discussions and staying up to date, please use the "teal" channel in the [`pharmaverse` slack workspace](https://pharmaverse.slack.com).

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.data.svg)](https://starchart.cc/insightsengineering/teal.data)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.data](http://reporoster.com/stars/insightsengineering/teal.data)](https://github.com/insightsengineering/teal.data/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.data](http://reporoster.com/forks/insightsengineering/teal.data)](https://github.com/insightsengineering/teal.data/network/members)
