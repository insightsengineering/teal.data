% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teal_data-constructor.R, R/teal_data-extract.R
\name{teal_data}
\alias{teal_data}
\alias{[.teal_data}
\title{Comprehensive data integration function for \code{teal} applications}
\usage{
teal_data(..., join_keys = teal.data::join_keys(), code = character(0))

\method{[}{teal_data}(x, names)
}
\arguments{
\item{...}{any number of objects (presumably data objects) provided as \code{name = value} pairs.}

\item{join_keys}{(\code{join_keys} or single \code{join_key_set})
optional object with datasets column names used for joining.
If empty then no joins between pairs of objects.}

\item{code}{(\code{character}, \code{language}) optional code to reproduce the datasets provided in \code{...}.
Note this code is not executed and the \code{teal_data} may not be reproducible

Use \code{\link[=verify]{verify()}} to verify code reproducibility .}

\item{x}{(\code{teal_data})}

\item{names}{(\code{character}) names of objects included in \code{teal_subset} to subset}
}
\value{
A \code{teal_data} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Universal function to pass data to teal application.
}
\section{Subsetting}{

\code{x[names]} subsets objects in \code{teal_data} environment and limit the code to the necessary needed to build limited
objects.
}

\examples{
teal_data(x1 = iris, x2 = mtcars)


# Subsetting
data <- teal_data()
data <- eval_code(data, "a <- 1;b<-2")
data["a"]
data[c("a", "b")]

join_keys(data) <- join_keys(join_key("a", "b", "x"))
join_keys(data["a"]) # should show empty keys
join_keys(data["b"])
join_keys(data)["a"] # should show empty keys
join_keys(data)["b"]

}
