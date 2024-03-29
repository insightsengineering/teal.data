% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_keys-parents.R
\name{parents}
\alias{parents}
\alias{parents.join_keys}
\alias{parents.teal_data}
\alias{parents<-}
\alias{parents<-.join_keys}
\alias{parents<-.teal_data}
\alias{parent}
\title{Get and set parents in \code{join_keys} object}
\usage{
parents(x)

\method{parents}{join_keys}(x)

\method{parents}{teal_data}(x)

parents(x) <- value

\method{parents}{join_keys}(x) <- value

\method{parents}{teal_data}(x) <- value

parent(x, dataset_name)
}
\arguments{
\item{x}{(\code{join_keys} or \code{teal_data}) object that contains "parents" information
to retrieve or manipulate.}

\item{value}{(\verb{named list}) of \code{character} vectors.}

\item{dataset_name}{(\code{character(1)}) Name of dataset to query on their parent.}
}
\value{
a \code{list} of \code{character} representing the parents.

For \code{parent(x, dataset_name)} returns \code{NULL} if parent does not exist.
}
\description{
\code{parents()} facilitates the creation of dependencies between datasets by
assigning a parent-child relationship.
}
\details{
Each element is defined by a \code{list} element, where \code{list("child" = "parent")}.
}
\section{Methods (by class)}{
\itemize{
\item \code{parents(join_keys)}: Retrieves parents of \code{join_keys} object.

\item \code{parents(teal_data)}: Retrieves parents of \code{join_keys} inside \code{teal_data} object.

}}
\section{Functions}{
\itemize{
\item \code{parents(x) <- value}: Assignment of parents in \code{join_keys} object.

\item \code{parents(join_keys) <- value}: Assignment of parents of \code{join_keys} object.

\item \code{parents(teal_data) <- value}: Assignment of parents of \code{join_keys} inside \code{teal_data} object.

\item \code{parent()}: Getter for individual parent.

}}
\examples{
# Get parents of join_keys ---

jk <- default_cdisc_join_keys["ADEX"]
parents(jk)
# Get parents of join_keys inside teal_data object ---

td <- teal_data(
  ADSL = rADSL,
  ADTTE = rADTTE,
  ADRS = rADRS,
  join_keys = default_cdisc_join_keys[c("ADSL", "ADTTE", "ADRS")]
)
parents(td)
# Assignment of parents ---

jk <- join_keys(
  join_key("ds1", "ds2", "id"),
  join_key("ds5", "ds6", "id"),
  join_key("ds7", "ds6", "id")
)

parents(jk) <- list(ds2 = "ds1")

# Setting individual parent-child relationship

parents(jk)["ds6"] <- "ds5"
parents(jk)["ds7"] <- "ds6"
# Assignment of parents of join_keys inside teal_data object ---

parents(td) <- list("ADTTE" = "ADSL") # replace existing
parents(td)["ADRS"] <- "ADSL" # add new parent
# Get individual parent ---

parent(jk, "ds2")
parent(td, "ADTTE")
}
\seealso{
\code{\link[=join_keys]{join_keys()}}
}
