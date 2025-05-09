% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{basisOfRecord_values}
\alias{basisOfRecord_values}
\alias{countryCode_values}
\title{Accepted value functions}
\usage{
basisOfRecord_values()

countryCode_values()
}
\value{
A vector of accepted values for that use case.
}
\description{
When creating a Darwin Core Archive, several fields have a vocabulary of
acceptable values. These functions provide a vector of terms that can be used
to fill or validate those fields.
}
\examples{
# See all valid basis of record values
basisOfRecord_values()

}
\seealso{
\code{\link[=occurrence_terms]{occurrence_terms()}} or \code{\link[=event_terms]{event_terms()}} for valid Darwin Core
\emph{terms} (i.e. column names).
}
