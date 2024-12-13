% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corella-package.R
\docType{package}
\name{corella-package}
\alias{corella}
\alias{corella-package}
\title{Build shareable biodiversity datasets}
\description{
\code{{corella}} is for data preparation, editing and checking of data to
follow 'Darwin Core standards'; a global data standard to store, document,
and share biodiversity information. The package provides tools to manipulate
data to conform with, and check validity against, Darwin Core standards.
Using \code{{corella}} will allow users to verify that their data can be used to
build 'Darwin Core Archives' using the \code{{galaxias}} package.

The package is named for a genus of Australian birds. The logo image is of
the Little Corella (\emph{Cacatua sanguinea}), and was drawn by Dax Kellie.
}
\section{Functions}{


\strong{Suggest where to start}
\itemize{
\item \code{\link[=suggest_workflow]{suggest_workflow()}} for a summary of what column names match Darwin Core terms in your data, and a workflow for adding or editing more.
}

\strong{Add Darwin Core Terms}

The following functions add single DwC fields, or collections of related
fields, to an existing \code{tibble}.
\itemize{
\item \code{\link[=use_events]{use_events()}} basic information on observation events (\code{eventID}, \code{parentEventID}, \code{eventType})
\item \code{\link[=use_occurrences]{use_occurrences()}} basic information on observations (\code{occurrenceID}, \code{basisOfRecord})
\item \code{\link[=use_scientific_name]{use_scientific_name()}} record the highest level of taxonomic specificity in the dataset (\code{scientificName}, \code{scientificNameAuthorship}, \code{taxonRank})
\item \code{\link[=use_taxonomy]{use_taxonomy()}} to specify higher taxonomic columns (\code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family}, \code{genus}, \code{species}, \code{specificEpithet}, \code{vernacularName})
\item \code{\link[=use_coordinates]{use_coordinates()}} for spatial data (\code{decimalLatitude}, \code{decimalLongitude}, \code{geodeticDatum}, \code{coordinateUncertaintyInMeters}, \code{coordinatePrecision})
\item \code{\link[=use_sf]{use_sf()}} for spatial data in \code{sf} format
\item \code{\link[=use_locality]{use_locality()}} for spatial descriptions (\code{continent}, \code{country}, \code{countryCode}, \code{stateProvince}, \code{locality})
\item \code{\link[=use_datetime]{use_datetime()}} for temporal data (\code{eventDate}, \code{year}, \code{month}, \code{day}, \code{eventTime})
\item \code{\link[=use_collection]{use_collection()}} to give museum- or collection- specific information (\code{datasetID}, \code{datasetName}, \code{catalogNumber})
\item \code{\link[=use_observer]{use_observer()}} to specify who made the observation (\code{recordedByID}, \code{recordedBy})
\item \code{\link[=use_abundance]{use_abundance()}} to state how many animals were seen during the observation (\code{individualCount}, \code{organismQuantity}, \code{organismQuantityType})
\item \code{\link[=use_individual_traits]{use_individual_traits()}} attributes of individuals measured (\code{individualID}, \code{lifeStage}, \code{sex}, \code{vitality}, \code{reproductiveCondition})
\item \code{\link[=use_measurements]{use_measurements()}} for 'Measurement or Fact' data (optional)
}

\strong{Checking data for Darwin Core compliance}

The wrapper function for checking tibbles for Darwin Core compliance is
\code{\link[=check_dataset]{check_dataset()}}. It calls all internal check functions for checking data
in columns with matching Darwin Core terms.

\strong{Helper functions}

These functions are called within \code{use_} (or \code{mutate()} functions), and
assist in common problems.
\itemize{
\item \code{\link[=composite_id]{composite_id()}} Supply a combination of variables to concatenate into a unique identifier, optionally incorporating \code{\link[=sequential_id]{sequential_id()}} or \code{\link[=random_id]{random_id()}}.
\item \code{\link[=occurrence_terms]{occurrence_terms()}} and \code{\link[=event_terms]{event_terms()}} return a vector of accepted terms for different use cases.
\item \code{\link[=basisOfRecord_values]{basisOfRecord_values()}} and \code{\link[=countryCode_values]{countryCode_values()}} return a vector of accepted values for \code{basisOfRecord} and \code{countryCode} (respectively)
}

\strong{Data}

Datasets to support usage of Darwin Core.
\itemize{
\item \link{darwin_core_terms} A tibble of accepted terms, including URIs and usage notes
\item \link{country_codes} A tibble of codes accepted by \code{countryCode}
}
}

\references{
If you have any questions, comments or suggestions, please email
\href{mailto:support@ala.org.au}{support@ala.org.au}.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://corella.ala.org.au}
}

}
\author{
\strong{Maintainer}: Dax Kellie \email{dax.kellie@csiro.au}

Authors:
\itemize{
  \item Shandiya Balasubramanium \email{shandiya.balasubramanium@csiro.au}
  \item Martin Westgate \email{martin.westgate@csiro.au}
}

}
\keyword{internal}
