% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_occurrences.R
\name{set_occurrences}
\alias{set_occurrences}
\title{Set, create or modify columns with occurrence-specific information using Darwin Core}
\usage{
set_occurrences(
  .df,
  occurrenceID = NULL,
  basisOfRecord = NULL,
  occurrenceStatus = NULL,
  .keep = "unused",
  .keep_composite = "all",
  .messages = TRUE
)
}
\arguments{
\item{.df}{a \code{data.frame} or \code{tibble} that the column should be appended to.}

\item{occurrenceID}{A character string. Every occurrence should have an
\code{occurrenceID} entry. Ideally IDs should be persistent to avoid being lost
in future updates. They should also be unique, both within the dataset, and
(ideally) across all other datasets.}

\item{basisOfRecord}{Record type. Only accepts \code{camelCase}, for
consistency with field names.
Accepted \code{basisOfRecord} values are one of:
\itemize{
\item \code{"humanObservation"}, \code{"machineObservation"}, \code{"livingSpecimen"},
\code{"preservedSpecimen"}, \code{"fossilSpecimen"}, \code{"materialCitation"}
}}

\item{occurrenceStatus}{Either \code{"present"} or \code{"absent"}.}

\item{.keep}{Control which columns from \code{.df} are retained in the output.
Note that unlike \code{\link[dplyr:mutate]{dplyr::mutate()}}, which defaults to \code{"all"} this defaults
to \code{"unused"}; i.e. only keeps Darwin Core fields, and not those fields used
to generate them.}

\item{.keep_composite}{Control which columns from \code{.df} are kept when
\code{\link[=composite_id]{composite_id()}} is used to assign values to \code{occurrenceID}, defaulting to
\code{"all"}. This has a different default from \code{.keep} because composite
identifiers often contain information that is valuable in other contexts,
meaning that deleting these columns by default is typically unwise.}

\item{.messages}{Logical: Should progress bar be shown? Defaults to \code{TRUE}.}
}
\value{
A tibble with the requested fields added.
}
\description{
Format fields uniquely identify each occurrence record and specify the type
of record. \code{occurrenceID} and \code{basisOfRecord} are necessary fields of
information for occurrence records, and should be appended to a data set
to conform to Darwin Core Standard prior to submission.

In practice this is no different from using \code{mutate()}, but gives some
informative errors, and serves as a useful lookup for fields in
the Darwin Core Standard.
}
\details{
Examples of \code{occurrenceID} values:
\itemize{
\item \verb{000866d2-c177-4648-a200-ead4007051b9}
\item \verb{http://arctos.database.museum/guid/MSB:Mamm:233627}
}

Accepted \code{basisOfRecord} values are one of:
\itemize{
\item \code{"humanObservation"}, \code{"machineObservation"}, \code{"livingSpecimen"},
\code{"preservedSpecimen"}, \code{"fossilSpecimen"}, \code{"materialCitation"}
}
}
\seealso{
\code{\link[=basisOfRecord_values]{basisOfRecord_values()}} for accepted values for the \code{basisOfRecord}
field'; \code{\link[=set_abundance]{set_abundance()}} for occurrence-level counts.
}
