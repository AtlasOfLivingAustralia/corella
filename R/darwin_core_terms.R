#' Dataset of supported Darwin Core terms
#'
#' The Darwin Core Standard is maintained by Biodiversity Information Standards,
#' previously known as the Taxonomic Databases Working Group and known by the
#' acronym 'TDWG'. This tibble is the full list of supported terms,
#' current as at 2024-12-10.
#' @name darwin_core_terms
#' @format
#' A tibble with 206 rows and 6 columns:
#' \describe{
#'   \item{class}{TDWG group that a term belongs to.}
#'   \item{term}{Column header names that can be used in Darwin Core}
#'   \item{url}{Stable url to information describing the term.}
#'   \item{definition}{Human-readable definition of the term.}
#'   \item{comments}{Further information from TDWG.}
#'   \item{examples}{Examples of how the field should be populated.}
#' }
#' @seealso [occurrence_terms()] and [event_terms()] to get terms for use in
#' [dplyr::select()]
#' @source Slightly modified version of a table supplied by TDWG at
#' [this link](https://github.com/tdwg/dwc/blob/master/vocabulary/term_versions.csv).
"darwin_core_terms"
