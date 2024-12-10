#' Select support functions
#'
#' When creating a Darwin Core archive, it is often useful to select only those
#' fields that conform to the standard. These functions provide a vector of
#' terms that can be used in combination with [dplyr::select()] and
#' [dplyr::any_of()] to quickly select Darwin Core terms for the relevant
#' data type (events, occurrences, media).
#' @returns A vector of terms that are acceptable (but not mandatory) for that
#' use case.
#' @seealso [basisOfRecord_values()] or [countryCode_values()] for valid entries
#' _within_ a field.
#' @rdname accepted_terms
#' @export
occurrence_terms <- function(){
  darwin_core_terms |>
    dplyr::pull(.data$term)
}

#' @rdname accepted_terms
#' @export
event_terms <- function(){
  darwin_core_terms |>
    dplyr::filter(.data$class %in% c("Generic", "Event", "Location")) |>
    dplyr::pull(.data$term)
}

#' Accepted value functions
#'
#' When creating a Darwin Core Archive, several fields have a vocabulary of
#' acceptable values. These functions provide a vector of terms that can be used
#' to fill or validate those fields.
#' @returns A vector of values that are acceptable (but not mandatory) for that
#' use case.
#' @seealso [occurrence_terms()] or [event_terms()] for valid Darwin Core
#' _terms_ (i.e. column names).
#' @rdname accepted_values
#' @export
basisOfRecord_values <- function(){
  c("humanObservation",
    "machineObservation",
    "livingSpecimen",
    "preservedSpecimen",
    "fossilSpecimen",
    "materialCitation")
}

#' @rdname accepted_values
#' @export
countryCode_values <- function(){
  country_codes |>
    dplyr::pull(.data$code)
}
