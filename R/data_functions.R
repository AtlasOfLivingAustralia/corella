#' Select support functions
#'
#' @description
#' When creating a Darwin Core archive, it is often useful to select only those
#' fields that conform to the standard. These functions provide a vector of
#' terms that can be used in combination with [dplyr::select()] and
#' [dplyr::any_of()] to quickly select Darwin Core terms for the relevant
#' data type (events, occurrences, media).
#' @returns A vector of accepted (but not mandatory) values for that use case.
#' @seealso [basisOfRecord_values()] or [countryCode_values()] for valid entries
#' _within_ a field.
#'
#' @examples
#' # Return a vector of accepted terms in an Occurrence-based dataset
#' occurrence_terms()
#'
#' # Use this vector to filter a data frame
#' df <- tibble::tibble(
#'   name = c("Crinia Signifera", "Crinia Signifera", "Litoria peronii"),
#'   longitude = c(35.27, 35.24, 35.83),
#'   latitude = c(149.33, 149.34, 149.34),
#'   eventDate = c("2010-10-14", "2010-10-14", "2010-10-14"),
#'   measurement1 = c(24.3, 24.9, 20.1), # example measurement column
#'   measurement2 = c(0.92, 1.03, 1.09)  # example measurement column
#'   )
#'
#' df |>
#'   select(any_of(occurrence_terms()))
#'
#' @rdname accepted_terms
#' @export
occurrence_terms <- function(){
  corella::darwin_core_terms |>
    dplyr::pull(.data$term)
}

#' @rdname accepted_terms
#' @export
event_terms <- function(){
  corella::darwin_core_terms |>
    dplyr::filter(.data$class %in% c("Generic", "Event", "Location")) |>
    dplyr::pull(.data$term)
}

#' Accepted value functions
#'
#' @description
#' When creating a Darwin Core Archive, several fields have a vocabulary of
#' acceptable values. These functions provide a vector of terms that can be used
#' to fill or validate those fields.
#' @returns A vector of accepted values for that use case.
#' @seealso [occurrence_terms()] or [event_terms()] for valid Darwin Core
#' _terms_ (i.e. column names).
#' @examples
#' # See all valid basis of record values
#' basisOfRecord_values()
#'
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
