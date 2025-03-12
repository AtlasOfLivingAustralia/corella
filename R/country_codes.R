#' Dataset of supported Country Codes
#'
#' @description
#' A `tibble` of ISO 3166-1 alpha-2 codes for countries, which are
#' the accepted standard for supplying `countryCode` in Darwin Core Standard.
#' @name country_codes
#' @format
#' A `tibble` containing valid country codes (249 rows x 3 columns).
#' Column descriptions are as follows:
#' \describe{
#'   \item{name}{ISO 3166-1 alpha-2 code, pointing to its ISO 3166-2 article.}
#'   \item{code}{English short name officially used by the ISO 3166 Maintenance Agency (ISO 3166/MA).}
#'   \item{year}{Year when alpha-2 code was first officially assigned.}
#' }
#' @seealso [use_locality()] for assigning `countryCode` within a tibble;
#' [countryCode_values()] to return valid codes as a vector.
#' @source
#' [Wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2).
"country_codes"
