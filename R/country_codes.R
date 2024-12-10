#' Dataset of supported Country Codes
#'
#' This tibble gives the ISO 3166-1 alpha-2 codes for countries, which are
#' the standard for supplying the Darwin Core term `countryCode`.
#' @name country_codes
#' @format
#' A tibble with 249 rows and 3 columns:
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
