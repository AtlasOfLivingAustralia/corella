#' Build shareable biodiversity datasets
#'
#' @description
#' `{corella}` is for data preparation, editing and checking of data to
#' follow 'Darwin Core standards'; a global data standard to store, document,
#' and share biodiversity information. The package provides tools to manipulate
#' data to conform with, and check validity against, Darwin Core standards.
#' Using `{corella}` will allow users to verify that their data can be used to
#' build 'Darwin Core Archives' using the `{galaxias}` package.
#'
#' The package is named for a genus of Australian birds. The logo image is of
#' the Little Corella (_Cacatua sanguinea_), and was drawn by Dax Kellie.
#'
#' @name corella-package
#' @docType package
#' @references If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section Functions:
#'
#' **Suggest where to start**
#'
#'  * [suggest_workflow()] for a summary of what column names match Darwin Core terms in your data, and a workflow for adding or editing more.
#'
#' **Add Darwin Core Terms**
#'
#'  The following functions add single DwC fields, or collections of related
#'  fields, to an existing `tibble`.
#'
#'   * [set_events()] basic information on observation events (`eventID`, `parentEventID`, `eventType`)
#'   * [set_occurrences()] basic information on observations (`occurrenceID`, `basisOfRecord`)
#'   * [set_scientific_name()] record the highest level of taxonomic specificity in the dataset (`scientificName`, `scientificNameAuthorship`, `taxonRank`)
#'   * [set_taxonomy()] to specify higher taxonomic columns (`kingdom`, `phylum`, `class`, `order`, `family`, `genus`, `species`, `specificEpithet`, `vernacularName`)
#'   * [set_coordinates()] for spatial data (`decimalLatitude`, `decimalLongitude`, `geodeticDatum`, `coordinateUncertaintyInMeters`, `coordinatePrecision`)
#'   * [set_coordinates_sf()] for spatial data in `sf` format
#'   * [set_locality()] for spatial descriptions (`continent`, `country`, `countryCode`, `stateProvince`, `locality`)
#'   * [set_datetime()] for temporal data (`eventDate`, `year`, `month`, `day`, `eventTime`)
#'   * [set_collection()] to give museum- or collection- specific information (`datasetID`, `datasetName`, `catalogNumber`)
#'   * [set_observer()] to specify who made the observation (`recordedByID`, `recordedBy`)
#'   * [set_abundance()] to state how many animals were seen during the observation (`individualCount`, `organismQuantity`, `organismQuantityType`)
#'   * [set_individual_traits()] attributes of individuals measured (`individualID`, `lifeStage`, `sex`, `vitality`, `reproductiveCondition`)
#'   * [set_measurements()] for 'Measurement or Fact' data (optional)
#'
#'  **Checking data for Darwin Core compliance**
#'
#'  The wrapper function for checking tibbles for Darwin Core compliance is
#'  [check_dataset()]. It calls all internal check functions for checking data
#'  in columns with matching Darwin Core terms.
#'
#'  **Helper functions**
#'
#'  These functions are called within `use_` (or `mutate()` functions), and
#'  assist in common problems.
#'
#'   * [composite_id()] Supply a combination of variables to concatenate into a unique identifier, optionally incorporating [sequential_id()] or [random_id()].
#'   * [occurrence_terms()] and [event_terms()] return a vector of accepted terms for different use cases.
#'   * [basisOfRecord_values()] and [countryCode_values()] return a vector of accepted values for `basisOfRecord` and `countryCode` (respectively)
#'
#'  **Data**
#'
#'  Datasets to support usage of Darwin Core.
#'
#'  * [darwin_core_terms] A tibble of accepted terms, including URIs and usage notes
#'  * [country_codes] A tibble of codes accepted by `countryCode`
#'
#' @keywords internal
"_PACKAGE"
