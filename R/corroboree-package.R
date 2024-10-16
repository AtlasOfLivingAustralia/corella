#' Build repositories to share biodiversity data
#'
#' @description
#' `{corella}` is for data preparation, editing and checking of data to
#' follow 'Darwin Core standards'; a
#' global data standard to store, document, and share biodiversity information.
#' The package provides tools to manipulate data to
#' conform with, and check validity against, Darwin Core standards.
#' Using `{corella}` will allow users to verify that their data can be used to
#' build 'Darwin Core Archives' using the `{galaxias}` package
#'
#' The package is named for an endangered species of alpine frog.
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
#'   * [use_coordinates()] for spatial data
#'   * [use_sf()] for spatial data in `sf` format
#'   * [use_datetime()] for temporal data
#'   * [use_locality()] for spatial descriptions
#'   * [use_occurrences()] basic information on observations (occurrenceID, basisOfrecord, occurrenceStatus, recordID (?))
#'   * [use_scientificName()] record the highest level of taxonomic specificity in the dataset (scientificName, scientificNameRank, scientificNameAuthorship)
#'   * [use_taxonomy()] to specify higher taxonomic columns (kingdom, phylum, class, order, family, genus, species, specificEpithet, vernacularName)
#'   * [use_abundance()] to state how many animals were seen during the observation (individualCount, organismQuantity, organismQuantityType)
#'   * [use_individual_traits()] attributes of individuals measured (individualID, lifeStage, sex, vitality, reproductiveCondition)
#'   * [use_observer()] to specify who made the observation (recordedByID, recordedBy)
#'   * [use_collection()] to give museum- or collection- specific information (datasetID, datasetName, catalogNumber)
#'   * [use_events()] basic information on observation events (eventID, parentEventID, eventType)
#'
#'   Proposed:
#'   * [use_darwin_core()] to subset to only fields with DwC names (i.e. same as `df |> select(any_of(dwc_fields()))`)
#'   * [use_measurement()] for 'Measurement or Fact' data (optional rn)
#'   * [use_media()] good idea, but unclear how users would supply said media; should be urls, but to where?
#'
#'  **Checking data for Darwin Core compliance**
#'
#'  The wrapper function for checking tibbles for Darwin Core compliance is
#'  [check_occurrences()]. It calls the following microfunctions:
#'
#'   * [check_fields()] Checks whether non-DwC fields are present
#'   * [check_occurrenceID()]
#'   * [check_basisOfRecord()]
#'   * [check_continent()]
#'   * [check_country()]
#'   * [check_countryCode()]
#'   * [check_decimalLatitude()]
#'   * [check_decimalLongitude()]
#'   * ADD MORE
#'
#'  Note that there are more `check_` functions than `use_` functions, because
#'  some `use_` functions affect multiple fields.
#'
#' @keywords internal
"_PACKAGE"
