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
#'   * [use_measurements()] for 'Measurement or Fact' data (optional rn)
#'
#'   Proposed:
#'   * [use_darwin_core()] to subset to only fields with DwC names (i.e. same as `df |> select(any_of(dwc_fields()))`)
#'   * [use_media()] good idea, but unclear how users would supply said media; should be urls, but to where?
#'
#'  **Checking data for Darwin Core compliance**
#'
#'  The wrapper function for checking tibbles for Darwin Core compliance is
#'  [check_dataset()]. It calls all internal check functions for checking data in columns with matching Darwin Core terms.
#'
#'  **Helper functions**
#'  These functions are called within `use_` (or `mutate()` functions), and assist in common problems
#'
#'   * [create_composite_id()] Supply a combination of variables to concatenate into a unique identifier
#'   * [create_sequential_id()] Create a unique identifier of sequential numbers
#'   * [create_random_id()] Create a unique identifier using `UUID()`
#'
#' @keywords internal
"_PACKAGE"
