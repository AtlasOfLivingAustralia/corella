#' Set, create or modify columns with information of biological relationships between organisms
#'
#' @description
#' Biological organisms can have interactions, such as parasitism, predation or
#' symbiotic relationships.
#' Here, we can record the subject (`resourceID`), the nature of the association
#' (`relationshipOfResource`), and the target (`relatedResourceID`).
#'
#' This function helps to format columns
#' with [Resource Relationship](https://dwc.tdwg.org/terms/#resourcerelationship)
#' information to a `tibble` using Darwin Core Standard. Note that
#' `resourceRelationship` a Darwin Core extension that requires a separate
#' dataset (csv) within a Darwin Core Archive.
#'
#' In practice this is used no differently from `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param resourceID An identifier for the resource that is the subject of the relationship.
#' @param relationshipOfResource The relationship of the subject (`resourceID`) to its object (`relatedResourceID`).
#' @param relatedResourceID An identifier for the related resource that is the object of the relationship.
#' @param relationshipAccordingTo The source (person, organization, publication, reference) establishing the relationship between the two resources.
#' @param relationshipEstablishedDate The date-time on which the relationship between the two resources was established.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested columns added/reformatted.
#' @details
#'
#' Examples of `resourceID`:
#' *  `f809b9e0-b09b-11e8-96f8-529269fb1459`
#'
#' Examples of `relationshipOfResource`:
#' *  `pollinated by members of taxon`
#' *  `pollinator of members of taxon`
#' *  `eaten by`
#'
#' Examples of `relatedResourceID`:
#' *  `	dc609808-b09b-11e8-96f8-529269fb1459`
#'
#' Examples of `relationshipEstablishedDate`:
#' *  `Julie Woodruff`
#'
#' Examples of `relationshipEstablishedDate`:
#' *  `1809-02-12`
#'
#'
#' @seealso [set_occurrences()] for adding occurrence information.
#' @examples
#' df <- tibble::tibble(
#'   scientificName = c("Capra", "Lepus"),
#'   latitude = c(32.827191, 32.8225),
#'   longitude = c(37.668196, 37.666389),
#'   eventDate = c("2010-07-20", "2014-03-31"),
#'   resourceID = c("f809b9e0-b09b-11e8-96f8-529269fb1459", "f809b9e0-b09b-11e8-96f8-529269fb1234"),
#'   relationship = c("pollinator of members of taxon"),
#'   related = c("dc609808-b09b-11e8-96f8-529269fb1459", "	dc609808-b09b-11e8-96f8-529269fb7404"),
#'   country = c("Australia", "Australia"),
#'   locality = c("Mound East", "Mound West Trench 1")
#' )
#'
#' # Reformat columns to Darwin Core Standard terms
#' df |>
#'   set_relationships(
#'     resourceID = resourceID
#'     relationshipOfResource = relationship,
#'     relatedResourceID = related
#'   )
#'
#' # Columns with valid Darwin Core terms as names are automatically detected
#' # and checked. This will do the same as above.
#' df |>
#'   set_geological_context(
#'     relationshipOfResource = relationship,
#'     relatedResourceID = related
#'   )
#'
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_null
#' @importFrom rlang zap
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @export
set_relationships <- function(.df,
                              resourceID = NULL,
                              relationshipOfResource = NULL,
                              relatedResourceID = NULL,
                              relationshipAccordingTo = NULL,
                              relationshipEstablishedDate = NULL,
                              .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(relatedResourceID, relationshipAccordingTo,
                    relationshipEstablishedDate, relationshipOfResource,
                    resourceID)
  names(fn_quos) <- fn_args

  # find arguments that are NULL but exist already in `df`
  # then remove their names before `mutate()`
  # otherwise, these DwC columns are deleted by `mutate(.keep = "unused")`
  fn_quo_is_null <- fn_quos |>
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |>
    unlist()

  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))

  if(any(null_col_exists_in_df)){
    fn_quos <- fn_quos |>
      purrr::keep(!names(fn_quos) %in% names(which(null_col_exists_in_df)))
  }

  # Update df
  result <- .df |>
    mutate(!!!fn_quos,
           .keep = .keep)

  check_missing_all_args(fn_call = match.call(),
                         fn_args = fn_args,
                         user_cols = colnames(result))

  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  col_progress_bar(cols = matched_cols)

  # run column checks
  check_resourceID(result, level = "abort")
  check_relationshipOfResource(result, level = "abort")
  check_relatedResourceID(result, level = "abort")
  check_relationshipAccordingTo(result, level = "abort")
  check_relationshipEstablishedDate(result, level = "abort")

  result
}


#' Check resourceID
#' @noRd
#' @keywords Internal
check_resourceID <- function(.df,
                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "resourceID")){
    .df |>
      select("resourceID") |>
      check_is_unique(level = level)
  }
  .df
}


#' Check relationshipOfResource
#' @noRd
#' @keywords Internal
check_relationshipOfResource <- function(.df,
                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "relationshipOfResource")){
    .df |>
      select("relationshipOfResource") |>
      check_is_string(level = level) |>
      check_contains_values(values = relationshipOfResource_values(),
                            level = level)
  }
  .df
}

#' Check relatedResourceID
#' @noRd
#' @keywords Internal
check_relatedResourceID <- function(.df,
                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "relatedResourceID")){
    .df |>
      select("relatedResourceID") |>
      check_is_unique(level = level)
      # check value is not equal to resourceID?
  }
  .df
}

#' Check relationshipAccordingTo
#' @noRd
#' @keywords Internal
check_relationshipAccordingTo <- function(.df,
                                    level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "relationshipAccordingTo")){
    .df |>
      select("relationshipAccordingTo") |>
      check_is_string(level = level)
    # check value is not equal to resourceID?
  }
  .df
}

#' Check relationshipEstablishedDate
#' @noRd
#' @keywords Internal
check_relationshipEstablishedDate <- function(.df,
                                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "relationshipAccordingTo")){
    .df |>
      select("relationshipAccordingTo") |>
      check_is_date(level = level) # this might need updating to support time?
  }
  .df
}
