#' Add occurrence-specific information to a `tibble`
#'
#' @description
#' Format fields uniquely identify each occurrence record and specify the type
#' of record. `occurrenceID` and `basisOfRecord` are necessary fields of
#' information for occurrence records, and should be appended to a data set
#' to conform to Darwin Core Standard prior to submission.
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param occurrenceID A character string. Every occurrence should have an
#' `occurrenceID` entry. Ideally IDs should be persistent to avoid being lost
#' in future updates. They should also be unique, both within the dataset, and
#' (ideally) across all other datasets.
#' @param basisOfRecord Record type. Only accepts `camelCase`, for
#' consistency with field names.
#' Accepted `basisOfRecord` values are one of:
#' * `"humanObservation"`, `"machineObservation"`, `"livingSpecimen"`,
#' `"preservedSpecimen"`, `"fossilSpecimen"`, `"materialCitation"`
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike most other `use_` functions in `corella`, this defaults to
#' `"all"` (i.e. same behavior as `dplyr::mutate`). This is because it is common
#' to create composite indicators from other columns (via
#' `create_composite_id()`), and deleting these columns by default is typically
#' unwise.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `occurrenceID` values:
#' * `000866d2-c177-4648-a200-ead4007051b9`
#' * `http://arctos.database.museum/guid/MSB:Mamm:233627`
#'
#' Accepted `basisOfRecord` values are one of:
#' * `"humanObservation"`, `"machineObservation"`, `"livingSpecimen"`,
#' `"preservedSpecimen"`, `"fossilSpecimen"`, `"materialCitation"`
#'
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_occurrences <- function(
    .df,
    occurrenceID = NULL,
    basisOfRecord = NULL,
    occurrenceStatus = NULL,
    # recordNumber = NULL, # keep?
    .keep = "all",
    .messages = TRUE
){
  if(missing(.df)){
    abort(".df is missing, with no default.")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(basisOfRecord, occurrenceID, occurrenceStatus)
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

  if(isTRUE(.messages)) {
    if(length(matched_cols > 0)) {
      col_progress_bar(cols = matched_cols)
    }
  }

  # run column checks
  check_occurrenceID(result, level = "abort")
  check_basisOfRecord(result, level = "abort")
  check_occurrenceStatus(result, level = "abort")

  return(result)
}

#' check basisOfRecord
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_basisOfRecord <- function(.df,
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "basisOfRecord")){
    .df |>
      select("basisOfRecord") |>
      check_is_string(level = level) |>
      check_contains_values(values = valid_basisOfRecord(),
                            level = level)
  }
  .df
}

#' Accepted values for `basisOfRecord`
#' @noRd
#' @keywords Internal
valid_basisOfRecord <- function(){
  c("humanObservation",
    "machineObservation",
    "livingSpecimen",
    "preservedSpecimen",
    "fossilSpecimen",
    "materialCitation")
}

#' check occurrenceID
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
check_occurrenceID <- function(.df,
                               level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "occurrenceID")){
    .df |>
      select("occurrenceID") |>
      check_is_unique(level = level)
  }
}

#' check occurrenceStatus
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_occurrenceStatus <- function(.df,
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "occurrenceStatus")){
    .df |>
      select("occurrenceStatus") |>
      check_is_string(level = level) |>
      check_contains_values(values = c("present", "absent"),
                            level = level)
  }

  .df
}
