#' Add a sampling, collection or image-capture Event to a `tibble`
#'
#' @description
#' Format fields that contain information about an
#' [Event](https://dwc.tdwg.org/list/#dwc_Event). An "Event" in Darwin Core
#' standard refers to an action that occurs at a place and time. Examples
#' include:
#'
#'  * A specimen collecting event
#'  * A survey or sampling event
#'  * A camera trap image capture
#'  * A marine trawl
#'  * A camera trap deployment event
#'  * A camera trap burst image event (with many images for one observation)
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param eventID A unique identifier for an individual Event.
#' @param eventType The type of Event
#' @param parentEventID The parent event under which one or more Events sit
#' within.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Each Event requires a unique `eventID` and `eventType` (because there can
#' be several types of Events in a single dataset), along with a
#' `parentEventID` which specifies the level under which the current Event sits
#' (e.g., An individual location's survey event ID, which is one of several
#' survey locations on a specific day's set of surveys ie the parentEvent).
#'
#' Examples of `eventID` values:
#' * `INBO:VIS:Ev:00009375`
#'
#' Examples of `eventType` values:
#' * 	`Sample`
#' *  `Observation`
#' *  `Survey`
#' *  `Site Visit`
#' *  `Deployment`
#' See more examples on [dwc.tdwg.org](https://dwc.tdwg.org/list/#dwc_eventType)
#'
#' Examples of `parentEventID`
#' `A1` (To identify the parent event in nested samples, each with their own `eventID` - `A1_1`, `A1_2`)
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang quo_is_null
#' @importFrom rlang enquos
#' @importFrom rlang zap
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @export
use_events <- function(
    .df,
    eventID = NULL,
    eventType = NULL,
    parentEventID = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(eventID, eventType, parentEventID)
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
  # TODO: Uncertain exactly what these should contain given the flexibility of IDs and events
  # check_eventID(level = "abort")
  # check_eventType(level = "abort")
  # check_parentEventID(level = "abort")

  result
}
