#' Set, create or modify columns with Event information using Darwin Core
#'
#' @description
#' Format columns that contain information about an
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
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param eventID A unique identifier for an individual Event.
#' @param eventType The type of Event
#' @param parentEventID The parent event under which one or more Events sit
#' within.
#' @param .keep Control which columns from `.df` are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults
#' to `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used
#' to generate them.
#' @param .keep_composite Control which columns from `.df` are kept when
#' [composite_id()] is used to assign values to `eventID`, defaulting to
#' `"all"`. This has a different default from `.keep` because composite
#' identifiers often contain information that is valuable in other contexts,
#' meaning that deleting these columns by default is typically unwise.
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
#' @importFrom purrr keep
#' @export
set_events <- function(
    .df,
    eventID = NULL,
    eventType = NULL,
    parentEventID = NULL,
    .keep = "unused",
    .keep_composite = "all"
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
    map(.f = rlang::quo_is_null) |>
    unlist()

  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))

  if(any(null_col_exists_in_df)){
    fn_quos <- fn_quos |>
      keep(!names(fn_quos) %in% names(which(null_col_exists_in_df)))
  }

  # check whether `composite_id()` is called within `eventID`
  # and if so, parse with .keep = .keep_composite
  eventID_check <- names(fn_quos) == "eventID"
  if(any(eventID_check)){
    eventID_quo <- fn_quos[which(eventID_check)]
    if(!rlang::quo_is_null(eventID_quo[[1]])){
      if(grepl("composite_id\\(", as_label(eventID_quo[[1]]))){
        .df <- .df |> mutate(!!!eventID_quo, .keep = .keep_composite)
        # now remove eventID_quo from consideration
        fn_quos <- fn_quos[-which(eventID_check)]
      }
    }
  }

  # Update df
  if(length(fn_quos) > 0){
    result <- .df |>
      mutate(!!!fn_quos, .keep = .keep)
  }else{
    result <- .df
  }

  check_missing_all_args(fn_call = match.call(),
                         fn_args = fn_args,
                         user_cols = colnames(result))

  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  col_progress_bar(cols = matched_cols)

  # run column checks
  # TODO: Uncertain exactly what these should contain given the flexibility of IDs and events
  # for now just make them string checks
  # later eventID should be unique for type = events, but not type = occurrences
  check_eventID(result, level = "abort")
  check_eventType(result, level = "abort")
  check_parentEventID(result, level = "abort")

  result
}

#' check eventID
#' @noRd
#' @keywords Internal
check_eventID <- function(.df,
                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "eventID")){
    .df |>
      select("eventID") |>
      check_is_string(level = level)
  }
  .df
}

#' check eventType
#' @noRd
#' @keywords Internal
check_eventType <- function(.df,
                            level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "eventType")){
    .df |>
      select("eventType") |>
      check_is_string(level = level)
  }
  .df
}

#' check parentEventID
#' @noRd
#' @keywords Internal
check_parentEventID <- function(.df,
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "parentEventID")){
    .df |>
      select("parentEventID") |>
      check_is_string(level = level)
  }
  .df
}
