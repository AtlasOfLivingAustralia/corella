#' Set, create or modify columns with information of individual organisms
#'
#' @description
#' Format fields that contain measurements or attributes of individual
#' organisms to a `tibble` using Darwin Core Standard. Fields include those
#' that specify sex, life stage or condition. Individuals can be identified by
#' an `individualID` if data contains resampling.
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param individualID An identifier for an individual or named group of
#' individual organisms represented in the Occurrence. Meant to accommodate
#' resampling of the same individual or group for monitoring purposes. May
#' be a global unique identifier or an identifier specific to a data set.
#' @param lifeStage The age class or life stage of an organism at the time of
#' occurrence.
#' @param sex The sex of the biological individual.
#' @param vitality An indication of whether an organism was alive or dead at
#' the time of collection or observation.
#' @param reproductiveCondition The reproductive condition of the biological
#' individual.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested fields added/reformatted.
#' @details
#' Examples of `lifeStage` values:
#' * `zygote`
#' * `larva`
#' * `adult`
#' * `seedling`
#' * `flowering`
#'
#' Examples of `vitality` values:
#' * `alive`
#' * `dead`
#' * `uncertain`
#'
#' Examples of `reproductiveCondition` values:
#' * `non-reproductive`
#' * `pregnant`
#' * `in bloom`
#' * `fruit bearing`
#'
#' @seealso [set_scientific_name()] for adding `scientificName` and authorship information.
#' @examples
#' df <- tibble::tibble(
#'   name = c("Crinia Signifera", "Crinia Signifera", "Litoria peronii"),
#'   latitude = c(-35.27, -35.24, -35.83),
#'   longitude = c(149.33, 149.34, 149.34),
#'   eventDate = c("2010-10-14", "2010-10-14", "2010-10-14"),
#'   id = c(4421, 4422, 3311),
#'   life_stage = c("juvenile", "adult", "adult")
#'   )
#'
#' # Reformat columns to Darwin Core Standard
#' df |>
#'   set_individual_traits(
#'     individualID = id,
#'     lifeStage = life_stage
#'     )
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
set_individual_traits <- function(
    .df,
    individualID = NULL,
    lifeStage = NULL,
    sex = NULL,
    vitality = NULL,
    reproductiveCondition = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(individualID, lifeStage, reproductiveCondition, sex, vitality)
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
  check_individualID(result, level = "abort")
  check_lifeStage(result, level = "abort")
  check_sex(result, level = "abort")
  check_vitality(result, level = "abort")
  check_reproductiveCondition(result, level = "abort")

  return(result)
}



#' Check individualID
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_individualID <- function(.df,
                                 level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "individualID")){
    .df |>
      select("individualID") |>
      check_is_unique(level = level)
  }
  .df
}



#' Check lifeStage
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_lifeStage <- function(.df,
                            level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "lifeStage")){
    .df |>
      select("lifeStage") |>
      check_is_string(level = level)
  }
  .df
}



#' Check sex
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_sex <- function(.df,
                      level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "sex")){
    .df |>
      select("sex") |>
      check_is_string(level = level)
  }
  .df
}



#' Check vitality
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_vitality <- function(.df,
                           level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "vitality")){
    .df |>
      select("vitality") |>
      check_is_string(level = level)
  }
  .df
}



#' Check reproductiveCondition
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_reproductiveCondition <- function(.df,
                           level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "reproductiveCondition")){
    .df |>
      select("reproductiveCondition") |>
      check_is_string(level = level)
  }
  .df
}
