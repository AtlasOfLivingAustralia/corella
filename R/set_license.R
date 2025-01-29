#' Set, create or modify columns with license and rights information
#'
#' @description
#' Format fields that contain information on permissions for use, sharing or
#' access to a record.
#'
#' In practice this function is no different from using `mutate()`, but gives
#' some informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#'
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param license A legal document giving official permission to do something
#' with the resource. Must be provided as a url to a valid license.
#' @param rightsHolder Person or organisation owning or managing rights to
#' resource.
#' @param accessRights Access or restrictions based on privacy or security.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `license` values:
#' * `http://creativecommons.org/publicdomain/zero/1.0/legalcode`
#' * `http://creativecommons.org/licenses/by/4.0/legalcode`
#' * `CC0`
#' * `CC-BY-NC 4.0 (Int)`
#'
#' Examples of `rightsHolder` values:
#' * `The Regents of the University of California`
#'
#' Examples of `accessRights` values:
#' * `not-for-profit use only` (string example)
#' * `https://www.fieldmuseum.org/field-museum-natural-history-conditions-and-suggested-norms-use-collections-data-and-images` (URI example)
#'
#' @seealso [set_observer()] for adding observer information.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
set_license <- function(
    .df,
    license = NULL,
    rightsHolder = NULL,
    accessRights = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(accessRights, license, rightsHolder)
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
  check_license(result, level = "abort")
  check_rightsHolder(result, level = "abort")
  check_accessRights(result, level = "abort")

  return(result)
}


#' Check license
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_license <- function(.df,
                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "license")){
    .df |>
      select("license") |>
      check_is_string(level = level)
  }
  .df
}

#' Check rightsHolder
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_rightsHolder <- function(.df,
                               level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "rightsHolder")){
    .df |>
      select("rightsHolder") |>
      check_is_string(level = level)
  }
  .df
}

#' Check accessRights
#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_accessRights <- function(.df,
                               level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "accessRights")){
    .df |>
      select("accessRights") |>
      check_is_string(level = level)
  }
  .df
}
