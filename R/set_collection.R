#' Set, create or modify columns with museum- or collection-specific information
#'
#' @description
#' Format fields that specify the collection or catalog number of a
#' specimen or occurrence record to a `tibble` using Darwin Core Standard.
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#'
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param datasetID An identifier for the set of data. May be a global unique
#' identifier or an identifier specific to a collection or institution.
#' @param datasetName The name identifying the data set from which the record
#' was derived.
#' @param catalogNumber A unique identifier for the record within the data set
#' or collection.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested fields added/reformatted.
#' @details
#' Examples of `datasetID` values:
#' * `b15d4952-7d20-46f1-8a3e-556a512b04c5`
#'
#' Examples of `datasetName` values:
#' * `Grinnell Resurvey Mammals`
#' * `Lacey Ctenomys Recaptures`
#'
#' Examples of `catalogNumber` values:
#' * `145732`
#' * `145732a`
#' * `2008.1334`
#' * `R-4313`
#'
#' @examples
#' df <- tibble::tibble(
#'   name = c("Crinia Signifera", "Crinia Signifera", "Litoria peronii"),
#'   eventDate = c("2010-10-14", "2010-10-14", "2010-10-14"),
#'   catalog_num = c("16789a", "16789c", "08742f"),
#'   dataset = c("Frog search", "Frog search", "Frog search")
#'   )
#'
#' # Reformat columns to Darwin Core terms
#' df |>
#'   set_collection(
#'     catalogNumber = catalog_num,
#'     datasetName = dataset
#'     )
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
set_collection <- function(
    .df,
    datasetID = NULL,
    datasetName = NULL,
    catalogNumber = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(catalogNumber, datasetID, datasetName)
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
  check_datasetName(result, level = "abort")

  # NOTE: Unsure of what checks are useful for columns below
  # check_catalogNumber(result, level = "abort")
  # check_datasetID(result, level = "abort")

  return(result)
}



#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_datasetName <- function(.df,
                              level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "datasetName")){
    .df |>
      select("datasetName") |>
      check_is_string(level = level)
  }
  .df
}

