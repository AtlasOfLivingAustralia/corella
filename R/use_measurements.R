#' Add museum- or collection-specific information to a `tibble`
#'
#' @description
#' test
#'
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' add examples
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_measurements <- function(
    .df,
    cols = NULL,
    type = NULL,
    unit = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_quos <- enquos(cols)

  # browser()

  # Nests measurement columns
  cli::cli_progress_step("Adding measurement columns")

  nested_df <- df_filtered |>
    # NOTE: Must use group_split to ensure we are grouping by row, not an unexpected grouping (ie rowwise)
    group_split(row_number(), .keep = FALSE) %>%
    purrr::map_dfr( ~ .x |>
                      nest(measurementOrFact = c(LMA_g.m2, LeafN_area_g.m2, PNUE)))

  # Pivots each row's data to long
  # Adds `unit` and `type` information to each nested tibble
  cli::cli_progress_step("Converting measurements to Darwin Core")

  result <- nested_df |>
    dplyr::mutate(
      measurementOrFact = purrr::map(
        measurementOrFact,
        ~ .x |>
          pivot_longer(names_to = "column_name",
                       values_to = "measurementValue",
                       cols = everything()) |>
          mutate(
            measurementUnit = unit,
            measurementType = type
          )
      ))

  return(result)
}

#' Add measurement column to dataframe in Darwin Core standard
#'
#' @description
#' tbd
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom dplyr n
#' @export
add_measure_column <- function(.df,
                               column = NULL,
                               type = NULL,
                               unit = NULL) {

  # column_name <- {{column}}



}
