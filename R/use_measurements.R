#' Add measurement data for an individual or event to a `tibble`
#'
#' @description
#' This function is a work in progress, and should be used with caution.
#'
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param cols vector of column names to be included as 'measurements'. Unquoted.
#' @param unit vector of strings giving units for each variable
#' @param type vector of strings giving a description for each variable
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' add examples
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang enquos
#' @export
use_measurements <- function(
    .df,
    cols = NULL,
    unit = NULL,
    type = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  # capture columns
  fn_quos <- enquos(cols)

  # Creates measurementOrFact column, nests measurement columns
  cli::cli_progress_step("Adding measurement columns")

  nested_df <- .df |>
    # add row number for id
    mutate(padded_row_number = sequential_id()) |>
    # NOTE: Must use group_split to preserve grouping by row, not an unexpected grouping (ie force rowwise)
    group_split(dplyr::row_number(), .keep = FALSE) |>
    purrr::map_dfr( ~ .x |>
                      nest(measurementOrFact = c(padded_row_number, !!!fn_quos)))

  # Pivots each row's data to long
  # Adds rowwise `unit` and `type` information to each nested tibble
  cli::cli_progress_step("Converting measurements to Darwin Core")

  result <- nested_df |>
    dplyr::mutate(
      measurementOrFact = purrr::map(
        .data$measurementOrFact,
        ~ .x |>
          pivot_longer(names_to = "column_name",
                       values_to = "measurementValue",
                       cols = !!!fn_quos) |>
          mutate(
            measurementID = glue("{column_name}|{padded_row_number}") |> as.character(), # create id
            measurementUnit = unit,
            measurementType = type
          ) |>
          select(-column_name, -padded_row_number)
      ))

  # if(!is.null(result$measurementOrFact)) {
  #   matched_cols = result |>
  #     select(measurementOrFact) |>
  #     unnest(measurementOrFact) |>
  #     colnames()
  # }
  #
  # if(isTRUE(.messages)) {
  #   if(length(matched_cols > 0)) {
  #     col_progress_bar(cols = matched_cols)
  #   }
  # }
  #
  # check_eventDate(result, level = "abort")

  return(result)
}

#' TODO: select & unnest nested columns, then run normal individual term/column checks like other use functions

#' Check measurementUnit
#' @noRd
#' @keywords Internal
check_measurementUnit <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "measurement")){
    .df |>
      select("eventTime") |>
      check_is_time(level = level)
  }
}

