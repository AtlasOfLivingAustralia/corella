#' Convert columns with measurement data for an individual or event to Darwin Core standard
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function is a work in progress, and should be used with caution.
#'
#' In raw collected data, many types of information can be captured in one
#' column. For example, the column name `LMA_g.m2` contains the measured trait
#' (Leaf Mass per Area, LMA) and the unit of measurement (grams per meter
#' squared, g/m2), and recorded in that column are the values themselves. In
#' Darwin Core, these different types of information must be separated into
#' multiple columns so that they can be ingested correctly and aggregated with
#' sources of data accurately.
#'
#' This function converts information preserved in a single measurement column
#' into multiple columns (`measurementID`, `measurementUnit`, and
#' `measurementType`) as per Darwin Core standard.
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
#' Columns are nested in a
#' single column `measurementOrFact` that contains Darwin Core standard
#' measurement fields. By nesting three measurement columns within the
#' `measurementOrFact`, despite measurement columns converting to long format
#' (one row per measurement, per occurrence),
#' data will remain organised by occurrences (one row per occurrences). Data
#' can be unnested into long format using `dplyr::unnest()`
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang enquos
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_done
#' @importFrom dplyr row_number
#' @importFrom purrr map_dfr
#' @export
set_measurements <- function(
    .df,
    cols = NULL,
    unit = NULL,
    type = NULL,
    .keep = "unused",
    .messages = TRUE
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  # capture columns
  fn_quos <- enquos(cols)

  # Creates measurementOrFact column, nests measurement columns
  cli_progress_step("Adding measurement columns")

  df_split <- .df |>
    # add row number for id
    mutate(
      padded_row_number = stringr::str_pad(row_number(),
                                           floor(log10(row_number())) + 1,
                                           pad = '0')
      ) |>
    group_split(dplyr::row_number(), .keep = FALSE)
    # NOTE: Must use group_split to preserve grouping by row, not an unexpected grouping (ie force rowwise)

  nested_df <- purrr::map_dfr(df_split,
                 ~ .x |>
                   nest(measurementOrFact = c(padded_row_number, !!!fn_quos))
                 )

  cli_progress_done()

  # Pivots each row's data to long
  # Adds rowwise `unit` and `type` information to each nested tibble
  cli_progress_step("Converting measurements to Darwin Core")

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

  cli_progress_done()

  # inform user which columns will be checked
  # if they've made it this far, these columns should exist (and be checked)
  matched_cols <- c("measurementID", "measurementUnit", "measurementType")

  if(isTRUE(.messages)) {
    if(length(matched_cols > 0)) {
      col_progress_bar(cols = matched_cols)
    }
  }

  check_measurementID(result, level = "abort")
  check_measurementUnit(result, level = "abort")
  check_measurementType(result, level = "abort")

  cli::cli_progress_step("Successfully nested measurement columns in column {.field measurementOrFact}.")

  return(result)
}

#' TODO: select & unnest nested columns, then run normal individual term/column checks like other use functions

#' Check measurementID
#' @importFrom tidyr unnest
#' @noRd
#' @keywords Internal
check_measurementID <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "measurementOrFact")) {
    #unnest columns
    result <- .df |>
      unnest(cols = measurementOrFact)


    if(any(colnames(.df) == "measurementID")){
      .df |>
        select("measurementID") |>
        check_is_string(level = level)
      # add check that column isn't empty or only NA?
    }
  }
}

#' Check measurementUnit
#' @importFrom tidyr unnest
#' @noRd
#' @keywords Internal
check_measurementUnit <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "measurementOrFact")) {
    #unnest columns
    result <- .df |>
      unnest(cols = measurementOrFact)


    if(any(colnames(.df) == "measurementUnit")){
      .df |>
        select("measurementUnit") |>
        check_is_string(level = level)
      # add check that column isn't empty or only NA?
    }
  }
}

#' Check measurementType
#' @importFrom tidyr unnest
#' @noRd
#' @keywords Internal
check_measurementType <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "measurementOrFact")) {
    # unnest columns
    result_unnested <- .df |>
      unnest(cols = measurementOrFact)


    if(any(colnames(result_unnested) == "measurementType")){
      result_unnested |>
        select("measurementType") |>
        check_is_string(level = level)
      # add check that column isn't empty or only NA?
    }
  }
}

