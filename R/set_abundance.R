#' Set, create or modify columns with abundance information
#'
#' @description
#' In some field methods, it is common to observe more than one individual
#' per observation; to observe abundance using non-integer measures such as
#' mass or area; or to seek individuals but not find them (abundance of zero).
#' As these approaches use different Darwin Core terms, this function assists in
#' specifying abundances to a `tibble` using Darwin Core Standard.
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for how columns with
#' abundance information are represented in the Darwin Core Standard.
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param individualCount The number of individuals present
#' @param organismQuantity A number or enumeration value for the quantity of
#' organisms. Used together with `organismQuantityType` to provide context.
#' @param organismQuantityType The type of quantification system used for
#' `organismQuantity`.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested fields added/reformatted.
#'
#' @details
#' Examples of `organismQuantity` & `organismQuantityType` values:
#' * 27 (`organismQuantity`) individuals (`organismQuantityType`)
#' * 12.5 (`organismQuantity`) % biomass (`organismQuantityType`)
#' * r (`organismQuantity`) Braun-Blanquet Scale (`organismQuantityType`)
#' * many (`organismQuantity`) individuals (`organismQuantityType`)
#'
#' @examples
#' df <- tibble::tibble(
#'   scientificName = c("Cacatua (Licmetis) tenuirostris",
#'                      "Cacatua (Licmetis) tenuirostris",
#'                      "Cacatua (Licmetis) tenuirostris"),
#'   n_obs = c(1, 3, 4)
#'   )
#'
#' df |>
#'   set_abundance(individualCount = n_obs)
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
set_abundance <- function(.df,
                          individualCount = NULL,
                          organismQuantity = NULL,
                          organismQuantityType = NULL,
                          .keep = "unused"
                          ){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(individualCount, organismQuantity, organismQuantityType)
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
  if(length(matched_cols > 0)) {
    col_progress_bar(cols = matched_cols)
  }

  # run column checks
  check_individualCount(result, level = "abort")
  check_organismQuantity(result, level = "abort")
  check_organismQuantityType(result, level = "abort")

  return(result)

}


#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @importFrom cli cli_abort
#' @noRd
#' @keywords Internal
check_individualCount <- function(.df,
                                  level = c("inform", "warn", "abort")
                                  ){
  level <- match.arg(level)
  if(any(colnames(.df) == "individualCount")){
    .df |>
      select("individualCount") |>
      check_is_numeric(level = level)

    if(any(.df$individualCount == 0)) {

      # check for occurrenceStatus column
      if("occurrenceStatus" %in% colnames(.df)) {

        # make sure 0s are tagged as absences in occurrenceStatus
        absences <- .df |>
          select("individualCount", "occurrenceStatus") |>
          filter(.data$individualCount == 0) |>
          mutate(match = .data$individualCount == 0 & .data$occurrenceStatus == "absent")

        if(any(absences$match == FALSE)) {
          n_unmatched <- absences |>
            filter(match == FALSE) |>
            nrow()
          bullets <- c(
            "{.field individualCount} values do not match {.field occurrenceStatus}.",
            x = "Found {n_unmatched} row{?s} where individualCount = 0 but occurrenceStatus = \"present\"."
          ) |>
            cli_bullets() |>
            cli_fmt()

          switch_check(level, bullets)
        }
      } else {

        # enforce that occurrenceStatus must be included
        bullets <- c(
          "{.field individualCount} of 0 detected but not marked as absence.",
          i = "Must use {.field occurrenceStatus} to mark counts of 0 as \"absent\".",
          i = "Use {.code set_occurrences(occurrenceStatus = ifelse(individualCount == 0, \"absent\", \"present\"))}."
        ) |>
          cli_bullets() |>
          cli_fmt()
        switch_check(level, bullets)
      }
    }
  }

  .df
}



#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_organismQuantity <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "organismQuantity")){
    if (!any(colnames(.df) == "organismQuantityType")) {
      bullets <- cli_bullets(c(
        "Missing {.field organismQuantityType} in dataframe.",
        i = "Must include {.field organismQuantityType} to give context to quantity. See {.code ?set_abundance}."
      )) |> cli_fmt()
      switch_check(level,
                   bullets)
    }
  }
  .df
}



#' @param level what action should the function take for non-conformance?
#' Defaults to `"inform"`.
#' @noRd
#' @keywords Internal
check_organismQuantityType <- function(.df,
                                   level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "organismQuantityType")){
    .df |>
      select("organismQuantityType") |>
      check_is_string(level = level)

    if (!any(colnames(.df) == "organismQuantity")) {
      bullets <- cli_bullets(c(
        "Missing {.field organismQuantity} in dataframe.",
        i = "Must include {.field organismQuantity} to give a quantity to measurement type. See {.code ?set_abundance}."
      )) |> cli_fmt()
      switch_check(level,
                   bullets)
    }
  }
  .df
}

