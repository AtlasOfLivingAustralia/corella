#' Suggest a workflow to make data comply with Darwin Core Standard
#'
#' @description
#' Checks whether a `data.frame` or `tibble` conforms to Darwin
#' Core standards and suggests how to standardise a data frame that is not
#' standardised to minimum Darwin Core requirements. This is intended as
#' users' go-to function for figuring out how to get started standardising
#' their data.
#'
#' Output provides a summary to users about which column names
#' match valid Darwin Core terms, the minimum required
#' column names/terms (and which ones are missing), and a suggested workflow to
#' add any missing terms.
#' @param .df A `data.frame`/`tibble` against which checks should be run
#' @returns Invisibly returns the input `data.frame`/`tibble`, but primarily
#' called for the side-effect of running check functions on that input.
#' @examples
#' df <- tibble(
#'   scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
#'   latitude = c(-35.310, "-35.273"), # deliberate error for demonstration purposes
#'   longitude = c(149.125, 149.133),
#'   eventDate = c("14-01-2023", "15-01-2023"),
#'   status = c("present", "present")
#' )
#'
#' # Summarise whether your data conforms to Darwin Core Standard.
#' # See a suggested workflow to amend or add missing information.
#' df |>
#'   suggest_workflow()
#'
#' @order 1
#' @export
suggest_workflow <- function(.df){
  dwc_spinny_message("Checking Darwin Core terms")
  check_dataframe(.df)
  invisible(.df)
}


#' Theatrics
#' @importFrom cli make_spinner
#' @noRd
#' @keywords Internal
dwc_spinny_message <- function(message) {

  # define the spinner
  spinny <- make_spinner(
    which = "dots2",
    template = paste0("{spin}", glue(" {message}"))
  )

  # update the spinner 100 times
  for(i in 1:100) {
    spinny$spin()
    wait(.01)
  }

  # clear the spinner from the status bar
  spinny$finish()
}


#' Checks dataframe and column names
#'
#' @description
#' Checks whether user dataframe contains sf `geometry` and preserves this
#' information, then checks matching user columns to darwin core terms.
#' @noRd
#' @keywords Internal
check_dataframe <- function(.df,
                         level = c("inform", "warn", "abort")){
  level <- match.arg(level)
  is_sf <- inherits(.df, "sf")

  result <- tibble(dwc_terms = colnames(.df)) |>
    check_contains_terms(dwc_terms = dwc_terms,
                         is_sf = is_sf,
                         level = level)
  .df
}


#' Match Darwin Core terms to column names
#'
#' @description
#' This is the main workhorse function of `suggest_workflow()`.
#'
#' @param .df vector of values
#' @param dwc_terms vector of valid Darwin Core terms against which .df should be compared
#' @importFrom cli cli_div
#' @importFrom cli cli_h1
#' @importFrom cli cli_h2
#' @importFrom cli cli_h3
#' @importFrom cli cli_end
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom rlang is_empty
#' @noRd
#' @keywords Internal
check_contains_terms <- function(.df,
                                 dwc_terms,
                                 is_sf,
                                 level = "inform",
                                 call = caller_env()
){
  check_is_dataframe(.df)
  field_name <- colnames(.df)[[1]]
  user_column_names <- .df |>
    pull(field_name) |>
    unique() |>
    sort()
  name_lookup <- user_column_names %in% dwc_terms$term

  ## Matching column names to darwin core terms

  # matches
  matched_values <- user_column_names[name_lookup]
  unmatched_values <- user_column_names[!name_lookup]
  all_cols_match <- is_empty(unmatched_values)

  ## Minimum required terms

  # retrieve required term match results
  req_terms_results <- check_required_terms(user_column_names)

  ## Suggested workflow & Additional functions

  # Function matching for suggested workflow
  main_functions <- fn_to_term_table()$main
  other_functions <- fn_to_term_table()$optional

  suggested_functions <- main_functions |>
    filter(!.data$dwc_term %in% matched_values) |>
    distinct(.data$set_function) |>
    pull("set_function")

  optional_functions <- other_functions |>
    filter(.data$dwc_term %in% matched_values) |>
    distinct(.data$set_function) |>
    pull("set_function")

  # this wraps text (which might not be optimal for this table)
  # withr::with_options(
  #   list(cli.width = 80),
  #   full_workflow_message()
  # )

  full_workflow_message(matched_values,
                        unmatched_values,
                        all_cols_match,
                        req_terms_results,
                        suggested_functions,
                        is_sf,
                        optional_functions)

  .df
}


#' Build "Matching terms" message
#'
#' @importFrom cli ansi_collapse
#' @importFrom cli cat_line
#' @importFrom cli cli_text
#' @importFrom cli cli_bullets
#' @importFrom cli cli_par
#' @importFrom cli col_green
#'
#' @noRd
#' @keywords Internal
matching_terms_message <- function(matched_values,
                                   unmatched_values,
                                   all_cols_match) {

  # concatenate matched & unmatched fields
  matched_string <- ansi_collapse(glue("{matched_values}"), sep = ", ", last = ", ")
  unmatched_string <- ansi_collapse(glue("{unmatched_values}"), sep = ", ", last = ", ")

  results_summary <- paste0("Matched {length(matched_values)} of {sum(length(matched_values), length(unmatched_values))} column name{?s} to DwC terms:")

  # message
  cat_line()
  cat_line(cli_text(results_summary))
  cli_bullets(c("v" = "Matched: {.field {matched_string}}"))
  cli_bullets(c("x" = "Unmatched: {col_red({unmatched_string})}"))
  cli_par()
  if(isTRUE(all_cols_match)) {
    # celebrate
    cat_line(paste0("\n", add_emoji(), " ", col_green("All column names matched DwC terms!"), "\n"))
  }
}

#' build "Matching terms" message
#'
#' @importFrom cli cat_line
#' @importFrom cli col_green
#' @noRd
#' @keywords Internal
minreq_terms_message <- function(req_terms_results) {

  # create message components
  req_terms_table <- build_req_terms_table(req_terms_results)
  all_req_terms_found <- all(req_terms_results$result == "pass")

  # message
  cat_line()
  cat_line(req_terms_table)
  if(isTRUE(all_req_terms_found)) {
    # celebrate
    cat_line(paste0("\n", add_emoji(), " ", col_green("All minimum column requirements met!"), "\n"))
  }
}

#' build suggested workflow message
#'
#' @importFrom cli ansi_collapse
#' @importFrom cli cat_line
#' @importFrom cli cli_text
#' @importFrom cli cli_div
#' @importFrom cli cli_alert
#' @importFrom cli cli_end
#' @importFrom cli style_italic
#' @importFrom rlang is_empty
#' @noRd
#' @keywords Internal
suggest_functions_message <- function(suggested_functions,
                                      is_sf,
                                      .envir = parent.frame()) {

  # if POINT sf class, suggest `set_coordinates_sf()`
  if(isTRUE(is_sf)) {
    # add
    suggested_functions <- c("set_coordinates_sf()", suggested_functions)
  }

  # add pipe when there are multiple suggested functions
  if(length(suggested_functions) > 1) {
    suggested_functions_piped <- c(paste0(utils::head(suggested_functions, -1), " |> "),
                                   utils::tail(suggested_functions, 1))
  } else {
    suggested_functions_piped <- suggested_functions
  }

  # test whether user doesn't need any additional functions
  workflow_is_empty <- is_empty(suggested_functions_piped)

    if(!any(workflow_is_empty)) {
      cat_line(style_italic(paste0("\n", "To make your data Darwin Core compliant, use the following workflow:", "\n")))
      cli_text("df |>")
      cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(suggested_functions_piped, cli_alert, .envir = .envir)
      cli_end()

    } else {
      cat_line(paste0("\n", add_emoji(), " ", col_green("Your dataframe is Darwin Core compliant!"), "\n"))
      cat_line(paste0("Run checks, or use your dataframe to build a Darwin Core Archive:\n"))
      cli_text("df |>")
      cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(paste0("check_dataset()"), cli_alert, .envir = .envir)
      cli_end()
    }
}


#' build additional functions message
#'
#' @importFrom cli col_grey
#' @importFrom cli cli_text
#' @importFrom cli cli_bullets
#' @importFrom cli cli_text
#' @importFrom cli cli_fmt
#' @importFrom cli ansi_collapse
#' @noRd
#' @keywords Internal
additional_functions_message <- function(optional_functions,
                                         .envir = parent.frame()) {

  # add list of optional functions
  if(length(optional_functions) >= 1) {
    optional_functions_string <- ansi_collapse(glue("`{optional_functions}`"),
                                               sep = ", ",
                                               last = ", ",
                                               trunc = 3)
    optional_functions_message <- paste0(
      "{optional_functions_string}") |> cli_text() |> cli_fmt()
  } else {
    optional_functions_message <- NA
  }

  if(!is.na(optional_functions_message)) {
    cli_text(paste0("Based on your matched terms, you can also add to your pipe: ", "\n"))
    cli_bullets(c("*" = optional_functions_message))
  }
  cli_bullets(c("i" = col_grey("See all `set_` functions at http://corella.ala.org.au/reference/index.html#add-rename-or-edit-columns-to-match-darwin-core-terms")))
}


#' Build full workfow message for `suggest_workflow()`
#'
#' @importFrom cli cli_div
#' @importFrom cli cli_end
#' @importFrom cli cli_h1
#' @importFrom cli cli_h3
#'
#' @noRd
#' @keywords Internal
full_workflow_message <- function(matched_values,
                                  unmatched_values,
                                  all_cols_match,
                                  req_terms_results,
                                  suggested_functions,
                                  is_sf,
                                  optional_functions) {

  # build final message with separate components

  # DwC terms
  cli_div()
  cli_h1("Matching Darwin Core terms")
  matching_terms_message(matched_values,
                         unmatched_values,
                         all_cols_match)

  cli_h1("Minimum required Darwin Core terms")
  minreq_terms_message(req_terms_results)
  cli_end()

  # Suggested workflow
  cli_h1("Suggested workflow")
  suggest_functions_message(suggested_functions,
                            is_sf)

  cli_h3(col_grey("Additional functions"))
  additional_functions_message(optional_functions)
}


#' Table of Darwin Core terms and their corresponding `set_` function
#'
#' @importFrom tibble lst
#' @noRd
#' @keywords Internal
fn_to_term_table <- function() {
  main <- tibble::tribble(
    ~"set_function", ~"dwc_term",
    "set_occurrences()", "basisOfRecord",
    "set_occurrences()", "occurrenceID",
    "set_scientific_name()", "scientificName",
    "set_coordinates()", "decimalLatitude",
    "set_coordinates()", "decimalLongitude",
    "set_coordinates()", "geodeticDatum",
    "set_coordinates()", "coordinateUncertaintyInMeters",
    "set_datetime()", "eventDate"
  )

  optional <- tibble::tribble(
    ~"set_function", ~"dwc_term",
    "set_locality()", "continent",
    "set_locality()", "country",
    "set_locality()", "countryCode",
    "set_locality()", "stateProvince",
    "set_locality()", "locality",
    "set_taxonomy()", "kingdom",
    "set_taxonomy()", "phylum",
    "set_taxonomy()", "class",
    "set_taxonomy()", "order",
    "set_taxonomy()", "family",
    "set_taxonomy()", "genus",
    # "set_taxonomy()", "species",
    "set_taxonomy()", "specificEpithet",
    "set_taxonomy()", "vernacularName",
    "set_abundance()", "individualCount",
    "set_abundance()", "organismQuantity",
    "set_abundance()", "organismQuantityType",
    "set_abundance()", "organismQuantity",
    "set_collection()", "datasetID",
    "set_collection()", "datasetName",
    "set_collection()", "catalogNumber",
    "set_coordinates()", "coordinatePrecision",
    "set_scientific_name()", "taxonRank",
    "set_scientific_name()", "scientificNameAuthorship",
    "set_datetime()", "year",
    "set_datetime()", "month",
    "set_datetime()", "day",
    "set_datetime()", "eventTime",
    "set_individual_traits()", "individualID",
    "set_individual_traits()", "lifeStage",
    "set_individual_traits()", "sex",
    "set_individual_traits()", "vitality",
    "set_individual_traits()", "reproductiveCondition",
    "set_observer()", "recordedBy",
    "set_observer()", "recordedByID",
    "set_events()", "eventID",
    "set_events()", "eventType",
    "set_events()", "parentEventID",
    "set_license()", "license",
    "set_license()", "rightsHolder",
    "set_license()", "accessRights",
    "set_measurements()", "measurementValue",
    "set_measurements()", "measurementID",
    "set_measurements()", "measurementUnit",
    "set_measurements()", "measurementType"
  )

  table <- lst(main, optional) # named list

  return(table)
}


#' Build table for messaging about minimum required terms
#'
#' @importFrom cli ansi_align
#' @importFrom cli ansi_collapse
#' @importFrom cli ansi_nchar
#' @importFrom cli col_blue
#' @importFrom cli col_green
#' @importFrom cli col_red
#' @importFrom cli symbol
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr full_join
#' @importFrom dplyr join_by
#' @importFrom rlang .data
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @noRd
#' @keywords Internal
build_req_terms_table <- function(req_terms) {

  # Unnest & concatenate terms by group
  missing_results <- req_terms |>
    select(-"matched") |>
    unnest(cols = c(.data$missing)) |>
    group_by(.data$term_group) |>
    mutate( # glue names
      missing = ansi_collapse(.data$missing, sep = ", ", last = ", ")
    ) |>
    unique()

  matched_results <- req_terms |>
    select(-"missing") |>
    unnest(cols = c(.data$matched)) |>
    group_by(.data$term_group) |>
    mutate( # glue names
      matched = ansi_collapse(.data$matched, sep = ", ", last = ", ")
    ) |>
    unique()

  req_terms_message <- missing_results |>
    full_join(matched_results,
              join_by("term_group", "result")) |>
    # remove other Identifier terms if one or more are matched
    mutate(
      missing = case_when(
        .data$term_group == "Identifier (at least one)" & !is.na(.data$matched) ~ NA,
        .default = .data$missing
        )) |>
    # add blank space for correct message formatting
    tidyr::replace_na(list(missing = stringr::str_pad("-", width = 16, side = "right"),
                           matched = stringr::str_pad("-", width = 16, side = "right")))

  # Group terms found vs missing
  pass <- req_terms_message |>
    filter(.data$result == "pass")

  failed <- req_terms_message |>
    filter(.data$result == "fail")

  pass_group <- glue("{pass$term_group}")
  pass_matched <- glue("{pass$matched}")
  pass_missing <- glue("{pass$missing}")
  fail_group <- glue("{failed$term_group}")
  fail_matched <- glue("{failed$matched}")
  fail_missing <- glue("{failed$missing}")

  headers <- paste0(
    "  ",
    ansi_align(col_blue("Type"), max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_blue("Matched term(s)"), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_blue("Missing term(s)"), max(ansi_nchar(c(pass_missing, fail_missing)))),"\n",
    collapse = "\n"
  )

  bullets_found <- paste0(paste0(
    col_green(symbol$tick), " ",
    ansi_align(pass_group, max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_green(pass_matched), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_red(pass_missing), max(ansi_nchar(c(pass_missing, fail_missing)))), " ",
    collapse = "\n"
  ), "\n")

  bullets_missing <- paste0(paste0(
    col_red(symbol$cross), " ",
    ansi_align(fail_group, max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_green(fail_matched), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_red(fail_missing), max(ansi_nchar(c(pass_missing, fail_missing)))), " ",
    collapse = "\n"
  ), "\n")


  # Remove tick when all terms are matched or missing
  if(nrow(pass) == 0) {
    bullets_found <- NULL
  }

  if(nrow(failed) == 0) {
    # celebrate
    bullets_missing <- NULL
  }

  # final message
  paste0(
    headers,
    bullets_found,
    bullets_missing
  )
}

#' Minimum required terms for a Darwin Core compliant data archive
#' @noRd
#' @keywords Internal
required_terms <- function() {
  terms <- list(
    identifier = c(
      "occurrenceID",
      "catalogNumber",
      "recordNumber"
    ),
    basis = c(
      "basisOfRecord"
    ),
    name = c(
      "scientificName"
    ),
    location = c(
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "coordinateUncertaintyInMeters"
    ),
    date = c(
      "eventDate"
    )
  )
}

#' Return missing minimum required terms
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
check_required_terms <- function(user_column_names) {

  terms <- required_terms()

  id <- tibble(
    term_group = "Identifier (at least one)",
    missing = list(terms$identifier[!terms$identifier %in% user_column_names]),
    matched = list(terms$identifier[terms$identifier %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) != 3, "pass", "fail")
    )
  basis <- tibble(
    term_group = "Record type",
    missing = list(terms$basis[!terms$basis %in% user_column_names]),
    matched = list(terms$basis[terms$basis %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  name <- tibble(
    term_group = "Scientific name",
    missing = list(terms$name[!terms$name %in% user_column_names]),
    matched = list(terms$name[terms$name %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  location <- tibble(
    term_group = "Location",
    missing = list(terms$location[!terms$location %in% user_column_names]),
    matched = list(terms$location[terms$location %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  date <- tibble(
    term_group= "Date/Time",
    missing = list(terms$date[!terms$date %in% user_column_names]),
    matched = list(terms$date[terms$date %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )

  # combine
  all_terms <- bind_rows(id, basis, name, location, date)

  # convert empty row value to NULL
  result <- all_terms |>
    mutate(
      missing = lapply(.data$missing, function(x) if(identical(x, character(0))) NULL else x),
      matched = lapply(.data$matched, function(x) if(identical(x, character(0))) NULL else x)
    )

  return(result)
}


#' Add happy emoji
#' @noRd
#' @keywords Internal
add_emoji <- function() {
  emoji <- c(
    "\U0001f600", # smile
    "\U0001f973", # party face
    "\U0001f638", # cat grin
    "\U0001f308", # rainbow
    "\U0001f947", # gold medal
    "\U0001f389", # party popper
    "\U0001f38a" # confetti ball
  )
  sample(emoji, 1)
}
