#' Set, create or modify columns with geological information of a sample/fossil/specimen
#'
#' @description
#' Material samples, fossils and specimens can require additional geological context
#' about their stratigraphy (i.e. layer of rock), including information about
#' their scientific dating estimate.
#'
#' This function helps to format columns
#' with [GeologicalContext](https://dwc.tdwg.org/terms/#geologicalcontext)
#' information to a `tibble` using Darwin Core Standard.
#'
#' In practice this is used no differently from `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for fields in
#' the Darwin Core Standard.
#' 
#' Note that other valid Darwin Core terms exist for recording 
#' geological context using 
#' [Eon/Eonothem](http://rs.tdwg.org/dwc/terms/earliestEonOrLowestEonothem) and  
#' [Era/Erathem](http://rs.tdwg.org/dwc/terms/earliestEraOrLowestErathem). 
#' These terms are still accepted within corella's valid [darwin_core_terms] 
#' but are not currently supported arguments in `set_geological_context()`.
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param geologicalContextID A unique identifier associated with the geological
#' information.
#' @param earliestAgeOrLowestStage The full name of the earliest possible
#' geochronologic age or lowest chronostratigraphic stage attributable to the
#' stratigraphic horizon from which the material was collected.
#' @param latestAgeOrHighestStage The full name of the latest possible
#' geochronologic age or highest chronostratigraphic stage attributable to the
#' stratigraphic horizon from which the material was collected.
#' @param earliestPeriodOrLowestSystem The full name of the earliest possible
#' geochronologic period or lowest chronostratigraphic system attributable to the
#' stratigraphic horizon from which the material was collected.
#' @param latestPeriodOrHighestSystem The full name of the latest possible
#' geochronologic period or lowest chronostratigraphic system attributable to the
#' stratigraphic horizon from which the material was collected.
#' @param lowestBiostratigraphicZone The full name of the lowest possible
#' geological biostratigraphic zone of the stratigraphic horizon from which
#' the material was collected.
#' @param highestBiostratigraphicZone The full name of the highest possible
#' geological biostratigraphic zone of the stratigraphic horizon from which
#' the material was collected.
#' @param lithostratigraphicTerms The combination of all lithostratigraphic
#' names for the rock from which the material was collected.
#' @param group The full name of the lithostratigraphic group from which the
#' material was collected.
#' @param formation The full name of the lithostratigraphic formation from
#' which the material was collected.
#' @param member The full name of the lithostratigraphic member from which the
#' material was collected.
#' @param bed The full name of the lithostratigraphic bed from which the
#' material was collected.
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested columns added/reformatted.
#' @details
#'
#' Examples of `geologicalContextID`:
#' *  `https://opencontext.org/subjects/e54377f7-4452-4315-b676-40679b10c4d9`
#'
#' Examples of `earliestAgeOrLowestStage`:
#' *  `Atlantic`
#' *  `Boreal`
#' *  `Skullrockian`
#' 
#' Examples of `earliestPeriodOrLowestSystem`:
#' *  `Neogene`
#' *  `Tertiary`
#' *  `Quaternary`
#'
#' Examples of `lowestBiostratigraphicZone`:
#' *  `Maastrichtian`
#'
#' Examples of `highestBiostratigraphicZone`:
#' *  `Blancan`
#'
#' Examples of `lithostratigraphicTerms`:
#' *  `Pleistocene-Weichselien`
#'
#' Examples of `group`:
#' *	`Bathurst`
#' *  `Lower Wealden`
#'
#' Examples of `formation`:
#' *  `Notch Peak Formation`
#' *  `House Limestone`
#' *  `Fillmore Formation`
#'
#' Examples of `member`:
#' *  `Lava Dam Member`
#' *  `Hellnmaria Member`
#'
#' Examples of `bed`:
#' *  `Harlem coal`
#'
#' @seealso [set_locality()] for adding location information.
#' @examples
#' df <- tibble::tibble(
#'   scientificName = c("Capra", "Lepus"),
#'   latitude = c(32.827191, 32.8225),
#'   longitude = c(37.668196, 37.666389),
#'   eventDate = c("2010-07-20", "2014-03-31"),
#'   geologicalContextID = c("https://opencontext.org/subjects/576d8322-9a55-4a9b-e60d-b466be610bb7", 
#'                           "https://opencontext.org/subjects/4ef35961-af07-4dec-3106-48baf0967a0a"),
#'   earliestPeriodOrLowestSystem = c("Quartenary", "Quartenary"),
#'   latestPeriodOrLowestSystem = c("Quartenary", "Quartenary"),
#'   country = c("Turkey", "Turkey"),
#'   locality = c("Mound East", "Mound West Trench 1")
#' )
#'
#' # Reformat columns to Darwin Core Standard terms
#' df |>
#'   set_geological_context(
#'     countryCode = countryCode,
#'     stateProvince = state,
#'     locality = locality
#'   )
#'
#' # Columns with valid Darwin Core terms as names are automatically detected
#' # and checked. This will do the same as above.
#' df |>
#'   set_locality(
#'     stateProvince = state
#'   )
#'
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom rlang enquos
#' @importFrom rlang quo_is_null
#' @importFrom rlang zap
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @export
set_geological_context <- function(.df,
                         geologicalContextID = NULL,
                         earliestAgeOrLowestStage = NULL,
                         latestAgeOrHighestStage = NULL,
                         earliestPeriodOrLowestSystem = NULL,
                         latestPeriodOrHighestSystem = NULL,
                         lowestBiostratigraphicZone = NULL,
                         highestBiostratigraphicZone = NULL,
                         lithostratigraphicTerms = NULL,
                         group = NULL,
                         formation = NULL,
                         member = NULL,
                         bed = NULL,
                         .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(bed, earliestAgeOrLowestStage, earliestPeriodOrLowestSystem,
                    formation, geologicalContextID, group,
                    highestBiostratigraphicZone, latestAgeOrHighestStage, 
                    latestPeriodOrHighestSystem, lithostratigraphicTerms, 
                    lowestBiostratigraphicZone, member)
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
  check_geologicalContextID(result, level = "abort")
  check_earliestAgeOrLowestStage(result, level = "abort")
  check_latestAgeOrHighestStage(result, level = "abort")
  check_lowestBiostratigraphicZone(result, level = "abort")
  check_highestBiostratigraphicZone(result, level = "abort")
  check_lithostratigraphicTerms(result, level = "abort")
  check_group(result, level = "abort")
  check_formation(result, level = "abort")
  check_member(result, level = "abort")
  check_bed(result, level = "abort")

  result
}


#' Check geologicalContextID
#' @noRd
#' @keywords Internal
check_geologicalContextID <- function(.df,
                                      level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "geologicalContextID")){
    .df |>
      select("geologicalContextID") |>
      check_is_unique(level = level)
  }
  .df
}

#' Check earliestAgeOrLowestStage
#' @noRd
#' @keywords Internal
check_earliestAgeOrLowestStage <- function(.df,
                                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "earliestAgeOrLowestStage")){
    .df |>
      select("earliestAgeOrLowestStage") |>
      check_is_string(level = level)
      
  }
  .df
}

#' Check latestAgeOrHighestStage
#' @noRd
#' @keywords Internal
check_latestAgeOrHighestStage <- function(.df,
                                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "latestAgeOrHighestStage")){
    .df |>
      select("latestAgeOrHighestStage") |>
      check_is_string(level = level)
  }
  .df
}

#' Check earliestPeriodOrLowestSystem
#' @noRd
#' @keywords Internal
check_earliestPeriodOrLowestSystem <- function(.df,
                                               level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "earliestPeriodOrLowestSystem")){
    .df |>
      select("earliestPeriodOrLowestSystem") |>
      check_is_string(level = level)
      
  }
  .df
}

#' Check latestPeriodOrHighestSystem
#' @noRd
#' @keywords Internal
check_latestPeriodOrHighestSystem <- function(.df,
                                              level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "latestPeriodOrHighestSystem")){
    .df |>
      select("latestPeriodOrHighestSystem") |>
      check_is_string(level = level)
  }
  .df
}

#' Check lowestBiostratigraphicZone
#' @noRd
#' @keywords Internal
check_lowestBiostratigraphicZone <- function(.df,
                                             level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "lowestBiostratigraphicZone")){
    .df |>
      select("lowestBiostratigraphicZone") |>
      check_is_string(level = level)
  }
  .df
}

#' Check highestBiostratigraphicZone
#' @noRd
#' @keywords Internal
check_highestBiostratigraphicZone <- function(.df,
                                              level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "highestBiostratigraphicZone")){
    .df |>
      select("highestBiostratigraphicZone") |>
      check_is_string(level = level)
  }
  .df
}

#' Check lithostratigraphicTerms
#' @noRd
#' @keywords Internal
check_lithostratigraphicTerms <- function(.df,
                                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "lithostratigraphicTerms")){
    .df |>
      select("lithostratigraphicTerms") |>
      check_is_string(level = level)
  }
  .df
}

#' Check group
#' @noRd
#' @keywords Internal
check_group <- function(.df,
                        level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "group")){
    .df |>
      select("group") |>
      check_is_string(level = level)
  }
  .df
}

#' Check formation
#' @noRd
#' @keywords Internal
check_formation <- function(.df,
                            level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "formation")){
    .df |>
      select("formation") |>
      check_is_string(level = level)
  }
  .df
}

#' Check member
#' @noRd
#' @keywords Internal
check_member <- function(.df,
                         level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "member")){
    .df |>
      select("member") |>
      check_is_string(level = level)
  }
  .df
}

#' Check bed
#' @noRd
#' @keywords Internal
check_bed <- function(.df,
                      level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "bed")){
    .df |>
      select("bed") |>
      check_is_string(level = level)
  }
  .df
}