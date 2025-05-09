#' Set, create or modify columns with spatial information
#'
#' This function helps format standard location fields like
#' latitude and longitude point coordinates to a `tibble` using Darwin Core
#' Standard.
#'
#' In practice this is no different from using `mutate()`, but gives some
#' informative errors, and serves as a useful lookup for how spatial columns are
#' represented in the Darwin Core Standard.
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param decimalLatitude The latitude in decimal degrees.
#' @param decimalLongitude The longitude in decimal degrees.
#' @param geodeticDatum The datum or spatial reference system that coordinates
#' are recorded against (usually "WGS84" or "EPSG:4326"). This is often known
#' as the Coordinate Reference System (CRS). If your coordinates are from a GPS
#' system, your data are already using WGS84.
#' @param coordinateUncertaintyInMeters (numeric) Radius of the smallest circle
#' that contains the whole location, given any possible measurement error.
#' `coordinateUncertaintyInMeters` will typically be around `30` (metres) if
#' recorded with a GPS after 2000, or `100` before that year.
#' @param coordinatePrecision (numeric) The precision that `decimalLatitude` and
#' `decimalLongitude` are supplied to. `coordinatePrecision` should be no less
#' than 0.00001 if data were collected using GPS.
#' @param .keep Control which columns from `.df` are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested columns added/reformatted.
#' @details
#' Example values are:
#' * `geodeticDatum` should be a valid EPSG code
#' @seealso [set_locality()] for provided text-based spatial information.
#' @examples
#' df <- tibble::tibble(
#'   scientificName = c("Crinia Signifera", "Crinia Signifera", "Litoria peronii"),
#'   latitude = c(-35.27, -35.24, -35.83),
#'   longitude = c(149.33, 149.34, 149.34),
#'   eventDate = c("2010-10-14", "2010-10-14", "2010-10-14")
#'   )
#'
#' # Reformat columns to Darwin Core Standard terms
#' df |>
#'   set_coordinates(
#'     decimalLongitude = longitude,
#'     decimalLatitude = latitude
#'     )
#'
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
set_coordinates <- function(
    .df,
    decimalLatitude = NULL,
    decimalLongitude = NULL,
    geodeticDatum = NULL,
    coordinateUncertaintyInMeters = NULL,
    coordinatePrecision = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(coordinatePrecision, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude, geodeticDatum)
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
  check_decimalLatitude(result, level = "abort")
  check_decimalLongitude(result, level = "abort")
  check_geodeticDatum(result, level = "abort")
  check_coordinateUncertaintyInMeters(result, level = "abort")
  check_coordinatePrecision(result, level = "abort")

  return(result)
}


#' Check decimalLatitude
#' @noRd
#' @keywords Internal
check_decimalLatitude <- function(.df,
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "decimalLatitude")){
    .df |>
      select("decimalLatitude") |>
      check_is_numeric(level = level) |>
      check_within_range(level = level,
                         lower = -90,
                         upper = 90)
  }
}

#' Check decimalLongitude
#' @noRd
#' @keywords Internal
check_decimalLongitude <- function(.df,
                                   level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "decimalLongitude")){
    .df |>
      select("decimalLongitude") |>
      check_is_numeric(level = level) |>
      check_within_range(level = level,
                         lower = -180,
                         upper = 180)
  }
}

#' Check coordinateUncertaintyInMeters
#' @noRd
#' @keywords Internal
check_coordinateUncertaintyInMeters <- function(.df,
                                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "coordinateUncertaintyInMeters")){
    .df |>
      select("coordinateUncertaintyInMeters") |>
      check_is_numeric(level = level)
  }
}

#' Check coordinatePrecision
#' @noRd
#' @keywords Internal
check_coordinatePrecision <- function(.df,
                                      level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "coordinatePrecision")){
    .df |>
      select("coordinatePrecision") |>
      check_is_numeric(level = level)
  }
}

#' Check geodeticDatum
#' @noRd
#' @keywords Internal
check_geodeticDatum <- function(.df,
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "geodeticDatum")){
    .df |>
      select("geodeticDatum") |>
      check_crs(level = level)
  }
}

#' Check that Coordinate Reference System exists and is valid
#' @importFrom sf st_crs
#' @importFrom rlang try_fetch
#' @noRd
#' @keywords Internal
check_crs <- function(.df,
                      level = "warn",
                      call = caller_env()
){
  check_is_dataframe(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)

  rlang::try_fetch(
    lapply(x, function(x) sf::st_crs(x)),
    warning = function(cnd) {
      bullets <- c(
        "*" = "{.field {field_name}} contains unrecognised Coordinate Reference System (CRS)."
      ) |> cli::cli_bullets() |> cli::cli_fmt()
      cli::cli_warn(bullets, parent = cnd, call = call)
    },
    error = function(cnd) {
      bullets <- c(
        "{.field {field_name}} contains invalid Coordinate Reference System (CRS)."
      ) |> cli::cli_bullets() |> cli::cli_fmt()
      cli::cli_abort(bullets, parent = cnd, call = call)
      })
}
