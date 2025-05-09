#' Set, create or modify columns with `sf` spatial information
#'
#' @description
#' This function helps format standard location fields like longitude and
#' latitude point coordinates to a `tibble` using Darwin Core Standard.
#'
#' It differs from `set_coordinates()` by accepting `sf` geometry columns of
#' class `POINT`as coordinates (rather than `numeric` lat/lon coordinates).
#' The advantage
#' of using an `sf` geometry is that the Coordinate Reference System (CRS) is
#' automatically formatted into the required `geodeticDatum` column.
#'
#' @param .df A `data.frame` or `tibble` that the column should be appended to.
#' @param geometry The latitude/longitude coordinates as `sf` `POINT` class
#' @param .keep Control which columns from .data are retained in the output.
#' Note that unlike [dplyr::mutate()], which defaults to `"all"` this defaults to
#' `"unused"`; i.e. only keeps Darwin Core columns, and not those columns used to
#' generate them.
#' @returns A `tibble` with the requested columns added/reformatted.
#' @seealso [set_coordinates()] for providing numeric coordinates,
#' [set_locality()] for providing text-based spatial information.
#' @examples
#' df <- tibble::tibble(
#'   scientificName = c("Crinia Signifera", "Crinia Signifera", "Litoria peronii"),
#'   latitude = c(-35.27, -35.24, -35.83),
#'   longitude = c(149.33, 149.34, 149.34),
#'   eventDate = c("2010-10-14", "2010-10-14", "2010-10-14")
#'   ) |>
#'   sf::st_as_sf(coords = c("longitude", "latitude")) |>
#'   sf::st_set_crs(4326)
#'
#' # Reformat columns to Darwin Core Standard terms.
#' # Coordinates and CRS are automatically detected and reformatted.
#' df |>
#'   set_coordinates_sf()
#'
#'
#' @importFrom rlang abort
#' @importFrom rlang get_expr
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_geometry_type
#' @importFrom cli cli_warn
#' @importFrom cli cli_abort
#' @export
set_coordinates_sf <- function(
    .df,
    geometry = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort(".df is missing, with no default.")
  }

  fn_args <- ls()

  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(geometry)
  names(fn_quos) <- fn_args

  fn_quo_is_null <- fn_quos |>
    map(.f = rlang::quo_is_null) |>
    unlist()

  # detect sf and handle sf objects
  if (!inherits(.df, "sf")) {
    bullets <- c(
      "No geometry detected.",
      i = "Must supply {.code set_coordinates_sf()} a dataframe with an {.pkg sf} geometry (i.e. {.code st_POINT})."
    ) |> cli_bullets() |> cli_fmt()
    cli_abort(bullets)
  } else {

    # enforce POINT geometry
    if (any(st_geometry_type(.df, by_geometry = FALSE) != "POINT")) {
      sf_type <- st_geometry_type(.df, by_geometry = FALSE)
      cli_abort(".df geometry must be of type 'POINT', not '{sf_type}'.")

    } else {

      # if geometry arg has been named, save the name
      if(!any(fn_quo_is_null)) {

        col_name_sfc <- paste0(get_expr(fn_quos$geometry)) # save name

        # check if column name is in the dataframe
        if(!col_name_sfc %in% colnames(.df)) {
          bullets <- c(
            "Must specify an existing 'geometry' column.",
            x = "Column '{col_name_sfc}' doesn't exist."
          ) |> cli_bullets() |> cli_fmt()

          cli_abort(bullets)
        }

      } else {

        # get column name that holds 'geometry'
        col_name_sfc <- .df |>
          select(which(sapply(.df, class) == 'sfc_POINT')) |> # might be overcomplicating `select(geometry)`
          colnames()
      }
    }
  }

  # inform user which columns will be checked
  matched_cols <- col_name_sfc
  if(length(matched_cols > 0)) {
    col_progress_bar(cols = matched_cols)
  }

  # run column checks
  # Add sf coords if valid
  check_coords(.df, level = "abort")

  result <- col_sf_to_dwc(.df,
                          col_name_sfc
                          # level = level
                          ) |>
    st_drop_geometry()

  cli_warn("{.field {col_name_sfc}} dropped from data frame.")

  result
}

#' Check coordinates supplied by an sfc_POINT object
#' @noRd
#' @keywords Internal
check_coords <- function(.df,
                         level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
    .df |>
      select(which(sapply(.df, class) == 'sfc_POINT')) |>
      check_is_sf(level = level) |>
      check_is_point(level = level) |>
      check_has_crs(level = level)
}

#' Check whether an object is of class `sf`
#' @importFrom sf st_is
#' @importFrom sf st_geometry
#' @noRd
#' @keywords Internal
check_is_sf <- function(.df,
                          level = c("inform", "warn", "abort"),
                          call = caller_env()
){
  check_is_dataframe(.df)
  # field_name <- colnames(.df)[[1]]
  # x <- .df |> pull(field_name)
  if (!inherits(.df, "sf")) {
    bullets <- c(
      "No geometry detected.",
      i = "Must supply {.code set_coordinates_sf()} a dataframe with an {.pkg sf} geometry (i.e. {.code st_POINT})."
    ) |> cli_bullets() |> cli_fmt()
    cli_abort(bullets) # FIXME: this ignores 'level' argument
  }
  .df
}

#' @noRd
#' @importFrom sf st_is
#' @importFrom sf st_geometry
#' @keywords Internal
check_is_point <- function(.df,
                        level = c("inform", "warn", "abort"),
                        call = caller_env()
){
  check_is_dataframe(.df)
  # enforce POINT geometry
  if (any(st_geometry_type(.df, by_geometry = FALSE) != "POINT")) {
    sf_type <- st_geometry_type(.df, by_geometry = FALSE)
    cli_abort(".df geometry must be of type 'POINT', not '{sf_type}'.") # FIXME: this ignores 'level' argument
  }
  .df
}

#' Check whether dataframe has a CRS
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @noRd
#' @keywords Internal
check_has_crs <- function(.df,
                          level = c("inform", "warn", "abort"),
                          call = caller_env()
){
  check_is_dataframe(.df)
  if(is.na(st_crs(.df))){

    bullets <- cli_bullets(c(
      "Missing Coordinate Reference System (CRS).",
      i = ".df must have CRS. See {.code ?sf::st_crs}."
    )) |>
      cli_fmt()

    switch_check(level,
                 bullets,
                 call = call)
  }
  .df
}

#' Convert sf `geometry` to valid Darwin Core columns
#' @importFrom sf st_coordinates
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @noRd
#' @keywords Internal
col_sf_to_dwc <- function(.df,
                          col_name,
                          # level = c("inform", "warn", "abort"), # unused
                          call = caller_env()
                          ){
  result <- .df |>
    mutate(
      decimalLongitude = sf::st_coordinates(.df)[,1],
      decimalLatitude = sf::st_coordinates(.df)[,2],
      geodeticDatum = sf::st_crs(.df)$input
      )

  new_cols <- colnames(result)[colnames(result) %in% c("decimalLongitude", "decimalLatitude", "geodeticDatum")]
  bullets <- c(
    "*" = "Converted {cli::col_cyan(paste('geometry'))} {symbol$arrow_right} {.field {new_cols}}."
    ) |> cli_bullets() |> cli_fmt()

  cli_inform(bullets)
  return(result)

}
