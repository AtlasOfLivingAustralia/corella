
#---
# load small dataset of occurrence records
occs <- nanoparquet::read_parquet("testdata/bandicoots.parquet")

# set lat/lon to sf `geometry`
occs_clean <- occs |>
  tidyr::drop_na(decimalLatitude, decimalLongitude) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#---

test_that("set_coordinates_sf errors when missing .df", {
  expect_error(set_coordinates_sf(geometry = geometry),
               ".df is missing")
})

test_that("set_coordinates_sf errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = c(149.125, 149.133))

  expect_error( # extra error when sf object isn't found
    expect_warning(suppressMessages(df |> set_coordinates_sf()),
                 "No Darwin Core terms detected")
    )
})

test_that("set_coordinates_sf returns tibble with updated dwc column names", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  result <- df |>
    quiet_set_coordinates_sf(geometry = geometry)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("recordID", "decimalLongitude", "decimalLatitude", "geodeticDatum"))
})

test_that("set_coordinates_sf detects unnamed but existing `geometry` in df", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  result <- df |>
    quiet_set_coordinates_sf()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("recordID", "decimalLongitude", "decimalLatitude", "geodeticDatum"))
})

test_that("set_coordinates_sf has progress messages", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  result <- df |> quiet_set_coordinates_sf()

  expect_false(is.null(result$messages))

})

test_that("set_coordinates_sf messages when successfully converted columns", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  result <- df |> quiet_set_coordinates_sf()

  expect_contains(tail(result$messages, n = 1), "* Converted geometry > decimalLongitude, decimalLatitude, and geodeticDatum.")
})

test_that("set_coordinates_sf accepts user-renamed `geometry` column if specified", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  sf::st_geometry(df) <- "coords"
  result <- df |>
    quiet_set_coordinates_sf(geometry = coords)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("recordID", "decimalLongitude", "decimalLatitude", "geodeticDatum"))
})

test_that("set_coordinates_sf errors when wrong `geometry` column is specified", {
  df <- occs_clean |>
    dplyr::select(recordID)
  sf::st_geometry(df) <- "coords"

  expect_error(suppressMessages(
    df |>
      set_coordinates_sf(geometry = borp)
  ),
  "Must specify an existing 'geometry' column")
})

test_that("set_coordinates_sf errors when df is not an sf object", {
  df <- tibble(
    latitude = c(-35.310, -35.273),
    longitude = c(149.125, 149.133)
  )

  expect_error(suppressMessages(
    df |>
      set_coordinates_sf()
  ),
  "No geometry detected")
})

test_that("set_coordinates_sf errors when df is not an sf object", {
  sf_map <- ozmaps::ozmap_states # TODO: Replace with another sf file, or download to testthat

  expect_error(suppressMessages(
    sf_map |>
      set_coordinates_sf()
  ),
  ".df geometry must be of type 'POINT', not 'MULTIPOLYGON'")
})

test_that("set_coordinates_sf warns that geometry has been dropped from df", {
  quiet_set_coordinates_sf <- purrr::quietly(set_coordinates_sf)
  df <- occs_clean |>
    dplyr::select(recordID)

  result <- df |>
    quiet_set_coordinates_sf()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_false(is.null(result$warnings))
  expect_equal(result$warnings, "geometry dropped from data frame.")
})

# set_coordinates_sf errors when missing crs
