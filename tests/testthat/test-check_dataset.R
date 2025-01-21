
test_that("check_dataset prints table and results", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla")
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset errors in table and results", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", 2)
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset handles multiple rows with errors", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    occurrenceStatus = c("present", "present"),
    decimalLatitude = c(-35.310, "-35.273")
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset only checks columns that match DwC terms", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    occurrenceStatus = c("present", "present"),
    decimalLatitude = c(-35.310, "-35.273"),
    longitude = c(149.125, 149.133)
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset prints a maximum of 5 error messages", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", 2),
    occurrenceStatus = c("present", "blop"),
    decimalLatitude = c(-35.310, "-35.273"),
    decimalLongitude = c(149.125, "149.133"),
    coordinatePrecision = c(.0001, ".0001"),
    genus = 1:2
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset notifies when data meets minimum Darwin Core column requirements", {
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    decimalLatitude = c(-35.310, -35.273), # deliberate error for demonstration purposes
    decimalLongitude = c(149.125, 149.133),
    eventDate = lubridate::dmy("14-01-2023", "15-01-2023"),
    occurrenceStatus = c("present", "present"),
    occurrenceID = c("d32ed0c8-d791-11ef-8000-01ff50b5e852", "d32ed0e6-d791-11ef-8000-01ff50b5e852"),
    basisOfRecord = "humanObservation",
    coordinateUncertaintyInMeters = 10,
    geodeticDatum = "WGS84"
  )

  expect_snapshot(cat(check_dataset(df)))
})

test_that("check_dataset handles `set_measurements()`", {
  df <- tibble::tibble(
    Species = c("Toechima", "Callicoma serratifolia"),
    Latitude = c(-17.1, -30.3),
    Longitude = c(146.002, 153.003),
    measurementValue = c(81.4, NA),
    measurementID = c("LMA_g.m2|1", "LMA_g.m2|2"),
    measurementUnit = "g/m2",
    measurementType = "LMA"
  ) |>
    tidyr::nest(measurementOrFact = c(measurementValue, measurementID,
                                      measurementUnit, measurementType))

  expect_snapshot(cat(check_dataset(df)))
})
