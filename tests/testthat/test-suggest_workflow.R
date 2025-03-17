no_error_check <- function(df){
  quiet_sw <- purrr::quietly(suggest_workflow)
  df |>
    quiet_sw() |>
    testthat::expect_no_error()
}

test_that("suggest_workflow() doesn't error for common use cases", {
  # simple example
  tibble::tibble(basisOfRecord = "humanObservation") |>
    no_error_check()

  # example from quick start guide
  tibble::tibble(
    latitude = c(-35.310, "-35.273"), # deliberate error for demonstration purposes
    longitude = c(149.125, 149.133),
    date = c("14-01-2023", "15-01-2023"),
    time = c("10:23:00", "11:25:00"),
    month = c("January", "February"),
    day = c(100, 101),
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    n = c(2, 3),
    crs = c("WGS84", "WGS8d"),
    country = c("Australia", "Denmark"),
    continent = c("Oceania", "Europe")
  ) |>
    no_error_check()
})

test_that("suggest_workflow prints table and results", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
  suppressWarnings(testthat::local_reproducible_output())
  df <- tibble::tibble(
    decimalLatitude = c(-35.310, -35.273),
    decimalLongitude = c(149.125, 149.133),
    date = c("14-01-2023", "15-01-2023"),
    time = c("10:23:00", "11:25:00"),
    month = c("January", "February"),
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    n = c(2, 3),
    country = c("Australia", "Denmark"),
    continent = c("Oceania", "Europe")
  )

  msgs <- fix_times(capture_cli_messages(suggest_workflow(df)))
  expect_snapshot(msgs)
})

test_that("suggest_workflow celebrates when data meets Darwin Core Standard", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
  suppressWarnings(testthat::local_reproducible_output())
  df <- tibble::tibble(
    decimalLatitude = c(-35.310, -35.273),
    decimalLongitude = c(149.125, 149.133),
    eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    basisOfRecord = "humanObservation",
    geodeticDatum = "WGS84",
    coordinateUncertaintyInMeters = "10",
    occurrenceID = c("92f5f704-02dc-11f0-8000-33817db453ab",
                     "92f5f70e-02dc-11f0-8000-33817db453ab")
  )

  msgs <- fix_emojis(fix_times(capture_cli_messages(suggest_workflow(df))))
  expect_snapshot(msgs)
})

# test_that("basisOfRecord() works", {
#   x <- tibble(x = 1)
#   # no error
#   x |>
#     basisOfRecord("humanObservation") |>
#     expect_no_error()
#   result <- basisOfRecord(x, "humanObservation")
#   expect_s3_class(result, c("tbl_df", "tbl", "data.frame")) # dwc_df case?
#   expect_equal(ncol(result), 2)
#   expect_equal(colnames(result), c("x", "basisOfRecord"))
#   # with error
#   x |>
#     basisOfRecord("something") |>
#     expect_error(regexp = "basisOfRecord")
#   # ideally this would check for `Error in check_basisOfRecord()`;
#   # but this isn't a message, so it doesn't work. May need to snapshot?
# })
