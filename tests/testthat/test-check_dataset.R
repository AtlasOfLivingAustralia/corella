
no_error_check <- function(df){
  quiet_cd <- purrr::quietly(check_dataset)
  df |>
    quiet_cd() |>
    testthat::expect_no_error()
}

test_that("check_dataset() doesn't error for common use cases", {
  # simple example
  tibble::tibble(basisOfRecord = "humanObservation") |>
    no_error_check()

  # example from quick start guide
  tibble::tibble(
    latitude = c(-35.310, -35.273),
    longitude = c(149.125, 149.133),
    date = c("14-01-2023", "15-01-2023"),
    month = c("January", "February"),
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    n = c(2, 3),
    geodeticDatum = c("WGS84", "WGS84"),
    country = c("Australia", "Denmark"),
    continent = c("Oceania", "Europe")
  ) |>
    no_error_check()
})

# NOTE: This snapshot solution to testing works, but it prints some messages to
#       the console.
#       There is another solution implemented by {cli} which is used at the
#       bottom of this test script.

test_that("check_dataset prints table and results", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
  suppressWarnings(testthat::local_reproducible_output())
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla")
  )

  msgs <- fix_times(capture_cli_messages(check_dataset(df)))
  expect_snapshot(msgs)
})

test_that("check_dataset errors in table and results", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
  df <- tibble::tibble(
    scientificName = c("Callocephalon fimbriatum", 2)
  )

  msgs <- fix_times(capture_cli_messages(check_dataset(df)))
  expect_snapshot(msgs)
})

test_that("check_dataset notifies when data meets minimum Darwin Core column requirements", {
  skip_on_cran()
  skip_on_ci()
  suppressWarnings(testthat::local_reproducible_output())
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
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

  msgs <- fix_times(capture_cli_messages(check_dataset(df)))
  expect_snapshot(msgs)
})

test_that("check_dataset handles `set_measurements()`", {
  skip_on_cran()
  skip_on_ci()
  suppressWarnings(testthat::local_reproducible_output())
  withr::local_options(cli.dynamic = TRUE,
                       cli.ansi = TRUE,
                       cli.unicode = TRUE,
                       cli.width = 80,
                       width = 80,
                       cli.num_colors = 1)
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

  msgs <- fix_times(capture_cli_messages(check_dataset(df)))
  expect_snapshot(msgs)
})




### FIXME: These tests once worked but now always fail. They use an alternative
#          test structure using callr::r(), implmented by cli for their own
#          tests.
#          They are worth fixing because including these tests brings us to
#          100% test coverage


### Alternative test structure: ###
# NOTES:
#
# - This works too, but callr cannot use `load_all()` for some reason,
#   so we expect this will fail on CRAN
# - To make it run, a test version must be installed locally (using devtools)
# - I don't know what the app does, but Jenny Bryan suggests adding it to any
#   cli test that calls cli and has expections about the cli output
#   See: https://github.com/r-lib/cli/issues/210


# cli::start_app()
# on.exit(cli::stop_app(), add = TRUE)
#
# test_that("check_dataset handles multiple rows with errors", {
#   skip_on_cran()
#   suppressWarnings(testthat::local_reproducible_output())
#   fun <- function() {
#     withr::local_options(cli.dynamic = TRUE,
#                          cli.ansi = TRUE,
#                          cli.unicode = TRUE,
#                          cli.width = 80,
#                          width = 80,
#                          cli.num_colors = 1
#                          )
#     df <- tibble::tibble(
#       scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
#       occurrenceStatus = c("present", "present"),
#       decimalLatitude = c(-35.310, "-35.273")
#     )
#     corella::check_dataset(df)
#   }
#
#   outfile <- tempfile()
#   on.exit(unlink(outfile), add = TRUE)
#   callr::r(fun, stdout = outfile, stderr = outfile)
#   expect_snapshot(fix_emojis(fix_times(readLines(outfile)))) # this shouldn't have emojis
# })
#
# test_that("check_dataset only checks columns that match DwC terms", {
#   skip_on_cran()
#   suppressWarnings(testthat::local_reproducible_output())
#   fun <- function() {
#     withr::local_options(cli.dynamic = TRUE,
#                          cli.ansi = TRUE,
#                          cli.unicode = TRUE,
#                          cli.width = 80,
#                          width = 80,
#                          cli.num_colors = 1
#                          )
#     df <- tibble::tibble(
#       scientificName = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
#       occurrenceStatus = c("present", "present"),
#       decimalLatitude = c(-35.310, "-35.273"),
#       longitude = c(149.125, 149.133)
#     )
#     corella::check_dataset(df)
#   }
#
#   outfile <- tempfile()
#   on.exit(unlink(outfile), add = TRUE)
#   callr::r(fun, stdout = outfile, stderr = outfile)
#   expect_snapshot(fix_emojis(fix_times(readLines(outfile))))
# })
#
# test_that("check_dataset prints a maximum of 5 error messages", {
#   skip_on_cran()
#   suppressWarnings(testthat::local_reproducible_output())
#     fun <- function() {
#   withr::local_options(cli.dynamic = TRUE,
#                        cli.ansi = TRUE,
#                        cli.unicode = TRUE,
#                        cli.width = 80,
#                        width = 80,
#                        cli.num_colors = 1,
#                        cli.progress_show_after = 0,
#                        cli.progress_handlers_only = "logger")
#   df <- tibble::tibble(
#     scientificName = c("Callocephalon fimbriatum", 2),
#     occurrenceStatus = c("present", "blop"),
#     decimalLatitude = c(-35.310, "-35.273"),
#     decimalLongitude = c(149.125, "149.133"),
#     coordinatePrecision = c(.0001, ".0001"),
#     genus = 1:2
#   )
#   corella::check_dataset(df)
#     }
#
#   outfile <- tempfile()
#   on.exit(unlink(outfile), add = TRUE)
#   callr::r(fun, stdout = outfile, stderr = outfile)
#   expect_snapshot(fix_logger_output(fix_emojis(fix_times(readLines(outfile))))) # this shouldn't have emojis
# })



