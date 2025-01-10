no_error_check <- function(df){
  quiet_sw <- purrr::quietly(suggest_workflow)
  df |>
    quiet_sw() |>
    testthat::expect_no_error()
}

test_that("suggest_workflow() doesn't error for common use cases", {
  # simple example
  tibble(basisOfRecord = "humanObservation") |>
    no_error_check()

  # example from quick start guide
  tibble(
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
