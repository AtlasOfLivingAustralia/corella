test_that("darwin_core_terms loads properly", {
  x <- corella::darwin_core_terms
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(x), c(208, 7)) # with individualID
  expect_equal(colnames(x),
               c("class", "term", "url", "definition", "comments", "examples", "set_function"))
})

test_that("country_codes loads properly", {
  x <- corella::country_codes
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(x), c(249, 3))
  expect_equal(colnames(x),
               c("name", "code", "year_added"))
})

test_that("occurrence_terms() works as expected", {
  df <- tibble::tibble(
    x = seq_len(3),
    eventID = c(5:7),
    decimalLatitude = c(151:153),
    decimalLongitude = c(-45:-47),
    basisOfRecord = "humanObservation",
    kingdom = "Animalia",
    class = "Amphibia",
    scientificName = c("Litoria", "Crinia", "Uperoleia"),
    y = 12,
    biscuits = "chocolate_chip")
  # Try for occurrences
  result <- df |>
    select(any_of(occurrence_terms()))
  expect_equal(colnames(result),
               c("basisOfRecord",
                 "eventID",
                 "decimalLatitude",
                 "decimalLongitude",
                 "scientificName",
                 "kingdom",
                 "class"))
  # note order of `occurrence_terms()` is preserved, not order of input
  # try for `event_terms()`
  result2 <- df |>
    select(any_of(event_terms()))
  expect_equal(colnames(result2),
               c("basisOfRecord",
                 "eventID",
                 "decimalLatitude",
                 "decimalLongitude"))
})

