library(tibble)

test_that("use_occurrences errors when missing .df", {
  expect_error(use_occurrences(basisOfRecord = basisOfRecord),
               ".df is missing")
})

test_that("use_occurrences errors when no dwc columns are named, or exist in the df", {
  df <- tibble(borp = "humanObservation")

  expect_warning(suppressMessages(use_occurrences(df)),
                 "No Darwin Core terms detected")
})

test_that("use_occurrences returns tibble with updated dwc column names", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(user_col = "humanObservation")

  result <- df |>
    quiet_use_occurrences(basisOfRecord = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("basisOfRecord"))
})

test_that("use_occurrences detects unnamed but existing dwc column names in df", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)
  df2 <- tibble(basisOfRecord = "borp",
                col2 = 1:2)

  result <- df |>
    quiet_use_occurrences()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("basisOfRecord", "col2"))
  expect_error(
    suppressMessages(
      df2 |> use_occurrences()
      ),
    "Unexpected value in basisOfRecord")
})

test_that("use_occurrences has progress messages", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  df <- tibble(basisOfRecord = "humanObservation",
               col2 = 1:2)

  result <- df |> quiet_use_occurrences()

  expect_false(is.null(result$messages))

})

test_that("use_occurrences only accepts valid values for basisOfRecord", {
  valid_values <- c("humanObservation", "machineObservation", "livingSpecimen",
                       "preservedSpecimen", "fossilSpecimen", "materialCitation")

  df_right <- tibble(basisOfRecord = valid_values)
  df_wrong <- tibble(basisOfRecord = c(valid_values, "blop"))

  expect_no_error(suppressMessages(
    df_right |> use_occurrences(basisOfRecord = basisOfRecord)
    ))
  expect_error(suppressMessages(
    df_wrong |> use_occurrences(basisOfRecord = basisOfRecord)),
    "Unexpected value in basisOfRecord"
    )
  expect_error(suppressMessages(
    df_wrong |> use_occurrences(basisOfRecord = 3)),
    "basisOfRecord must be a character vector, not numeric"
    )
})

test_that("use_occurrences checks occurrenceStatus format", {
  quiet_use_occurrences <- purrr::quietly(use_occurrences)
  valid_values <- c("present", "absent")
  df <- tibble(occurrenceStatus = valid_values)
  df_wrong_class <- tibble(occurrenceStatus = c(1, 2))
  df_wrong_name <- tibble(occurrenceStatus = c(valid_values, "blop"))

  result <- df |>
    quiet_use_occurrences(occurrenceStatus = occurrenceStatus)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("occurrenceStatus"))
  expect_type(result$result$occurrenceStatus, "character")

  expect_error(
    suppressMessages(
      df_wrong_class |> use_occurrences(occurrenceStatus = occurrenceStatus)
    ),
    "occurrenceStatus must be a character vector, not numeric"
  )

  expect_error(
    suppressMessages(
      df_wrong_name |> use_occurrences(occurrenceStatus = occurrenceStatus)
    ),
    "Unexpected value in occurrenceStatus"
  )
})

test_that("create_sequential_id() works with use_occurrences()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_occurrences(occurrenceID = create_sequential_id())
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "occurrenceID"))
  expect_equal(as.integer(result$occurrenceID),
               seq_len(15))
  expect_true(all(nchar(result$occurrenceID) == 3))
})

test_that("create_sequential_id() accepts `width` argument with use_occurrences()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_occurrences(occurrenceID = create_sequential_id(width = 10))
  )
  expect_true(all(nchar(result$occurrenceID) == 10))
})

test_that("create_random_id() works with use_occurrences()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_occurrences(occurrenceID = create_random_id())
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "occurrenceID"))
  expect_equal(length(unique(result$occurrenceID)),
               nrow(result))
})

test_that("create_composite_id() works with use_occurrences()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_occurrences(occurrenceID = create_composite_id(site, eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "occurrenceID"))
  expect_equal(paste0(result$site, "-", result$eventDate),
               result$occurrenceID)
})

test_that("create_sequential_id() works within create_composite_id()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_occurrences(occurrenceID = create_composite_id(create_sequential_id(),
                                                       site,
                                                       eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "occurrenceID"))
  expect_true(all(grepl("^[[:digit:]]{3}-", result$occurrenceID)))
})
