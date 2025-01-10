test_that("set_events() keeps fields when composite_id() is called, but only for occurrenceID fields", {
  quiet_set_events <- purrr::quietly(set_events)
  df <- tibble(obs_type = "survey",
               site = seq_len(10),
               year = 2024)
  result <- df |>
    quiet_set_events(eventID = composite_id(sequential_id(),
                                            site,
                                            year),
               eventType = obs_type)
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result),
               c("site", "year", "eventID", "eventType"))
  # i.e. components of `eventID` are retained, but `obs_type` is not
})

test_that("setting .keep_composite = 'unused' affects set_occurrences()", {
  quiet_set_events <- purrr::quietly(set_events)
  df <- tibble(user_col = "humanObservation",
               site = seq_len(10),
               year = 2024)
  df <- tibble(obs_type = "survey",
               site = seq_len(10),
               year = 2024)
  result <- df |>
    quiet_set_events(eventID = composite_id(sequential_id(),
                                            site,
                                            year),
                     eventType = obs_type,
                     .keep_composite = "unused")
  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result),
               c("eventID", "eventType"))
})

test_that("sequential_id() works with set_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(
    result <- input |>
      set_events(eventID = sequential_id())
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(as.integer(result$eventID),
               seq_len(15))
  expect_true(all(nchar(result$eventID) == 3))
})

test_that("sequential_id() accepts `width` argument works with set_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    set_events(eventID = sequential_id(width = 10))
  )
  expect_true(all(nchar(result$eventID) == 10))
})

test_that("random_id() works with set_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    set_events(eventID = random_id()))

  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(length(unique(result$eventID)),
               nrow(result))
})

test_that("composite_id() works with set_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    set_events(eventID = composite_id(site, eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(paste0(result$site, "-", result$eventDate),
               result$eventID)
})

test_that("sequential_id() works within composite_id()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    set_events(eventID = composite_id(sequential_id(),
                                      site,
                                      eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_true(all(grepl("^[[:digit:]]{3}-", result$eventID)))
})
