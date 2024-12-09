test_that("create_sequential_id() works with use_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(
    result <- input |>
      use_events(eventID = create_sequential_id())
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(as.integer(result$eventID),
               seq_len(15))
  expect_true(all(nchar(result$eventID) == 3))
})

test_that("create_sequential_id() accepts `width` argument works with use_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_events(eventID = create_sequential_id(width = 10))
  )
  expect_true(all(nchar(result$eventID) == 10))
})

test_that("create_random_id() works with use_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_events(eventID = create_random_id())
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(length(unique(result$eventID)),
               nrow(result))
})

test_that("create_composite_id() works with use_events()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_events(eventID = create_composite_id(site, eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_equal(paste0(result$site, "-", result$eventDate),
               result$eventID)
})

test_that("create_sequential_id() works within create_composite_id()", {
  input <- tibble(eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
                  basisOfRecord = "humanObservation",
                  site = rep(c("A01", "A02", "A03"), each = 5))
  suppressMessages(result <- input |>
    use_events(eventID = create_composite_id(create_sequential_id(),
                                             site,
                                             eventDate))
  )
  expect_equal(colnames(result),
               c("eventDate", "basisOfRecord", "site", "eventID"))
  expect_true(all(grepl("^[[:digit:]]{3}-", result$eventID)))
})
