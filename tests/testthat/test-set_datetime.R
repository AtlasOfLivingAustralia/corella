library(tibble)

# create quiet function that captures side-effects
# NOTE: This must be re-run if changes are made to `set_datetime()` for bug-fixing
quiet_set_datetime <- purrr::quietly(set_datetime)

test_that("set_datetime errors when missing .df", {
  expect_error(
    set_datetime(eventDate = eventDate),
    ".df is missing")
})

test_that("set_datetime errors when no dwc columns are named, or exist in the df", {
  df <- tibble(col1 = "value")

  expect_warning(df |> set_datetime(),
               "No Darwin Core terms detected")
})

test_that("set_datetime returns tibble with updated dwc column names", {
  df <- tibble(user_col = lubridate::dmy(c("14-01-2023", "15-01-2023")))

  suppressWarnings(suppressMessages(
  result <- df |>
    set_datetime(eventDate = user_col)
  ))

  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result), c("eventDate"))
})

test_that("set_datetime detects unnamed but existing dwc column names in df", {
  df <- tibble(eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
               col2 = 1:2)
  df2 <- tibble(eventDate = "borp",
                col2 = 1:2)

  result <- df |> quiet_set_datetime()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("eventDate", "col2"))
  expect_error(
    suppressWarnings(suppressMessages(
    df2 |> set_datetime()
    )),
    "eventDate must be a Date vector"
    )
})

test_that("set_datetime has progress messages", {
  df <- tibble(eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
               col2 = 1:2)

  result <- df |> quiet_set_datetime()

  expect_false(is.null(result$messages))

})

test_that("set_datetime detects correct number of existing fields", {
  df <- tibble(eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
               col2 = 1:2)
  df2 <- tibble(eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
                year = c(2023, 2023),
                col2 = 1:2)

  result <- df |> quiet_set_datetime()
  result2 <- df2 |> quiet_set_datetime()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("eventDate", "col2"))
  expect_true(grepl("Checking 1 column: eventDate", result$messages[1]))
  expect_s3_class(result2$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result2$result), c("eventDate", "year", "col2"))
  expect_true(grepl("Checking 2 columns: eventDate and year", result2$messages[1]))

})

test_that("set_datetime checks eventDate format", {
  correct <- tibble(eventDate = lubridate::dmy(c("14-01-2023", "15-01-2023")),
                    col2 = 1:2)
  not_a_date <- tibble(eventDate = "borp",
                col2 = 1:2)
  not_a_time <- tibble(eventDate = c("14-01-2023 01", "15-01-2023 01"),
                col2 = 1:2)

  result <- correct |>
    quiet_set_datetime()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("eventDate", "col2"))
  ## TODO: The code to generate this warning is commented out in `set_datetime()`
  ## Check whether to reinstate
  # expect_warning(
  #   suppressMessages(
  #   correct |> set_datetime()
  #   ),
  #   "eventDate defaults to UTC standard"
  # )
  expect_error(
    suppressWarnings(suppressMessages(
    not_a_date |> set_datetime(eventDate = eventDate)
    )),
    "eventDate must be a Date vector"
    )
  expect_error(
    suppressWarnings(suppressMessages(
    not_a_time |> set_datetime(eventDate = eventDate)
    )),
    "eventDate must be a Date vector"
    )
})

test_that("set_datetime checks time format", {
  correct_date <- tibble(eventTime = lubridate::hms(c("10:23:00", "11:25:32")),
                    col2 = 1:2)
  correct_chr <- tibble(eventTime = c("10:23", "11:25"),
                       col2 = 1:2)
  chr_not_a_time <- tibble(eventTime = "borp",
                       col2 = 1:2)
  date_and_time <- tibble(eventTime = lubridate::dmy_hms(c("14-01-2023 01:01:01", "15-01-2023 01:01:02")),
                       col2 = 1:2)

  result1 <- correct_date |>
    quiet_set_datetime()
  result2 <- correct_chr |>
    quiet_set_datetime()

  expect_s3_class(result1$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result1$result), c("eventTime", "col2"))
  expect_type(result1$result$eventTime, "double")

  expect_s3_class(result2$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result2$result), c("eventTime", "col2"))
  expect_type(result2$result$eventTime, "character")

  expect_error(
    suppressMessages(
      chr_not_a_time |> set_datetime(eventTime = eventTime)
    ),
    "Invalid time format"
  )
  expect_error(
    suppressMessages(
      date_and_time |> set_datetime(eventTime = eventTime)
    ),
    "Must format"
  )
})

test_that("set_datetime handles correct lubridate format as character", {
  my_data <- tibble::tibble(
    eventTime = c("10H 23M", "11H 25M"),
  )

  result1 <- my_data |>
    quiet_set_datetime()

  expect_s3_class(result1$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result1$result), c("eventTime"))
  expect_type(result1$result$eventTime, "character")

  expect_no_error(
    suppressMessages(
      my_data |> set_datetime(eventTime = eventTime)
    ))
})

test_that("set_datetime checks year format", {
  correct_year <- tibble(year = c(2021, 105),
                         col2 = 1:2)
  wrong_year <- tibble(year = c(2021, 2100),
                       col2 = 1:2)
  wrong_class <- tibble(year = c("2021", "105"),
                        col2 = 1:2)

  result <- correct_year |>
    quiet_set_datetime(year = year)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("year", "col2"))
  expect_type(result$result$year, "double")

  expect_error(
    suppressMessages(
      wrong_year |> set_datetime(year = year)
    ),
    "Value is outside"
  )

  expect_error(
    suppressMessages(
      wrong_class |> set_datetime(year = year)
    ),
    "year must be a numeric"
  )
})

test_that("set_datetime checks month numeric range", {
  correct_month <- tibble(month = c(1, 11),
                         col2 = 1:2)
  wrong_month <- tibble(month = c(1, 13),
                       col2 = 1:2)

  result <- correct_month |>
    quiet_set_datetime(month = month)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("month", "col2"))
  expect_type(result$result$month, "double")

  expect_error(
    suppressMessages(
      wrong_month |> set_datetime(month = month)
    ),
    "Value is outside"
  )
})

test_that("set_datetime checks month abbreviations", {
  correct_month <- tibble(month = c("Jan", "Nov"),
                          col2 = 1:2)
  wrong_month <- tibble(month = c("Jan", "borp"),
                        col2 = 1:2)

  result <- correct_month |>
    quiet_set_datetime(month = month)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("month", "col2"))
  expect_type(result$result$month, "character")

  expect_warning(
    suppressMessages(
      wrong_month |> set_datetime(month = month)
    ),
    "month contains 1 unrecognised month"
  )
})

test_that("set_datetime checks month names", {
  correct_month <- tibble(month = c("January", "September"),
                          col2 = 1:2)
  wrong_month <- tibble(month = c("September", "borp"),
                        col2 = 1:2)

  result <- correct_month |>
    quiet_set_datetime(month = month)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("month", "col2"))
  expect_type(result$result$month, "character")

  expect_warning(
    suppressMessages(
      wrong_month |> set_datetime(month = month)
    ),
    "month contains 1 unrecognised month"
  )
})

test_that("set_datetime checks day format", {
  correct_day <- tibble(day = c(1, 30),
                          col2 = 1:2)
  wrong_day <- tibble(day = c(13, 50),
                      col2 = 1:2)
  wrong_class <- tibble(day = c("10", "20"),
                        col2 = 1:2)

  result <- correct_day |>
    quiet_set_datetime(day = day)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("day", "col2"))
  expect_type(result$result$day, "double")

  expect_error(
    suppressMessages(
      wrong_day |> set_datetime(day = day)
    ),
    "Value is outside"
  )

  expect_error(
    suppressMessages(
      wrong_class |> set_datetime(day = day)
    ),
    "day must be a numeric"
  )
})
