library(tibble)

test_that("switch_check() works", {
  expect_error(switch_check("abort", "something"))
  expect_warning(switch_check("warn", "something"))
  expect_message(switch_check("inform", "something"))
  expect_message(switch_check()) # empty message when no args given
})

test_that("check_is_dataframe() works", {
  # case with no errors
  x <- tibble(variable = c(1, 2))
  expect_no_error(check_is_dataframe(x))
  result <- check_is_dataframe(x)
  expect_equal(x, result)
  # case with errors
  x <- list(variable = c(1, 2))
  check_is_dataframe(x) |>
    expect_error(regexp = "Must supply a \`tibble\` or \`data.frame\` to \`check_\` functions.")
  x <- tibble(variable = c(1, 2),
              something = "a_value")
  check_is_dataframe(x) |>
    expect_error(regexp = "Must supply \`data.frame\` with one column to \`check_\` functions.")
})

test_that("check_contains_values() works", {
  # case with no errors
  x <- tibble(variable = c(1, 2))
  values <- c(1, 2, 3)
  result <- check_contains_values(x, values)
  expect_no_error(check_contains_values(x, values))
  expect_equal(x, result) # `check_` returns an unmodified tibble

  # with errors
  values <- c(2, 3)
  expect_error(
    check_contains_values(x, values, level = "abort"), # TODO capture full error message
    "Unexpected value in "
    )
  expect_warning(check_contains_values(x, values, level = "warn"))
  expect_message(check_contains_values(x, values, level = "inform"))
  expect_message(check_contains_values(x, values)) # defaults to "inform"
})

# cli::test_that_cli(configs = "ascii", "check_contains_values()", {
#   testthat::local_edition(3)
#   testthat::expect_snapshot({
#     check_contains_values(x, values, level = "abort")
#   })
# })

test_that("check_is_numeric() works", {
  # case with no errors
  df_dbl <- tibble(variable = as.double(c(1, 3)))
  df_num <- tibble(variable = as.integer(c(1, 3)))
  result <- check_is_numeric(df_num)

  expect_no_error(check_is_numeric(df_dbl))
  expect_no_error(check_is_numeric(df_num))
  expect_equal(df_dbl, result)
  # with errors
  df_chr <- tibble(variable = "a_string")
  expect_error(check_is_numeric(df_chr, level = "abort")) # TODO capture full error message
  expect_warning(check_is_numeric(df_chr, level = "warn"))
  expect_message(check_is_numeric(df_chr, level = "inform"))
  expect_message(check_is_numeric(df_chr)) # defaults to inform
})

test_that("check_is_string() works", {
  # with no errors
  df_chr <- tibble(basisOfRecord = "something")
  df_dbl <- tibble(variable = as.double(c(1, 3)))
  result <- check_is_string(df_chr)

  expect_no_error(check_is_string(df_chr))
  expect_equal(df_chr, result)
  expect_error(check_is_string(df_dbl, level = "abort"), # TODO capture full error message
               " must be a character")
  expect_warning(check_is_string(df_dbl, level = "warn"))
  expect_message(check_is_string(df_dbl, level = "inform"))
  expect_message(check_is_string(df_dbl))
})

test_that("check_is_unique() works", {
  # with no errors
  values <- tibble(variable = c(1, 2, 3, 4))
  dupes_dbl <- tibble(variable = c(1, 2, 2, 3, 4, 4))
  dupes_chr <- tibble(variable = c("something", "something", "something_else"))
  result <- check_is_unique(values)

  expect_no_error(check_is_unique(values))
  expect_equal(values, result)
  expect_error( # numeric duplicates
    check_is_unique(dupes_dbl, level = "abort"),
    "Duplicate values in "
    )
  expect_error( # character duplicates
    check_is_unique(dupes_chr, level = "abort"),
    "Duplicate values in "
    )
  expect_warning(check_is_unique(dupes_chr, level = "warn")) # levels
  expect_message(check_is_unique(dupes_chr, level = "inform"))
  expect_message(check_is_unique(dupes_chr))
})

test_that("check_within_range() works", {
  # with no errors
  x <- tibble(variable = c(1, 2, 3, 4, 5))
  expect_no_error(check_within_range(x, lower = 0, upper = 20) )
  result <- check_within_range(x, lower = 0, upper = 20)
  expect_equal(x, result)
  # with numeric errors
  expect_error(check_within_range(x, level = "abort", lower = 4, upper = 20),
               "Value is outside of expected range in ")
  expect_warning(check_within_range(x, level = "warn", lower = 4, upper = 20))
  expect_message(check_within_range(x, level = "inform", lower = 4, upper = 20))
  expect_message(check_within_range(x, lower = 4, upper = 20))
  # with string errors
  x <- tibble(variable = "something")
  expect_error(check_within_range(x, level = "abort", lower = 4, upper = 20),
               regexp = " must be a numeric vector, not character.")
})


## FIXME
# current cli tests fail when load times differ between snapshots and test results
# test_that("col_progress_bar() works", {
#   withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE, CLI_NO_THREAD = "1")
#   col_name <- tibble("scientificName")
#   expect_no_error(col_progress_bar(col_name))
#   msgs <- fix_times(capture_cli_messages(cli_with_ticks(col_progress_bar(col_name))))
#   expect_snapshot(msgs)
# })

test_that("check_is_date() works", {
  df <- tibble::tibble(variable = lubridate::ymd(c("2023-10-23", "2023-11-24")))
  df_datetime <- tibble::tibble(variable = lubridate::ymd_hms(c("2023-10-23 22:10:22", "2023-11-24 08:10:00")))
  df_string <- tibble::tibble(variable = c("2023-10-23", "2023-11-24"))
  df_num <- tibble(variable = 1:2)

  expect_no_error(check_is_date(df))
  expect_no_error(check_is_date(df_datetime))
  expect_warning(
    check_is_date(df_string),
    "must be a Date vector, not a character"
    )
  expect_warning(
    check_is_date(df_num),
    "must be a Date vector, not a integer"
    )
})

test_that("check_is_date_time() works", {
  df_datetime <- tibble::tibble(variable = ymd_hms(c("2023-10-23 22:10:22", "2023-11-24 08:10:00")))
  suppressWarnings(
    df_datetime_wrong <- tibble::tibble(variable = ymd_hms(c("2023-10-23 22:10:223", "2023-11-24 08:10:00")))
  )

  expect_no_error(check_is_date_time(df_datetime))
  expect_warning(
    check_is_date(df_datetime_wrong),
    "eventDate contains invalid date/time format"
  )
})

test_that("check_is_time() works", {
  df_time <- tibble::tibble(variable = lubridate::hms(c("22:10:22", "08:10:00")))
  df_time2 <- tibble::tibble(variable = lubridate::hm(c("22:10", "08:10")))
  suppressWarnings(
    df_time_wrong <- tibble::tibble(variable = c("222:10:22333", "08:10:00"))
    )

  expect_no_error(check_is_time(df_time))
  expect_no_error(check_is_time(df_time2))
  expect_warning(
    check_is_time(df_time_wrong),
    "Invalid time format in variable"
    )
})

test_that("check_missing_all_args() works", {
  fn_call <- c("set_scientific_name", "df")
  fn_args <- c("scientificName", "taxa")
  user_cols <- c("scientificName", "bing", "Boom")
  user_cols_no_matches <- c("scientific_name", "bing", "Boom")

  expect_no_error(
    check_missing_all_args(fn_call, fn_args, user_cols)
  )

  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)
  suppressWarnings(testthat::local_reproducible_output())
  expect_warning(
    check_missing_all_args(fn_call, fn_args, user_cols_no_matches),
    "No Darwin Core terms detected by `set_scientific_name\\(\\)`" # writing it this way could cause problems?
    )
})

# test check_mismatch_code_country
# test check_word_number
