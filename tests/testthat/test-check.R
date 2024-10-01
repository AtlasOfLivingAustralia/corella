library(tibble)

test_that("switch_check() works", {
  expect_error(switch_check("abort", "something"))
  expect_warning(switch_check("warn", "something"))
  expect_message(switch_check("inform", "something"))
  expect_message(switch_check()) # empty message when no args given
})

test_that("check_data_frame() works", {
  # case with no errors
  x <- tibble(variable = c(1, 2))
  expect_no_error(check_data_frame(x))
  result <- check_data_frame(x)
  expect_equal(x, result)
  # case with errors
  x <- list(variable = c(1, 2))
  check_data_frame(x) |>
    expect_error(regexp = "Must supply a \`tibble\` or \`data.frame\` to \`check_\` functions.")
  x <- tibble(variable = c(1, 2),
              something = "a_value")
  check_data_frame(x) |>
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
  expect_warning(check_is_string(x, level = "warn"))
  expect_message(check_is_string(x, level = "inform"))
  expect_message(check_is_string(x))
})

test_that("check_unique() works", {
  # with no errors
  values <- tibble(variable = c(1, 2, 3, 4))
  dupes_dbl <- tibble(variable = c(1, 2, 2, 3, 4, 4))
  dupes_chr <- tibble(variable = c("something", "something", "something_else"))
  result <- check_unique(values)

  expect_no_error(check_unique(values))
  expect_equal(values, result)
  expect_error( # numeric duplicates
    check_unique(dupes_dbl, level = "abort"),
    "Duplicate values in "
    )
  expect_error( # character duplicates
    check_unique(dupes_chr, level = "abort"),
    "Duplicate values in "
    )
  expect_warning(check_unique(dupes_chr, level = "warn")) # levels
  expect_message(check_unique(dupes_chr, level = "inform"))
  expect_message(check_unique(dupes_chr))
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
