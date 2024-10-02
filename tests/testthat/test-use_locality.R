
test_that("use_locality errors when missing .df", {
  expect_error(use_locality(continent = continent),
               ".df is missing")
})

test_that("use_locality errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "Oceania")

  expect_warning(suppressMessages(use_locality(df)),
                 "No Darwin Core terms detected")
})

test_that("use_locality returns tibble with updated dwc column names", {
  quiet_use_locality <- purrr::quietly(use_locality)
  df <- tibble::tibble(user_col = "New South Wales")

  result <- df |>
    quiet_use_locality(stateProvince = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("stateProvince"))
})

test_that("use_locality detects unnamed but existing dwc column names in df", {
  quiet_use_locality <- purrr::quietly(use_locality)
  df <- tibble::tibble(continent = "Oceania",
               col2 = 1:2)
  df2 <- tibble::tibble(continent = "borp",
                col2 = 1:2)

  result <- df |>
    quiet_use_locality()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("continent", "col2"))
  expect_error(
    suppressMessages(
      df2 |> use_locality()
    ),
    "Unexpected value in continent")
})

test_that("use_locality has progress messages", {
  quiet_use_locality <- purrr::quietly(use_locality)
  df <- tibble::tibble(stateProvince = "Victoria",
               col2 = 1:2)

  result <- df |> quiet_use_locality()

  expect_false(is.null(result$messages))

})

test_that("use_locality only accepts valid values for continent", {
  valid_values <- c("Africa", "Antarctica", "Asia", "Europe", "North America", "Oceania", "South America")

  df_right <- tibble::tibble(continent = valid_values)
  df_wrong <- tibble::tibble(continent = c(valid_values, "blop"))
  df_num <- tibble::tibble(continent = 1:3)

  expect_no_error(suppressMessages(
    df_right |> use_locality(continent = continent)
  ))
  expect_error(suppressMessages(
    df_wrong |> use_locality(continent = continent)),
    "Unexpected value in continent"
  )
  expect_error(suppressMessages(
    df_num |> use_locality(continent = continent)),
    "continent must be a character vector, not integer"
  )
})


test_that("use_locality only accepts valid values for country", {
  valid_values <- c("Australia", "United States of America", "New Zealand")

  df_right <- tibble::tibble(country = valid_values)
  df_wrong <- tibble::tibble(country = c(valid_values, "blop"))

  expect_no_error(suppressMessages(
    df_right |> use_locality(country = country)
  ))
  expect_error(suppressMessages(
    df_wrong |> use_locality(country = country)),
    "Unexpected value in country"
  )
  expect_error(suppressMessages(
    df_wrong |> use_locality(country = 3)),
    "country must be a character vector, not numeric"
  )
})

test_that("use_locality only accepts valid values for country", {
  # subset of common values
  valid_values <- c("Australia", "United States of America", "New Zealand", "Indonesia")

  df_right <- tibble::tibble(country = valid_values)
  df_wrong <- tibble::tibble(country = c(valid_values, "blop"))

  expect_no_error(suppressMessages(
    df_right |> use_locality(country = country)
  ))
  expect_error(suppressMessages(
    df_wrong |> use_locality(country = country)),
    "Unexpected value in country"
  )
  expect_error(suppressMessages(
    df_wrong |> use_locality(country = 3)),
    "country must be a character vector, not numeric"
  )
})

test_that("use_locality only accepts valid values for countryCode", {
  # subset of common values
  valid_values <- c("AU", "US", "NZ", "JP", "ID")

  df_right <- tibble::tibble(countryCode = valid_values)
  df_wrong <- tibble::tibble(countryCode = c(valid_values, "blop"))

  expect_no_error(suppressMessages(
    df_right |> use_locality(countryCode = countryCode)
  ))
  expect_error(suppressMessages(
    df_wrong |> use_locality(countryCode = countryCode)),
    "Unexpected value in countryCode"
  )
  expect_error(suppressMessages(
    df_wrong |> use_locality(countryCode = 3)),
    "countryCode must be a character vector, not numeric"
  )
})

test_that("use_locality checks stateProvince format", {

  df_chr <- tibble::tibble(stateProvince = c("Victoria", "Perth"))
  df_dbl <- tibble::tibble(stateProvince =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_locality(stateProvince = stateProvince)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_locality(stateProvince = stateProvince)),
    "stateProvince must be a character vector, not integer"
  )
})

test_that("use_locality checks locality format", {

  df_chr <- tibble::tibble(locality = c("a place", "another place"))
  df_dbl <- tibble::tibble(locality =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_locality(locality = locality)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_locality(locality = locality)),
    "locality must be a character vector, not integer"
  )
})
