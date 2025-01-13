
test_that("set_observer errors when missing .df", {
  expect_error(set_observer(recordedBy = recordedBy),
               ".df is missing")
})

test_that("set_observer errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = c("Generic Name", "Generic Name"))

  expect_warning(suppressMessages(set_observer(df)),
                 "No Darwin Core terms detected")
})

test_that("set_observer returns tibble with updated dwc column names", {
  quiet_set_observer <- purrr::quietly(set_observer)
  df <- tibble::tibble(user_col = "New South Wales")

  result <- df |>
    quiet_set_observer(recordedBy = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("recordedBy"))
})

test_that("set_observer detects unnamed but existing dwc column names in df", {
  quiet_set_observer <- purrr::quietly(set_observer)
  df <- tibble::tibble(recordedBy = "Oceania",
                       col2 = 1:2)
  df2 <- tibble::tibble(recordedBy = 1:2,
                        col2 = 1:2)

  result <- df |>
    quiet_set_observer()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("recordedBy", "col2"))
  expect_error(
    suppressMessages(
      df2 |> set_observer()
    ),
    "recordedBy must be a character vector, not integer")
})

test_that("set_observer has progress messages", {
  quiet_set_observer <- purrr::quietly(set_observer)
  df <- tibble::tibble(recordedBy = "Victoria",
                       col2 = 1:2)

  result <- df |> quiet_set_observer()

  expect_false(is.null(result$messages))

})

test_that("set_observer checks recordedBy format", {

  df_chr <- tibble::tibble(recordedBy = c("Generic Name", "Generic Name 2"))
  df_dbl <- tibble::tibble(recordedBy = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_observer(recordedBy = recordedBy)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_observer(recordedBy = recordedBy)),
    "recordedBy must be a character vector, not integer"
  )
})

test_that("set_observer checks recordedByID format", {

  df_chr <- tibble::tibble(recordedByID = c("https://orcid.org/0000-0002-1825-0097", "https://orcid.org/0000-0002-1825-0098"))
  df_dbl <- tibble::tibble(recordedByID = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_observer(recordedByID = recordedByID)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_observer(recordedByID = recordedByID)),
    "recordedByID must be a character vector, not integer"
  )
})
