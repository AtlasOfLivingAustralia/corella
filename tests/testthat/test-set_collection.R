
test_that("set_collection errors when missing .df", {
  expect_error(set_collection(datasetID = datasetID),
               ".df is missing")
})

test_that("set_collection errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "anID")

  expect_warning(suppressMessages(set_collection(df)),
                 "No Darwin Core terms detected")
})

test_that("set_collection returns tibble with updated dwc column names", {
  quiet_set_collection <- purrr::quietly(set_collection)
  df <- tibble::tibble(user_col = "FrogID")

  result <- df |>
    quiet_set_collection(datasetName = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("datasetName"))
})

test_that("set_collection detects unnamed but existing dwc column names in df", {
  quiet_set_collection <- purrr::quietly(set_collection)
  df <- tibble::tibble(datasetName = "FrogID",
                       col2 = 1:2)
  df2 <- tibble::tibble(datasetName = 1:2,
                        col2 = 1:2)

  result <- df |>
    quiet_set_collection()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("datasetName", "col2"))
  expect_error(
    suppressMessages(
      df2 |> set_collection()
    ),
    "datasetName must be a character vector, not integer")
})

test_that("set_collection has progress messages", {
  quiet_set_collection <- purrr::quietly(set_collection)
  df <- tibble::tibble(datasetName = "FrogID",
                       col2 = 1:2)

  result <- df |> quiet_set_collection()

  expect_false(is.null(result$messages))

})


test_that("set_collection checks datasetName format", {

  df_chr <- tibble::tibble(datasetName = c("FrogID", "iNaturalist observations"))
  df_dbl <- tibble::tibble(datasetName =1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_collection(datasetName = datasetName)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_collection(datasetName = datasetName)),
    "datasetName must be a character vector, not integer"
  )
})


