
test_that("use_abundance errors when missing .df", {
  expect_error(use_abundance(individualCount = individualCount),
               ".df is missing")
})

test_that("use_abundance errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = 23)

  expect_warning(suppressMessages(use_abundance(df)),
                 "No Darwin Core terms detected")
})

test_that("use_abundance returns tibble with updated dwc column names", {
  quiet_use_abundance <- purrr::quietly(use_abundance)
  df <- tibble::tibble(user_col = 1:2)

  result <- df |>
    quiet_use_abundance(individualCount = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("individualCount"))
})

test_that("use_abundance detects unnamed but existing dwc column names in df", {
  quiet_use_abundance <- purrr::quietly(use_abundance)
  df <- tibble::tibble(individualCount = 2:3,
                       col2 = 1:2)
  df2 <- tibble::tibble(individualCount = "borp",
                        col2 = 1:2)

  result <- df |>
    quiet_use_abundance()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("individualCount", "col2"))
  expect_error(
    suppressMessages(
      df2 |> use_abundance()
    ),
    "individualCount must be a numeric vector, not character")
})

test_that("use_abundance has progress messages", {
  quiet_use_abundance <- purrr::quietly(use_abundance)
  df <- tibble::tibble(individualCount = 1:2,
                       col2 = 1:2)

  result <- df |> quiet_use_abundance()

  expect_false(is.null(result$messages))

})

test_that("use_abundance checks individualCount format", {

  df_dbl <- tibble::tibble(individualCount = c(1, 100, 265))
  df_chr <- tibble::tibble(individualCount = c("bleep", "blorp"))

  expect_no_error(suppressMessages(
    df_dbl |> use_abundance(individualCount = individualCount)
  ))
  expect_error(suppressMessages(
    df_chr |> use_abundance(individualCount = individualCount)),
    "individualCount must be a numeric vector, not character"
  )
})

test_that("use_abundance errors if individualCount = 0 & occurrenceStatus isn't in df", {
  df <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    # occurrenceStatus = c("present", "present"),
    individualCount = c(0, 2)
  )

  expect_error(
    suppressMessages(
      df |> use_abundance(individualCount = individualCount)
    ),
    "individualCount of 0 detected"
  )
})

test_that("use_abundance errors if `individualCount = 0` and `occurrenceStatus = 'absent'` don't match", {
  df <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    occurrenceStatus = c("present", "present"),
    individualCount = c(0, 2)
  )

  expect_error(
    suppressMessages(
      df |> use_abundance(individualCount = individualCount)
    ),
    "individualCount values do not match occurrenceStatus"
  )
})

test_that("use_abundance requires organismQuantity is paired with organismQuantityType", {
  df <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    organismQuantity = c(0, 2),
    organismQuantityType = c("organism", "organism")
  )
  df_wrong <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    organismQuantity = c(0, 2)
  )

  expect_no_error(suppressMessages(
    df |> use_abundance(organismQuantity = organismQuantity)
  ))

  expect_error(
    suppressMessages(
      df_wrong |> use_abundance(organismQuantity = organismQuantity)
    ),
    "Missing organismQuantityType in dataframe"
  )
})

test_that("use_abundance requires organismQuantityType is paired with organismQuantity", {
  df <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    organismQuantity = c(0, 2),
    organismQuantityType = c("organism", "organism")
  )
  df_wrong <- tibble::tibble(
    species = c("Callocephalon fimbriatum", "Eolophus roseicapilla"),
    organismQuantityType = c("organism", "organism")
  )

  expect_no_error(suppressMessages(
    df |> use_abundance(organismQuantityType = organismQuantityType)
  ))

  expect_error(
    suppressMessages(
      df_wrong |> use_abundance(organismQuantityType = organismQuantityType)
    ),
    "Missing organismQuantity in dataframe"
  )
})
