
test_that("set_individual_traits errors when missing .df", {
  expect_error(set_individual_traits(individualID = individualID),
               ".df is missing")
})

test_that("set_individual_traits errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "Oceania")

  expect_warning(suppressMessages(set_individual_traits(df)),
                 "No Darwin Core terms detected")
})

test_that("set_individual_traits returns tibble with updated dwc column names", {
  quiet_set_individual_traits <- purrr::quietly(set_individual_traits)
  df <- tibble::tibble(user_col = "thisIsAnID")

  result <- df |>
    quiet_set_individual_traits(individualID = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("individualID"))
})

test_that("set_individual_traits detects unnamed but existing dwc column names in df", {
  quiet_set_individual_traits <- purrr::quietly(set_individual_traits)
  df <- tibble::tibble(lifeStage = "zygote",
                       col2 = 1:2)

  result <- df |>
    quiet_set_individual_traits()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("lifeStage", "col2"))
})

test_that("set_individual_traits has progress messages", {
  quiet_set_individual_traits <- purrr::quietly(set_individual_traits)
  df <- tibble::tibble(lifeStage = "zygote",
                       col2 = 1:2)

  result <- df |> quiet_set_individual_traits()

  expect_false(is.null(result$messages))

})

test_that("set_individual_traits checks individualID format", {

  df <- tibble::tibble(individualID = c("thisIsAnID", "thisIsAnDifferentID"))
  df_dupes <- tibble::tibble(individualID = c("thisIsAnID", "thisIsAnID"))

  expect_no_error(suppressMessages(
    df |> set_individual_traits(individualID = individualID)
  ))
  expect_error(suppressMessages(
    df_dupes |> set_individual_traits(individualID = individualID)),
    "Duplicate values in individualID"
  )
})

test_that("set_individual_traits checks lifeStage format", {

  df_chr <- tibble::tibble(lifeStage = c("zygote", "seedling"))
  df_dbl <- tibble::tibble(lifeStage = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_individual_traits(lifeStage = lifeStage)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_individual_traits(lifeStage = lifeStage)),
    "lifeStage must be a character vector, not integer"
  )
})

test_that("set_individual_traits checks sex format", {

  df_chr <- tibble::tibble(sex = c("male", "female"))
  df_dbl <- tibble::tibble(sex = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_individual_traits(sex = sex)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_individual_traits(sex = sex)),
    "sex must be a character vector, not integer"
  )
})

test_that("set_individual_traits checks sex format", {

  df_chr <- tibble::tibble(sex = c("male", "female"))
  df_dbl <- tibble::tibble(sex = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_individual_traits(sex = sex)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_individual_traits(sex = sex)),
    "sex must be a character vector, not integer"
  )
})

test_that("set_individual_traits checks vitality format", {

  df_chr <- tibble::tibble(vitality = c("alive", "dead"))
  df_dbl <- tibble::tibble(vitality = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_individual_traits(vitality = vitality)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_individual_traits(vitality = vitality)),
    "vitality must be a character vector, not integer"
  )
})

test_that("set_individual_traits checks reproductiveCondition format", {

  df_chr <- tibble::tibble(reproductiveCondition = c("alive", "dead"))
  df_dbl <- tibble::tibble(reproductiveCondition = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_individual_traits(reproductiveCondition = reproductiveCondition)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_individual_traits(reproductiveCondition = reproductiveCondition)),
    "reproductiveCondition must be a character vector, not integer"
  )
})
