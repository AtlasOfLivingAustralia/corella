
test_that("set_license errors when missing .df", {
  expect_error(set_license(license = license),
               ".df is missing")
})

test_that("set_license errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "http://creativecommons.org/licenses/by/4.0/legalcode")

  expect_warning(suppressMessages(set_license(df)),
                 "No Darwin Core terms detected")
})

test_that("set_license returns tibble with updated dwc column names", {
  quiet_set_license <- purrr::quietly(set_license)
  df <- tibble::tibble(user_col = "http://creativecommons.org/licenses/by/4.0/legalcode")

  result <- df |>
    quiet_set_license(license = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("license"))
})

test_that("set_license detects unnamed but existing dwc column names in df", {
  quiet_set_license <- purrr::quietly(set_license)
  df <- tibble::tibble(license = "CCBY0",
                       col2 = 1:2)
  df2 <- tibble::tibble(license = 1:2,
                        col2 = 1:2)

  result <- df |>
    quiet_set_license()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("license", "col2"))
  expect_error(
    suppressMessages(
      df2 |> set_license()
    ),
    "license must be a character vector, not integer")
})

test_that("set_license has progress messages", {
  quiet_set_license <- purrr::quietly(set_license)
  df <- tibble::tibble(license = "http://creativecommons.org/licenses/by/4.0/legalcode",
                       col2 = 1:2)

  result <- df |> quiet_set_license()

  expect_false(is.null(result$messages))

})


test_that("set_license checks license format", {

  df_chr <- tibble::tibble(license = c("http://creativecommons.org/licenses/by/4.0/legalcode",
                                       "CC-BY-NC 4.0 (Int)"))
  df_dbl <- tibble::tibble(license = c(2,1))

  expect_no_error(suppressMessages(
    df_chr |> set_license(license = license)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_license(license = license)),
    "license must be a character vector, not numeric"
  )
})

test_that("set_license checks rightsHolder format", {

  df_chr <- tibble::tibble(rightsHolder = c("Some person", "Some Institution Name"))
  df_dbl <- tibble::tibble(rightsHolder =1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_license(rightsHolder = rightsHolder)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_license(rightsHolder = rightsHolder)),
    "rightsHolder must be a character vector, not integer"
  )
})

test_that("set_license checks accessRights format", {

  df_chr <- tibble::tibble(accessRights = c("Some institution Name", "http://somerandomurl.org/"))
  df_dbl <- tibble::tibble(accessRights =1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_license(accessRights = accessRights)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_license(accessRights = accessRights)),
    "accessRights must be a character vector, not integer"
  )
})
