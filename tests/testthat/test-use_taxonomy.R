
test_that("use_taxonomy errors when missing .df", {
  expect_error(use_taxonomy(kingdom = kingdom),
               ".df is missing")
})

test_that("use_taxonomy errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "Oceania")

  expect_warning(suppressMessages(use_taxonomy(df)),
                 "No Darwin Core terms detected")
})

test_that("use_taxonomy returns tibble with updated dwc column names", {
  quiet_use_taxonomy <- purrr::quietly(use_taxonomy)
  df <- tibble::tibble(user_col = "New South Wales")

  result <- df |>
    quiet_use_taxonomy(kingdom = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("kingdom"))
})

test_that("use_taxonomy detects unnamed but existing dwc column names in df", {
  quiet_use_taxonomy <- purrr::quietly(use_taxonomy)
  df <- tibble::tibble(kingdom = "Plantae",
                       col2 = 1:2)
  df2 <- tibble::tibble(kingdom = 1:2, # class check at the moment, but could be values
                        col2 = 1:2)

  result <- df |>
    quiet_use_taxonomy()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("kingdom", "col2"))
  expect_error(
    suppressMessages(
      df2 |> use_taxonomy()
    ),
    "kingdom must be a character vector, not integer")
})

test_that("use_taxonomy has progress messages", {
  quiet_use_taxonomy <- purrr::quietly(use_taxonomy)
  df <- tibble::tibble(kingdom = "Fungi",
                       col2 = 1:2)

  result <- df |> quiet_use_taxonomy()

  expect_false(is.null(result$messages))

})

test_that("use_taxonomy checks kingdom format", {

  df_chr <- tibble::tibble(kingdom = c("plantae", "plantae"))
  df_dbl <- tibble::tibble(kingdom =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(kingdom = kingdom)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(kingdom = kingdom)),
    "kingdom must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks phylum format", {

  df_chr <- tibble::tibble(phylum = c("Chordata", "Chordata"))
  df_dbl <- tibble::tibble(phylum =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(phylum = phylum)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(phylum = phylum)),
    "phylum must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks class format", {

  df_chr <- tibble::tibble(class = c("Amphibia", "Amphibia"))
  df_dbl <- tibble::tibble(class =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(class = class)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(class = class)),
    "class must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks order format", {

  df_chr <- tibble::tibble(order = c("Anura", "Anura"))
  df_dbl <- tibble::tibble(order =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(order = order)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(order = order)),
    "order must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks family format", {

  df_chr <- tibble::tibble(family = c("Myobatrachidae", "Myobatrachidae"))
  df_dbl <- tibble::tibble(family =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(family = family)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(family = family)),
    "family must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks genus format", {

  df_chr <- tibble::tibble(genus = c("Pseudophryne", "Pseudophryne"))
  df_dbl <- tibble::tibble(genus =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(genus = genus)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(genus = genus)),
    "genus must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks specificEpithet format", {

  df_chr <- tibble::tibble(specificEpithet = c("corroboree", "corroboree"))
  df_dbl <- tibble::tibble(specificEpithet =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(specificEpithet = specificEpithet)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(specificEpithet = specificEpithet)),
    "specificEpithet must be a character vector, not integer"
  )
})

test_that("use_taxonomy checks vernacularName format", {

  df_chr <- tibble::tibble(vernacularName = c("corroboree frog", "corroboree frog"))
  df_dbl <- tibble::tibble(vernacularName =1:3)

  expect_no_error(suppressMessages(
    df_chr |> use_taxonomy(vernacularName = vernacularName)
  ))
  expect_error(suppressMessages(
    df_dbl |> use_taxonomy(vernacularName = vernacularName)),
    "vernacularName must be a character vector, not integer"
  )
})
