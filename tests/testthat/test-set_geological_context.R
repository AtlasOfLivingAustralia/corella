test_that("set_geological_context errors when missing .df", {
  expect_error(set_geological_context(geologicalContextID = geologicalContextID),
               ".df is missing")
})

test_that("set_geological_context errors when no dwc columns are named or exist in the df", {
  df <- tibble::tibble(borp = "Oceania")

  expect_warning(suppressMessages(set_geological_context(df)),
                 "No Darwin Core terms detected")
})

test_that("set_geological_context returns tibble with updated dwc column names", {
   quiet_set_geological_context <- purrr::quietly(set_geological_context)
  df <- tibble::tibble(user_col = "thisIsAnID")

  result <- df |>
    quiet_set_geological_context(geologicalContextID = user_col)

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_match(colnames(result$result), c("geologicalContextID"))
})

test_that("set_geological_context detects unnamed but existing dwc column names in df", {
   quiet_set_geological_context <- purrr::quietly(set_geological_context)
  df <- tibble::tibble(geologicalContextID = c("https://opencontext.org/subjects/576d8322-9a55-4a9b-e60d-b466be610bb7",
                                               "https://opencontext.org/subjects/4ef35961-af07-4dec-3106-48baf0967a0a"),
                       col2 = 1:2)

  result <- df |>
     quiet_set_geological_context()

  expect_s3_class(result$result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result$result), c("geologicalContextID", "col2"))
})

test_that("set_geological_context has progress messages", {
  quiet_set_geological_context <- purrr::quietly(set_geological_context)
  df <- tibble::tibble(geologicalContextID = c("https://opencontext.org/subjects/576d8322-9a55-4a9b-e60d-b466be610bb7",
                                               "https://opencontext.org/subjects/4ef35961-af07-4dec-3106-48baf0967a0a"),
                       col2 = 1:2)

  result <- df |> quiet_set_geological_context()

  expect_false(is.null(result$messages))

})

test_that("set_geological_context checks geologicalContextID format", {

  df <- tibble::tibble(geologicalContextID = c("thisIsAnID", "thisIsAnDifferentID"))
  df_dupes <- tibble::tibble(geologicalContextID = c("thisIsAnID", "thisIsAnID"))

  expect_no_error(suppressMessages(
    df |> set_geological_context(geologicalContextID = geologicalContextID)
  ))
  expect_error(suppressMessages(
    df_dupes |> set_geological_context(geologicalContextID = geologicalContextID)),
    "Duplicate values in geologicalContextID"
  )
})

test_that("set_geological_context checks earliestAgeOrLowestStage format", {

  df_chr <- tibble::tibble(earliestAgeOrLowestStage = c("Skullrockian", "Skullrockian"))
  df_dbl <- tibble::tibble(earliestAgeOrLowestStage = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(earliestAgeOrLowestStage = earliestAgeOrLowestStage)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(earliestAgeOrLowestStage = earliestAgeOrLowestStage)),
    "earliestAgeOrLowestStage must be a character vector, not integer"
  )
})

test_that("set_geological_context checks latestAgeOrHighestStage format", {

  df_chr <- tibble::tibble(latestAgeOrHighestStage = c("Skullrockian", "Skullrockian"))
  df_dbl <- tibble::tibble(latestAgeOrHighestStage = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(latestAgeOrHighestStage = latestAgeOrHighestStage)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(latestAgeOrHighestStage = latestAgeOrHighestStage)),
    "latestAgeOrHighestStage must be a character vector, not integer"
  )
})

test_that("set_geological_context checks earliestPeriodOrLowestSystem format", {

  df_chr <- tibble::tibble(earliestPeriodOrLowestSystem = c("Quartenary", "Tertiary"))
  df_dbl <- tibble::tibble(earliestPeriodOrLowestSystem = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(earliestPeriodOrLowestSystem = earliestPeriodOrLowestSystem)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(earliestPeriodOrLowestSystem = earliestPeriodOrLowestSystem)),
    "earliestPeriodOrLowestSystem must be a character vector, not integer"
  )
})

test_that("set_geological_context checks latestPeriodOrHighestSystem format", {

  df_chr <- tibble::tibble(latestPeriodOrHighestSystem = c("Quartenary", "Tertiary"))
  df_dbl <- tibble::tibble(latestPeriodOrHighestSystem = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(latestPeriodOrHighestSystem = latestPeriodOrHighestSystem)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(latestPeriodOrHighestSystem = latestPeriodOrHighestSystem)),
    "latestPeriodOrHighestSystem must be a character vector, not integer"
  )
})

test_that("set_geological_context checks lowestBiostratigraphicZone format", {

  df_chr <- tibble::tibble(lowestBiostratigraphicZone = c("Maastrichtian", "Blancan"))
  df_dbl <- tibble::tibble(lowestBiostratigraphicZone = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(lowestBiostratigraphicZone = lowestBiostratigraphicZone)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(lowestBiostratigraphicZone = lowestBiostratigraphicZone)),
    "lowestBiostratigraphicZone must be a character vector, not integer"
  )
})

test_that("set_geological_context checks highestBiostratigraphicZone format", {

  df_chr <- tibble::tibble(highestBiostratigraphicZone = c("Maastrichtian", "Blancan"))
  df_dbl <- tibble::tibble(highestBiostratigraphicZone = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(highestBiostratigraphicZone = highestBiostratigraphicZone)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(highestBiostratigraphicZone = highestBiostratigraphicZone)),
    "highestBiostratigraphicZone must be a character vector, not integer"
  )
})

test_that("set_geological_context checks highestBiostratigraphicZone format", {

  df_chr <- tibble::tibble(lithostratigraphicTerms = c("Pleistocene-Weichselien", "Pleistocene-Weichselien"))
  df_dbl <- tibble::tibble(lithostratigraphicTerms = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(lithostratigraphicTerms = lithostratigraphicTerms)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(lithostratigraphicTerms = lithostratigraphicTerms)),
    "lithostratigraphicTerms must be a character vector, not integer"
  )
})

test_that("set_geological_context checks group format", {

  df_chr <- tibble::tibble(group = c("Bathurst", "Lower Wealden"))
  df_dbl <- tibble::tibble(group = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(group = group)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(group = group)),
    "group must be a character vector, not integer"
  )
})

test_that("set_geological_context checks formation format", {

  df_chr <- tibble::tibble(formation = c("House Limestone", "Notch Peak Formation"))
  df_dbl <- tibble::tibble(formation = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(formation = formation)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(formation = formation)),
    "formation must be a character vector, not integer"
  )
})

test_that("set_geological_context checks formation member", {

  df_chr <- tibble::tibble(member = c("Lava Dam Member", "Hellnmaria Member"))
  df_dbl <- tibble::tibble(member = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(member = member)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(member = member)),
    "member must be a character vector, not integer"
  )
})

test_that("set_geological_context checks bed member", {

  df_chr <- tibble::tibble(bed = c("Harlem coal", "Harlem coal"))
  df_dbl <- tibble::tibble(bed = 1:3)

  expect_no_error(suppressMessages(
    df_chr |> set_geological_context(bed = bed)
  ))
  expect_error(suppressMessages(
    df_dbl |> set_geological_context(bed = bed)),
    "bed must be a character vector, not integer"
  )
})

