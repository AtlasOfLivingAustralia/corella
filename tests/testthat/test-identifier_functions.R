test_that("random_id() generates unique and random IDs", {
  df <- tibble::tibble(x = 1:5)

  # Testing random ID generating unique ID for each row
  result <- df |>
    dplyr::mutate(id = random_id())
  expect_equal(nrow(result), 5)
  expect_true(all(nchar(result$id) == 36))
  expect_true(length(unique(result$id)) == 5)

  # Testing with a single row (single ID generated)
  single_row_df <- tibble::tibble(x = 1)
  result <- single_row_df |>
    dplyr::mutate(id = random_id())
  expect_equal(nrow(result), 1)
  expect_true(nchar(result$id) == 36)

  # Test with no rows (nothing returned)
  empty_df <- tibble::tibble(x = numeric(0))
  result <- empty_df |>
    dplyr::mutate(id = random_id())
  expect_equal(nrow(result), 0)

})

test_that("sequential_id generates sequential IDs correctly", {
  df <- tibble::tibble(x = 1:5)

  # Default width
  result <- df |>
    dplyr::mutate(id = sequential_id())
  expect_equal(result$id, c("01", "02", "03", "04", "05"))

  # Create sequential IDs of specified width
  result <- df |>
    dplyr::mutate(id = sequential_id(width = 4))
  expect_equal(result$id, c("0001", "0002", "0003", "0004", "0005"))

  # Single row - single ID
  single_row_df <- tibble::tibble(x = 1)
  result <- single_row_df |>
    dplyr::mutate(id = sequential_id())
  expect_equal(result$id, "01")

  # Single row - single ID of specified width
  single_row_df <- tibble::tibble(x = 1)
  result <- single_row_df |>
    dplyr::mutate(id = sequential_id(width = 6))
  expect_equal(result$id, "000001")

  # No rows - empty character returned and warning generated
  empty_df <- tibble::tibble(x = numeric(0))
  expect_warning(result <- empty_df |> dplyr::mutate(id = sequential_id()), "no non-missing arguments to max; returning -Inf")
  expect_equal(result$id, character(0))
})

test_that("composite_id generates composite strings correctly", {
  # Test with multiple inputs and default separator
  expect_equal(composite_id(1, "A", TRUE), "1-A-TRUE")
  # Test with a custom separator
  expect_equal(composite_id(1, "A", TRUE, sep = "_"), "1_A_TRUE")
  # Test with only one input
  expect_equal(composite_id("!@n$9m7"), "!@n$9m7")
  # Test with no input
  expect_equal(composite_id(sep = "-"), character(0))
  # Test with mixed input types
  expect_equal(composite_id(429, "te_b_st", NULL, 0.4, "\"", sep = "/"), "429/te_b_st//0.4/\"")

})
