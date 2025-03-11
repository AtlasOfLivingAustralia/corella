#' Create unique identifier columns
#'
#' @description
#' A unique identifier is a pattern of words, letters and/or numbers that is
#' unique to a single record within a dataset.
#' Unique identifiers are useful because they identify unique observations,
#' even when more than one record contains the same information
#' (and would otherwise be considered a duplicate).
#'
#' The identifier functions in corella, designed to make it easier to
#' generate such columns from a given dataset. They are designed to be called
#' within [use_events()], [use_occurrences()], or (equivalently)
#' [dplyr::mutate()]. Generally speaking, it is best practice to use existing
#' information from a dataset to generate identifiers; for this reason we
#' recomment using `composite_id()` to aggregate existing fields, if no
#' such composite is already present within the dataset. It is possible to call
#' `sequential_id()` or `random_id()` within
#' `composite_id()` to combine existing and new columns.
#' @rdname identifier_functions
#' @param ... Zero or more variable names from the tibble being
#' mutated (unquoted), and/or zero or more `_id` functions, separated by
#' commas.
#' @param sep Character used to separate field values. Defaults to `"-"`
#' @returns An amended tibble, containing a field with the requested information.
#' @examples
#' df <- tibble::tibble(
#'   eventDate = paste0(rep(c(2020:2024), 3), "-01-01"),
#'   basisOfRecord = "humanObservation",
#'   site = rep(c("A01", "A02", "A03"), each = 5)
#'   )
#'
#' df |>
#'     use_occurrences(occurrenceID = composite_id(sequential_id(),
#'                                                 site,
#'                                                 eventDate))
#' @order 1
#' @export
composite_id <- function(...,
                              sep = "-"){
  x <- enquos(...)
  string_result <- purrr::map(x, switch_expr_type)
  names(string_result) <- glue("V{seq_along(string_result)}")
  string_result <- c(string_result, sep = sep)
  do.call(paste, string_result)
}

#' Switch functions for quosures
#' @param x A (single) quosure
#' @importFrom rlang abort
#' @importFrom rlang quo_get_expr
#' @noRd
#' @keywords internal
switch_expr_type <- function(x){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)},
         "call" = {eval_tidy(x)},
         "literal" = {quo_get_expr(x)},
         abort("Quosure type not recognised.")
  )
}

#' Get type from quosures
#' @param x A (single) quosure
#' @importFrom rlang quo_is_symbol
#' @importFrom rlang quo_is_call
#' @importFrom rlang quo_get_expr
#' @importFrom rlang is_syntactic_literal
#' @noRd
#' @keywords internal
expr_type <- function(x){
  if(quo_is_symbol(x)){
    "symbol"
  }else if(quo_is_call(x)){
    "call"
  }else if(is_syntactic_literal(quo_get_expr(x))){
    "literal"
  }else{
    typeof(x)
  }
}

#' Check whether symbols exist before they are parsed
#' @param x A (single) quosure
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @importFrom rlang eval_tidy
#' @importFrom rlang as_label
#' @noRd
#' @keywords internal
parse_symbol <- function(x){
  if(exists(quo_get_expr(x), where = quo_get_env(x))){
    result <- eval_tidy(x)
    if(inherits(result, "function")){ # special case for functions like 'data'
      as_label(x)                     # which exist in Global
    }else{
      result
    }
  }else{
    as_label(x)
  }
}

#' Internal function to parse a call
#' @importFrom rlang eval_tidy
#' @noRd
#' @keywords internal
parse_call <- function(x, ...){
  eval_tidy(x)
}

#' @rdname identifier_functions
#' @param width (Integer) how many characters should the resulting string be?
#' Defaults to one plus the order of magnitude of the largest number.
#' @order 2
#' @export
sequential_id <- function(width){
  row_count <- dplyr::n()
  result <- seq_len(row_count)
  max_digits <- max(floor(log10(result)) + 1)
  if(missing(width)){
    width <- max_digits + 1
  }
  formatC(result,
          width = width,
          format = "d",
          flag = "0")
}

#' @rdname identifier_functions
#' @order 3
#' @export
random_id <- function(){
  uuid::UUIDgenerate(use.time = TRUE, dplyr::n())
}
