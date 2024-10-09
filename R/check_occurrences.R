#' Check occurrence data for Darwin Core conformance
#'
#' Function to check whether a `data.frame` or `tibble` conforms to Darwin
#' Core standards. While most users will only want to call `suggest_workflow()`,
#' the underlying check functions are exported for detailed work, or for
#' debugging.
#' @param .df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom purrr map
#' @importFrom cli cli_bullets
#' @importFrom cli col_blue
#' @importFrom cli col_green
#' @importFrom cli col_red
#' @importFrom cli col_yellow
#' @importFrom cli style_italic
#' @importFrom cli cli_h3
#' @importFrom cli ansi_align
#' @importFrom cli ansi_nchar
#' @importFrom cli cat_line
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom dplyr slice_head
#' @importFrom dplyr group_split
#' @importFrom tidyr unnest
#' @returns Invisibly returns the input, but primarily called for the
#' side-effect of running check functions on that input.
#' @order 1
#' @export
check_occurrences <- function(.df){

  # dwc_terms
  fields <- colnames(.df)
  available_checks <- fn_to_term_table() |>
    bind_rows() |>
    select(dwc_term) |>
    pull()
  checkable_fields <- fields[fields %in% available_checks]

  # find fields in .df with available checks
  check_functions_names <- c(glue("check_{checkable_fields}"))
  check_functions <- as.list(check_functions_names)
  names(check_functions) <- checkable_fields

  ## Table

  # print table headers
  add_table_headers(checkable_fields)
  invisible() # prevent df results from printing with headers

  # Check all checkable fields, save fields & error messages
  check_results <-
    check_functions_names |>
    map(~ check_all(.x, .df, checkable_fields)) |>
    bind_rows()

  # result summary
  summary_message(check_results, checkable_fields)


  ## Messages

  # truncate to 5 messages if there are more than 5
  if(length(check_results$messages) > 5) {
    check_results <- check_results |>
      slice_head(n = 5)

    cli_h3(col_yellow(style_italic("Truncating to first 5 error messages")))
  }

  if(length(check_results$messages) > 0) {

  # split messages by function for message formatting
    results_split <- check_results |>
      unnest(messages) |>
      mutate(
        term = factor(term, levels = unique(term)) # maintain original term order
        ) |>
      group_split(term)

  # print preserved errors in a nice format
    results_split |>
      map(~ format_messages_from_checks(.x))

  } else {
    # celebrate
    cat_line(paste0("\n", add_emoji(), " ", col_green("All column checks pass!"), "\n"))
  }

  invisible(.df)
}


#' Check all fields that match Darwin Core terms in a dataframe
#'
#' @description
#' Runs checks on all columns that match Darwin Core terms. `check_all()` does this by
#' detecting and matching matched Darwin Core terms to their associated `check_` function.
#'
#' `check_all()` runs in a similar way to `devtools::test()`, whereby it will run and
#' report the results of checks "live". `check_all()` will then return a summary table and
#' any error messages returned by data checks.
#'
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
#' @importFrom cli ansi_align
#' @importFrom cli ansi_nchar
#' @importFrom stringr str_remove_all
#' @importFrom rlang cnd_muffle
#' @importFrom rlang exec
#' @noRd
#' @keywords Internal
check_all <- function(fn, .df, checkable_fields) {

  # message saving & counting setup
  m_counter <- 0
  msgs <- list()
  passing <- list()
  all_results <- list()

  # message format setup
  field_nchar <- max(ansi_nchar(checkable_fields))
  fn_name <- str_remove_all(fn, 'check_')
  progress_msg <- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                         ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                         ansi_align(glue("{passing}"), ansi_nchar(1)), " "
  )

  # run checks
  tryCatch(withCallingHandlers(
    {
      progress <- cli_progress_step("{progress_msg}", spinner = TRUE) # prints message
      exec(fn, .df) # runs check function
    },
    message = function(m) {
      # update counter if a galax message is triggered
      if (inherits(m, "galax_message")) {
        m_counter <<- m_counter + 1
        msgs <<- append(msgs, m$message)
        passing <<- col_red(symbol$cross)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                               ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                               ansi_align(glue("{passing}"), ansi_nchar(1)), " "
        )
        cli_progress_update(id = progress)
        cnd_muffle(m)
      } else {
        passing <<- col_green(symbol$tick)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                                ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                                ansi_align(glue("{passing}"), ansi_nchar(1)), " "
        )
      }
    }),

    finally = {
      # capture all messages somewhere
      results <- tibble(
        term = fn_name,
        check_function = fn,
        messages = msgs
      )
      return(results)
    }
  )
}

#' Format table headers for `check_all()`
#'
#' @importFrom cli ansi_align
#' @importFrom cli ansi_nchar
#'
#' @noRd
#' @keywords Internal
add_table_headers <- function(row_values) {
  headers <- paste0(
    ansi_align(" ", max(ansi_nchar(symbol$tick))), " ",
    ansi_align(col_blue("Column"), max(ansi_nchar(row_values))), " ",
    ansi_align(col_red("E"), max(ansi_nchar("E "))), " ",
    ansi_align(col_green("P"), max(ansi_nchar("P")))
  )
  cat_line(headers)
}

#' Format each saved message from `check_all()` nicely
#'
#' @importFrom cli cat_line
#' @importFrom cli cli_rule
#'
#' @noRd
#' @keywords Internal
format_messages_from_checks <- function(df) {
  # retrieve term & message
  term <- df$term |> unique()
  m <- paste0(df$messages)

  # format & print
  cat_line()
  cli_rule("Error in {term}")
  cat_line()
  cat_line(m)
  cat_line()
}

#' Build `check_all()` summary message
#'
#' @importFrom cli cat_line
#' @importFrom cli col_red
#' @importFrom cli col_green
#' @noRd
#' @keywords Internal
summary_message <- function(results, checkable_fields) {
  n_errors <- length(results$messages)
  n_passing_fields <- length(checkable_fields) - length(unique(results$term))

  # message
  cat_line()
  cat_line(glue("[ FAIL: {col_red(n_errors)} | {col_green('PASS')} {col_green(n_passing_fields)} ]"))
}


#' Advanced `check_all()` with separate message, warning, error tracking
#' @noRd
#' @keywords Internal
# check_all_advanced <- function(fn, .df, checkable_fields) {
#
#   # message saving & counting setup
#   m_counter <- 0
#   w_counter <- 0
#   e_counter <- 0
#   msgs <- list()
#   wrns <- list()
#   errs <- list()
#   all_results <- list()
#
#   # message format setup
#   field_nchar <- max(ansi_nchar(checkable_fields))
#   fn_name <- stringr::str_remove_all(fn, 'check_')
#   progress_msg <- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
#                          ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
#                          ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
#                          ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
#   )
#
#   # run check functions
#   tryCatch(withCallingHandlers(
#     {
#       progress <- cli::cli_progress_step("{progress_msg}", spinner = TRUE) # prints message
#       rlang::exec(fn, .df) # runs check function
#     },
#     message = function(m) {
#       if (inherits(m, "galax_message")) {
#         m_counter <<- m_counter + 1
#         msgs <<- append(msgs, m$message)
#         progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
#                                 ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
#         )
#         cli::cli_progress_update(id = progress)
#         rlang::cnd_muffle(m)
#       }
#     },
#     warning = function(w) {
#       if (inherits(w, "galax_warning")) {
#         w_counter <<- w_counter + 1
#         wrns <<- append(wrns, w$message)
#         progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
#                                 ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
#         )
#         cli::cli_progress_update(id = progress)
#         rlang::cnd_muffle(w)
#       }
#     }),
#     error = function(e) {
#       if (inherits(e, "galax_error")) {
#         e_counter <<- e_counter + 1
#         errs <<- append(errs, e)
#         progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
#                                 ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
#                                 ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
#         )
#         cli::cli_progress_update(id = progress)
#         rlang::cnd_muffle(e)
#       }
#     },
#
#     finally = {
#       # capture all messages somewhere
#       tibble(
#         term = fn_name,
#         check_function = fn,
#         messages = msgs,
#         warnings = wrns,
#         errors = errs
#       )
#     }
#   )
# }

