#' Check a dataset for Darwin Core conformance
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
check_dataset <- function(.df){

  # dwc_terms
  fields <- colnames(.df)
  available_checks <- fn_to_term_table() |>
    bind_rows() |>
    select("dwc_term") |>
    pull()
  checkable_fields <- fields[fields %in% available_checks]

  # find fields in .df with available checks
  check_functions_names <- c(glue("check_{checkable_fields}"))
  check_functions <- as.list(check_functions_names)
  names(check_functions) <- checkable_fields

  ## Run Checks

  # inform user
  cli::cli_alert_info("Testing data")
  for(i in 1:100) {
    wait(0.001)
  }

  # build table
  add_table_headers(checkable_fields)
  invisible() # prevent df results from printing with headers

  # check all checkable fields, save fields & error messages
  check_results <- check_functions_names |>
    map(~ check_all(.x, .df, checkable_fields)) |>
    bind_rows()

  # print result summary
  summary_message(check_results, checkable_fields)
  cat_line()

  ## Darwin Core compliance
  # dwc_spinny_message(c("Meets minimum requirements for Darwin Core terms"))
  dwc_compliant <- check_min_req_dwc(checkable_fields)
  min_req_dwc_message(dwc_compliant)

  ## Error Messages

  # truncate
  gt_msg_max <- ifelse(length(check_results$messages) > 5, TRUE, FALSE)
  check_results <- truncate_messages(check_results, gt_msg_max)

  if(length(check_results$messages) > 0) {

    dwc_spinny_message(paste0("Collecting error messages"))

  # split messages by function for message formatting
    results_split <- check_results |>
      unnest(.data$messages) |>
      mutate(
        term = factor(.data$term, levels = unique(.data$term)) # maintain original term order
        ) |>
      group_split(.data$term)

  # print preserved errors in a nice format
    results_split |>
      map(~ format_messages_from_checks(.x))

  } else {
    if(isTRUE(dwc_compliant)) {
      # celebrate
      cat_line(paste0("\n", add_emoji(), " ", col_green("All column checks pass!"), "\n"))
    }
  }

  # TODO: check_dataset() should only celebrate when Data meets minimum requirements AND column checks pass
  invisible(.df)
}


#' Check all fields that match Darwin Core terms in a data frame
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
#' @importFrom rlang cnd_muffle
#' @importFrom rlang exec
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
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
  progress_msg <- paste0(
    # ansi_align(" ", max(ansi_nchar(symbol$tick))), " ",
                         ansi_align(glue("| {m_counter}"), ansi_nchar("| E")), " ",
                         ansi_align(glue("{passing} |"), ansi_nchar("P |")), " ",
                         ansi_align(glue("{fn_name}"), field_nchar), " "
  )

  # run live checks
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
        progress_msg <<- paste0(
          # ansi_align(" ", max(ansi_nchar(symbol$tick))), " ",
                                ansi_align(glue("| {m_counter}"), ansi_nchar("| E")), " ",
                                ansi_align(glue("{passing} |"), ansi_nchar("P |")), " ",
                               ansi_align(glue("{fn_name}"), field_nchar), " "
        )
        cli_progress_update(id = progress)
        cnd_muffle(m)
      } else {
        passing <<- col_green(symbol$tick)
        progress_msg <<- paste0(
          # ansi_align(" ", max(ansi_nchar(symbol$tick))), " ",
                                ansi_align(glue("| {m_counter}"), ansi_nchar("| E")), " ",
                                ansi_align(glue("{passing} |"), ansi_nchar("P |")), " ",
                               ansi_align(glue("{fn_name}"), field_nchar), " "
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
#' @importFrom cli col_yellow
#' @importFrom cli col_green
#'
#' @noRd
#' @keywords Internal
add_table_headers <- function(row_values) {
  # browser()
  headers <- paste0(
    ansi_align(col_green(symbol$tick), max(ansi_nchar(symbol$tick))), " ",
    ansi_align(glue("| {col_yellow('E')}"), max(ansi_nchar("| E"))), " ",
    ansi_align(glue("{col_green('P')} |"), max(ansi_nchar(glue("{symbol$tick} |")))), " ",
    ansi_align("Column", max(ansi_nchar(row_values)))

  )
  cat_line(headers)
}

#' Format each saved message from `check_all()` nicely
#'
#' @importFrom cli cat_line
#' @importFrom cli cli_rule
#' @importFrom cli style_bold
#' @importFrom cli col_yellow
#'
#' @noRd
#' @keywords Internal
format_messages_from_checks <- function(df) {
  # retrieve term & message
  term <- df$term |> unique()
  m <- paste0(df$messages)

  # format & print
  cat_line()
  cli_rule("{col_yellow(style_bold('Error'))} in {term}")
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
  cli_div(theme = list(rule = list("line-type" = "double")))
  cli::cli_rule(left = "Results")
  cli_end()
  cat_line()
  cat_line(glue("[ {col_yellow('Errors')}: {n_errors} | {col_green('Pass')}: {n_passing_fields} ]"))
}


#' Check whether data meets minimum requirements
#'
#' @description
#' Simple check for whether data meets minimum requirements to be accepted as a
#' Darwin Core archive. The check is a simplifed version of the underlying check
#' in `suggest_workflow()`.
#' @noRd
#' @keywords Internal
check_min_req_dwc <- function(checkable_fields) {

  # message
  dwc_spinny_message(glue("Data meets minimum Darwin Core requirements"))

  # check matching user columns with minimum required DwC terms
  req_terms_results <- check_required_terms(checkable_fields)
  is_dwc_compliant <- all(req_terms_results$result == "pass")
  return(is_dwc_compliant)
}

#' Build message about results of `check_min_req_dwc()`
#'
#' @importFrom cli cat_line
#' @importFrom cli col_red
#' @importFrom cli col_green
#' @importFrom cli cli_progress_step
#' @noRd
#' @keywords Internal
min_req_dwc_message <- function(is_dwc_compliant) {

  if(isTRUE(is_dwc_compliant)) {
    complies_text <- "Data meets minimum Darwin Core requirements"
    cli::cli_status_clear()
    cat_line(glue("{col_green(symbol$tick)} {complies_text}"))
  } else {
    noncomplies_text <- "Data does not meet minimum Darwin Core requirements"
    cli::cli_status_clear()
    cat_line(glue("{col_red(symbol$cross)} {noncomplies_text}"))
    cli_bullets(c(i = "Use `suggest_workflow()` to see more information."))
  }
  cat_line()

}


#' Truncate list of messages
#'
#' @description
#' Truncates list of messages if greater than 5.
#'
#' @importFrom cli cat_line
#' @importFrom cli cli_h3
#' @importFrom cli col_green
#' @importFrom dplyr slice_head
#' @noRd
#' @keywords Internal
truncate_messages <- function(check_results, gt_msg_max) {
  if(isTRUE(gt_msg_max)) {
    check_results <- check_results |>
      slice_head(n = 5)

    cli_h3(col_yellow(style_italic("Truncating to first 5 error messages")))
  }

  return(check_results)
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

