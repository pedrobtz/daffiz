# testthat expectation --------------------------------------------------------

format_failure_value <- function(x) {
  if (length(x) == 0L) return("<none>")
  if (is.nan(x)) return("NaN")
  if (is.na(x)) return("NA")
  if (is.character(x)) return(encodeString(x, quote = '"'))
  if (is.factor(x)) return(encodeString(as.character(x), quote = '"'))
  if (is.numeric(x)) return(format(x, trim = TRUE, digits = 12L))
  as.character(x)
}

expectation_failure_text <- function(cmp, max_columns, max_rows, max_cells) {
  report <- summary(
    cmp,
    max_columns = max_columns,
    max_rows = max_rows,
    max_cells = max_cells
  )
  s <- report$settings
  lines <- c(
    sprintf("Tables do not match: %s -> %s", s$x_name, s$y_name),
    sub("^Result: ", "", report_result_line(report)),
    paste0("Identity: ", paste(s$by, collapse = ", ")),
    paste0("Measures: ", paste(s$compare, collapse = ", ")),
    sprintf(
      "Tolerance: abs %s; rel %s",
      format_tolerance(s$abs_tol),
      format_tolerance(s$rel_tol)
    )
  )

  if (nrow(report$duplicate_info)) {
    lines <- c(lines, sprintf(
      "Duplicate identities: %d group(s); policy=%s%s",
      nrow(report$duplicate_info),
      s$duplicate_keys,
      if (s$duplicate_keys == "pair") "; heuristic pairing used" else ""
    ))
  }

  if (nrow(report$columns)) {
    lines <- c(lines, "Columns:")
    for (i in seq_len(nrow(report$columns))) {
      row <- report$columns[i]
      lines <- c(lines, sprintf(
        "  %s: n_diff=%d, n_x_only=%d, n_y_only=%d, fraction_diff=%s, max_abs_diff=%s",
        row$column,
        row$n_diff,
        row$n_x_only,
        row$n_y_only,
        format_failure_value(row$fraction_diff),
        format_failure_value(row$max_abs_diff)
      ))
    }
  }

  if (nrow(report$rows)) {
    lines <- c(lines, "Rows:")
    for (i in seq_len(nrow(report$rows))) {
      row <- report$rows[i]
      lines <- c(lines, sprintf(
        "  .row_x=%s, .row_y=%s, type=%s, n_diff=%d, columns=%s, max_abs_diff=%s",
        format_failure_value(row$.row_x),
        format_failure_value(row$.row_y),
        as.character(row$row_type),
        row$n_diff,
        paste(row$diff_columns[[1L]], collapse = ","),
        format_failure_value(row$max_abs_diff)
      ))
    }
  }

  if (nrow(report$cells)) {
    lines <- c(lines, "Cells:")
    for (i in seq_len(nrow(report$cells))) {
      row <- report$cells[i]
      identity <- paste(
        vapply(
          s$by,
          function(nm) paste0(nm, "=", format_failure_value(row[[nm]])),
          character(1L)
        ),
        collapse = ", "
      )
      lines <- c(lines, sprintf(
        "  %s; .row_x=%s, .row_y=%s, metric=%s, x=%s, y=%s, diff=%s, type=%s",
        identity,
        format_failure_value(row$.row_x),
        format_failure_value(row$.row_y),
        row$.metric,
        format_failure_value(row$.value_x),
        format_failure_value(row$.value_y),
        format_failure_value(row$.diff),
        as.character(row$.match_type)
      ))
    }
  }

  paste(lines, collapse = "\n")
}

#' Expect two tables to match within tolerance
#'
#' A testthat expectation backed by [compare_dt()]. Failures contain bounded,
#' deterministic column, row, and cell diagnostics. The complete comparison is
#' attached to the failure condition as its `comparison` attribute.
#'
#' Unlike [compare_dt()], this defaults to `duplicate_keys = "error"`: a
#' duplicated identity makes row alignment a heuristic, which is not a sound
#' basis for a passing assertion. Pass `duplicate_keys = "pair"` explicitly to
#' accept the heuristic.
#'
#' @param object,expected Tables to compare.
#' @param ... Arguments passed to [compare_dt()], for example `by`, `compare`,
#'   `abs_tol`, `rel_tol` or `duplicate_keys`.
#' @param max_columns,max_rows,max_cells Maximum diagnostic records included in
#'   a failure message.
#'
#' @return The comparison object, invisibly.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' expect_dt_equal(x, x)
expect_dt_equal <- function(
    object,
    expected,
    ...,
    max_columns = 10L,
    max_rows = 10L,
    max_cells = 20L) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    daffiz_abort(
      "daffiz_error_dependency",
      "expect_dt_equal() requires the suggested package 'testthat'."
    )
  }
  max_columns <- validate_report_limit(max_columns, "max_columns")
  max_rows <- validate_report_limit(max_rows, "max_rows")
  max_cells <- validate_report_limit(max_cells, "max_cells")

  dots <- list(...)
  # `compare_dt()` defaults to "pair", which sorts each duplicated group by its
  # measure values before pairing -- that is, it pairs rows so as to minimise
  # apparent differences. Under that default an assertion passes on tables
  # whose duplicated rows genuinely disagree, with only a warning that testthat
  # does not fail on. An assertion must not decide identity heuristically, so
  # ambiguity is an error here unless the caller asks for something else.
  if (!"duplicate_keys" %in% names(dots)) {
    dots$duplicate_keys <- "error"
  }
  if (!"x_name" %in% names(dots)) {
    dots$x_name <- capture_label(substitute(object), "object")
  }
  if (!"y_name" %in% names(dots)) {
    dots$y_name <- capture_label(substitute(expected), "expected")
  }
  cmp <- do.call(compare_dt, c(list(x = object, y = expected), dots))

  if (is_matching(cmp)) {
    testthat::succeed()
    return(invisible(cmp))
  }

  testthat::expectation(
    "failure",
    expectation_failure_text(cmp, max_columns, max_rows, max_cells),
    comparison = cmp
  )
  invisible(cmp)
}
