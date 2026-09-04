# Reporting -------------------------------------------------------------------

validate_report_limit <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 0 || x != trunc(x)) {
    daffiz_abort(
      "daffiz_error_report",
      paste0("`", name, "` must be a single non-negative whole number."),
      argument = name
    )
  }
  as.integer(x)
}

format_tolerance <- function(x, max_n = 4L) {
  values <- unique(as.numeric(x))
  if (length(values) == 1L) return(format(values, trim = TRUE, digits = 8L))
  shown <- utils::head(seq_along(x), max_n)
  out <- paste0(
    names(x)[shown], "=", format(x[shown], trim = TRUE, digits = 8L),
    collapse = ", "
  )
  if (length(x) > max_n) out <- paste0(out, ", ...")
  out
}

# Counts are taken from the cached summaries and the cell table directly.
# Going through `diff_cells()` / `x_only()` / `y_only()` meant materialising a
# full copy of every non-equal cell and every unmatched source row just to call
# `nrow()` on it.
comparison_counts <- function(cmp) {
  columns <- column_summary(cmp)
  list(
    matching = is_matching(cmp),
    incomplete = has_uncompared_duplicates(cmp),
    n_non_equal_cells = sum(columns$n_diff, columns$n_x_only, columns$n_y_only),
    n_affected_rows = nrow(affected_row_summary(cmp)),
    n_affected_columns = nrow(diff_columns(cmp)),
    n_compared_cells = sum(columns$n_compared),
    n_x_only_rows = length(indices_for_type(cmp, "x", "only")),
    n_y_only_rows = length(indices_for_type(cmp, "y", "only"))
  )
}

#' Summarize a daffiz comparison
#'
#' Builds a bounded, structured report backed by the public data accessors.
#' Printing the result shows the overall outcome followed by the most affected
#' columns, rows, and cells.
#'
#' @param object A `daffiz_comparison` from [compare_dt()].
#' @param max_columns,max_rows,max_cells Maximum records retained in each
#'   printed diagnostic section. Use zero to hide a section.
#' @param ... Reserved for future extensions.
#'
#' @return A `summary.daffiz_comparison` object.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27))
#' summary(compare_dt(x, y))
summary.daffiz_comparison <- function(
    object, ...,
    max_columns = 5L,
    max_rows = 5L,
    max_cells = 5L) {
  validate_comparison(object)
  max_columns <- validate_report_limit(max_columns, "max_columns")
  max_rows <- validate_report_limit(max_rows, "max_rows")
  max_cells <- validate_report_limit(max_cells, "max_cells")
  counts <- comparison_counts(object)

  structure(
    c(
      list(
        settings = object$settings,
        columns = utils::head(diff_columns(object), max_columns),
        rows = utils::head(diff_rows(object), max_rows),
        cells = utils::head(diff_cells(object), max_cells),
        duplicate_info = duplicate_info(object),
        x_profile = key_profile(object, "x"),
        y_profile = key_profile(object, "y"),
        limits = list(
          max_columns = max_columns,
          max_rows = max_rows,
          max_cells = max_cells
        )
      ),
      counts
    ),
    class = "summary.daffiz_comparison"
  )
}

report_result_line <- function(x) {
  if (x$incomplete) {
    base <- sprintf(
      "Result: incomplete; %s duplicated identity group(s) were not compared",
      format(nrow(x$duplicate_info), big.mark = ",")
    )
    if (x$n_non_equal_cells > 0L) {
      base <- paste0(
        base, "; ", format(x$n_non_equal_cells, big.mark = ","),
        " other cell(s) are not equal"
      )
    }
    return(base)
  }
  if (x$matching) return("Result: all compared cells match")
  sprintf(
    "Result: %s non-equal cell(s) in %s row(s) across %s measure(s)",
    format(x$n_non_equal_cells, big.mark = ","),
    format(x$n_affected_rows, big.mark = ","),
    format(x$n_affected_columns, big.mark = ",")
  )
}

print_report_table <- function(title, x, columns = names(x)) {
  if (!nrow(x)) return(invisible(NULL))
  cat("\n", title, "\n", sep = "")
  out <- copy(x[, columns, with = FALSE])
  if ("diff_columns" %in% names(out)) {
    out[, diff_columns := vapply(diff_columns, paste, character(1L), collapse = ",")]
  }
  print(out)
  invisible(NULL)
}

#' @export
print.summary.daffiz_comparison <- function(x, ...) {
  s <- x$settings
  cat(sprintf("daffiz comparison: %s -> %s\n", s$x_name, s$y_name))
  cat(sprintf(
    "Rows: %s / %s | numeric measures: %d | compared cells: %s\n",
    format(s$n_row_x, big.mark = ","),
    format(s$n_row_y, big.mark = ","),
    length(s$compare),
    format(x$n_compared_cells, big.mark = ",")
  ))
  cat(report_result_line(x), "\n", sep = "")
  cat(sprintf(
    "Identity (%s): %s\n",
    if (s$by_inferred) "inferred" else "explicit",
    fmt_names(s$by)
  ))
  cat(sprintf(
    "Measures (%s): %s\n",
    if (s$compare_inferred) "inferred" else "explicit",
    fmt_names(s$compare)
  ))
  cat(sprintf(
    "Tolerance: abs %s | rel %s\n",
    format_tolerance(s$abs_tol),
    format_tolerance(s$rel_tol)
  ))
  if (s$batch < length(s$compare)) {
    cat(sprintf("Measure batch size: %d\n", s$batch))
  }

  if (length(s$excluded)) {
    cat("Excluded: ", fmt_names(s$excluded), "\n", sep = "")
  }
  if (length(s$unknown_exclude)) {
    cat("Unknown exclusions: ", fmt_names(s$unknown_exclude), "\n", sep = "")
  }
  if (length(s$ignored)) {
    cat("Ignored after role resolution: ", fmt_names(s$ignored), "\n", sep = "")
  }
  if (nrow(x$duplicate_info)) {
    cat(sprintf(
      "Duplicate identities: %s group(s) | policy: %s%s\n",
      format(nrow(x$duplicate_info), big.mark = ","),
      s$duplicate_keys,
      if (s$duplicate_keys == "pair") " (heuristic pairing)" else ""
    ))
  }
  if (x$n_x_only_rows > 0L || x$n_y_only_rows > 0L) {
    cat(sprintf(
      "Unmatched rows: %s only in %s | %s only in %s\n",
      format(x$n_x_only_rows, big.mark = ","), s$x_name,
      format(x$n_y_only_rows, big.mark = ","), s$y_name
    ))
    if (nrow(x$x_profile)) {
      cat("Leading ", s$x_name, " key profile: ", x$x_profile$column[[1L]], "\n", sep = "")
    }
    if (nrow(x$y_profile)) {
      cat("Leading ", s$y_name, " key profile: ", x$y_profile$column[[1L]], "\n", sep = "")
    }
  }

  print_report_table(
    "Measures with differences",
    x$columns,
    c("column", "n_diff", "n_x_only", "n_y_only", "fraction_diff", "max_abs_diff")
  )
  print_report_table(
    "Rows with differences",
    x$rows,
    c(".row_x", ".row_y", "row_type", "n_diff", "diff_columns", "max_abs_diff")
  )
  cell_columns <- c(
    s$by,
    if (s$occurrence) ".occurrence" else character(),
    ".row_x", ".row_y", ".metric", ".value_x", ".value_y", ".diff",
    ".match_type"
  )
  print_report_table("Example cells", x$cells, cell_columns)
  invisible(x)
}
