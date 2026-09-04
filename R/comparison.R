# The comparison object -------------------------------------------------------
#
# Design plan section 6. Accessors return copies so callers cannot mutate
# cached state by reference.

new_comparison <- function(settings, x_original, y_original, cells,
                             duplicate_info) {
  structure(
    list(
      settings = settings,
      x_original = x_original,
      y_original = y_original,
      cells = cells,
      duplicate_info = duplicate_info,
      cache = new.env(parent = emptyenv())
    ),
    class = "daffiz_comparison"
  )
}

validate_comparison <- function(cmp) {
  if (!inherits(cmp, "daffiz_comparison")) {
    daffiz_abort(
      "daffiz_error_comparison",
      "`cmp` must be a daffiz_comparison returned by compare_dt()."
    )
  }
  invisible(cmp)
}

cached_comparison_value <- function(cmp, name, build) {
  validate_comparison(cmp)
  if (!exists(name, envir = cmp$cache, inherits = FALSE)) {
    assign(name, build(), envir = cmp$cache)
  }
  get(name, envir = cmp$cache, inherits = FALSE)
}

# A cell contributes to the magnitude statistics when both *source values* are
# finite. Filtering on the derived `.diff` instead silently discarded genuine,
# enormous differences whose subtraction overflowed to Inf -- `max_abs_diff`
# then reported 0 for the largest difference in the table, and the affected row
# sorted last in every ranking.
is_finite_pair <- function(value_x, value_y) {
  is.finite(value_x) & is.finite(value_y)
}

# Root mean square, scaled by the largest magnitude so that squaring cannot
# overflow. Plain `sqrt(mean(z^2))` returns Inf for any difference above about
# 1.3e154, well inside the representable range.
scaled_rmse <- function(z) {
  if (!length(z)) return(NA_real_)
  m <- max(abs(z))
  if (!is.finite(m)) return(Inf)
  if (m == 0) return(0)
  m * sqrt(mean((z / m)^2))
}

has_uncompared_duplicates <- function(cmp) {
  identical(cmp$settings$duplicate_keys, "report") &&
    nrow(cmp$duplicate_info) > 0L
}

# A compact header, deliberately cheaper than `summary()`: it reads only the
# settings and one pass over `.match_type`. It used to build a full summary
# with every limit set to zero, which truncated the diagnostic sections only
# after computing them, so asking for five lines cost the whole report.
#
# `print()` shows the full diagnostic report rather than this header, so it
# does not route through `format()`.
#' @export
format.daffiz_comparison <- function(x, ...) {
  validate_comparison(x)
  s <- x$settings
  n_non_equal <- sum(x$cells$.match_type != "equal")
  c(
    sprintf("<daffiz_comparison> %s -> %s", s$x_name, s$y_name),
    sprintf(
      "Rows: %s / %s | measures: %d | cells: %s",
      format(s$n_row_x, big.mark = ","),
      format(s$n_row_y, big.mark = ","),
      length(s$compare),
      format(nrow(x$cells), big.mark = ",")
    ),
    sprintf("Identity: %s", fmt_names(s$by, 6L)),
    sprintf("Measures: %s", fmt_names(s$compare, 6L)),
    if (has_uncompared_duplicates(x)) {
      sprintf(
        "Result: incomplete (%s duplicated identity group(s) not compared)",
        format(nrow(x$duplicate_info), big.mark = ",")
      )
    } else if (n_non_equal == 0L) {
      "Result: all compared cells match"
    } else {
      sprintf("Result: %s cell(s) not equal", format(n_non_equal, big.mark = ","))
    }
  )
}

#' Compare two comparison objects
#'
#' Ignores the internal accessor cache, which is an environment and therefore
#' compares by reference. `identical()` on two `daffiz_comparison` objects is
#' always `FALSE` for that reason; use `all.equal()` or `isTRUE(all.equal(...))`
#' instead.
#'
#' @param target,current `daffiz_comparison` objects from [compare_dt()].
#' @param ... Passed to the underlying [all.equal()] methods.
#' @return `TRUE`, or a character vector describing the differences.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 1:2, amount = c(10, 25))
#' isTRUE(all.equal(compare_dt(x, y), compare_dt(x, y)))
all.equal.daffiz_comparison <- function(target, current, ...) {
  validate_comparison(target)
  if (!inherits(current, "daffiz_comparison")) {
    return("`current` is not a daffiz_comparison")
  }
  drop_cache <- function(cmp) {
    cmp$cache <- NULL
    unclass(cmp)
  }
  all.equal(drop_cache(target), drop_cache(current), ...)
}

#' @export
print.daffiz_comparison <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}

#' Do the compared tables match?
#'
#' `TRUE` when every aligned numeric cell is `equal` and neither table has rows
#' the other lacks.
#'
#' Named `is_matching()` rather than `matches()` to avoid shadowing
#' `tidyselect::matches()`, which is attached in most analysis sessions.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @return A single logical value.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 1:2, amount = c(10, 25))
#' is_matching(compare_dt(x, y))
is_matching <- function(cmp) {
  validate_comparison(cmp)
  !has_uncompared_duplicates(cmp) &&
    !any(cmp$cells$.match_type != "equal", na.rm = TRUE)
}

#' The canonical melted comparison table
#'
#' One row per aligned numeric cell. Columns are the identity columns, then
#' `.occurrence` (only when duplicate pairing was applied), `.row_x`, `.row_y`,
#' `.metric`, `.value_x`, `.value_y`, `.diff`, `.abs_diff`, `.rel_diff` and
#' `.match_type`.
#'
#' `.row_x` and `.row_y` are the one-based row numbers in the original inputs,
#' carried as data rather than as join keys. `NA` marks a row absent from that
#' side. `.match_type` is a factor with levels `equal`, `diff`, `x_only`,
#' `y_only`.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @return A `data.table` copy; modifying it does not affect `cmp`.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 1:2, amount = c(10, 25))
#' all_cells(compare_dt(x, y))
all_cells <- function(cmp) {
  validate_comparison(cmp)
  copy(cmp$cells)
}

#' Duplicated identity groups
#'
#' The identity groups that appeared more than once on either side, with their
#' per-side counts. Empty when identities were unique.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @return A `data.table` copy.
#' @export
#' @examples
#' x <- data.frame(id = c(1L, 1L), amount = c(10, 20))
#' cmp <- suppressWarnings(compare_dt(x, x))
#' duplicate_info(cmp)
duplicate_info <- function(cmp) {
  validate_comparison(cmp)
  copy(cmp$duplicate_info)
}
