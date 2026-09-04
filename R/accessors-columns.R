# Column accessors ------------------------------------------------------------

build_column_summary <- function(cmp) {
  cells <- cmp$cells
  stats <- cells[, {
    paired <- is_finite_pair(.value_x, .value_y)
    finite_diff <- .diff[paired]
    finite_abs <- .abs_diff[paired]
    list(
      n_cells = .N,
      n_compared = sum(.match_type %in% c("equal", "diff")),
      n_equal = sum(.match_type == "equal"),
      n_diff = sum(.match_type == "diff"),
      n_x_only = sum(.match_type == "x_only"),
      n_y_only = sum(.match_type == "y_only"),
      mean_diff = if (length(finite_diff)) mean(finite_diff) else NA_real_,
      mean_abs_diff = if (length(finite_abs)) mean(finite_abs) else NA_real_,
      rmse = scaled_rmse(finite_diff),
      max_abs_diff = if (length(finite_abs)) max(finite_abs) else NA_real_,
      p95_abs_diff = if (length(finite_abs)) {
        as.numeric(stats::quantile(finite_abs, 0.95, names = FALSE))
      } else {
        NA_real_
      }
    )
  }, by = .metric]

  setnames(stats, ".metric", "column")
  out <- merge(
    data.table(column = cmp$settings$compare),
    stats,
    by = "column",
    all.x = TRUE,
    sort = FALSE
  )

  count_columns <- c(
    "n_cells", "n_compared", "n_equal", "n_diff", "n_x_only", "n_y_only"
  )
  for (nm in count_columns) {
    set(out, i = which(is.na(out[[nm]])), j = nm, value = 0L)
    set(out, j = nm, value = as.integer(out[[nm]]))
  }
  out[, fraction_diff := fifelse(n_compared > 0L, n_diff / n_compared, NA_real_)]
  setcolorder(out, c(
    "column", "n_cells", "n_compared", "n_equal", "n_diff", "n_x_only",
    "n_y_only", "fraction_diff", "mean_diff", "mean_abs_diff", "rmse",
    "max_abs_diff", "p95_abs_diff"
  ))
  out[, .measure_order := match(column, cmp$settings$compare)]
  setorderv(out, ".measure_order")
  out[, .measure_order := NULL]
  out[]
}

#' Summary statistics by numeric measure
#'
#' Produces exact counts and finite-difference statistics for every resolved
#' measure, including measures with no joined cells.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#'
#' @return A `data.table` with one row per measure in input-column order.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27))
#' column_summary(compare_dt(x, y))
column_summary <- function(cmp) {
  out <- cached_comparison_value(
    cmp,
    "column_summary",
    function() build_column_summary(cmp)
  )
  copy(out)
}

#' Numeric measures with differences
#'
#' Filters [column_summary()] to measures containing `diff`, `x_only`, or
#' `y_only` cells and ranks the most affected measures first.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#'
#' @return A filtered and ranked `data.table`.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20), score = c(1, 2))
#' y <- data.frame(id = 1:2, amount = c(10, 25), score = c(1, 4))
#' diff_columns(compare_dt(x, y))
diff_columns <- function(cmp) {
  validate_comparison(cmp)
  out <- column_summary(cmp)[n_diff > 0L | n_x_only > 0L | n_y_only > 0L]
  out[, .measure_order := match(column, cmp$settings$compare)]
  setorderv(
    out,
    c("n_diff", "fraction_diff", "max_abs_diff", ".measure_order"),
    order = c(-1L, -1L, -1L, 1L),
    na.last = TRUE
  )
  out[, .measure_order := NULL]
  out[]
}
