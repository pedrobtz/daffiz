# Summaries of a comparison ---------------------------------------------------

#' Summarize a comparison by measure
#'
#' Counts of each status for every measure of a [diff_table()] result, and,
#' for the usual one-row-per-value shape, how large the differences are.
#'
#' The magnitudes are taken over every value compared on both sides where both
#' source values are finite. They are filtered on the source values, not on
#' `diff`: two finite values can differ by more than the largest double, and
#' filtering on the difference would drop exactly that value and report the
#' largest difference in the table as 0. `rmse` is computed with scaling, so
#' squaring cannot overflow either.
#'
#' @param d A result of [diff_table()].
#' @return A `data.table` with one row per measure, in the benchmark's column
#'   order (a single row when there are no measures): `metric`, `n`,
#'   `n_same`, `n_changed`, `n_only_x`, `n_only_y`, and for the `"cells"`
#'   shape `max_abs_diff`, `mean_abs_diff` and `rmse` (`NA` for a measure
#'   with no finite pair).
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30), rate = c(1, 1, 1))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27), rate = c(1, 1, 1))
#' diff_summary(diff_table(x, y))
diff_summary <- function(d) {
  shape <- attr(d, "shape")
  if (!is.data.table(d) || is.null(shape) || !"status" %in% names(d)) {
    daffiz_abort("daffiz_error_input", "`d` must be a result of diff_table()")
  }

  grp <- intersect("metric", names(d))
  counts <- d[, .(
    n = .N,
    n_same = sum(status == "same"),
    n_changed = sum(status == "changed"),
    n_only_x = sum(status == "only_x"),
    n_only_y = sum(status == "only_y")
  ), by = grp]
  if (!length(grp) && !nrow(counts)) {
    counts <- data.table(n = 0L, n_same = 0L, n_changed = 0L, n_only_x = 0L, n_only_y = 0L)
  }

  if (shape == "cells") {
    sizes <- d[, {
      z <- (value_y - value_x)[is_finite_pair(value_x, value_y)]
      .(
        max_abs_diff = if (length(z)) max(abs(z)) else NA_real_,
        mean_abs_diff = if (length(z)) mean(abs(z)) else NA_real_,
        rmse = scaled_rmse(z)
      )
    }, by = metric]
    counts <- counts[sizes, on = "metric"]
  }

  if (length(grp)) {
    order_of <- attr(d, "name_map")$column
    counts <- counts[order(match(metric, order_of))]
  }
  counts[]
}

# A value contributes to the magnitudes when both *source values* are finite.
# Filtering on the derived difference instead would discard a genuine,
# enormous difference whose subtraction overflowed to Inf.
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
