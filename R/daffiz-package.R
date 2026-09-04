#' @keywords internal
"_PACKAGE"

#' @import data.table
NULL

.datatable.aware <- TRUE

# Column names the comparison machinery creates in its own results. Input
# columns may not use these names (preflight gate 2).
#
# `.in_x` / `.in_y` are transient side-presence helpers that exist only between
# the join and match classification, and are dropped before `cells` is stored.
# They are reserved anyway: a user column of the same name would silently
# corrupt the classification rather than fail.
daffiz_reserved_names <- c(
  ".row_x", ".row_y", ".occurrence", ".position", ".metric",
  ".value_x", ".value_y", ".diff", ".abs_diff", ".rel_diff",
  ".match_type", ".in_x", ".in_y"
)

# Stable factor levels for the `.match_type` column (design plan section 6.1).
daffiz_match_levels <- c("equal", "diff", "x_only", "y_only")

# Base types data.table can group and join on, and therefore the only types an
# identity column may have. Classed identities ride on these: factor is
# integer; Date, POSIXct, difftime and integer64 are double.
daffiz_joinable_types <- c("logical", "integer", "double", "character")

utils::globalVariables(c(
  ".row_x", ".row_y", ".occurrence", ".position", ".metric",
  ".value_x", ".value_y", ".diff", ".abs_diff", ".rel_diff",
  ".match_type", ".in_x", ".in_y", ".N", ".SD",
  # duplicate_info columns
  "n_x", "n_y",
  # Phase 2 accessor columns
  "column", "n_cells", "n_compared", "n_equal", "n_diff", "n_x_only",
  "n_y_only", "fraction_diff", "mean_diff", "mean_abs_diff", "rmse",
  "max_abs_diff", "p95_abs_diff", ".measure_order", "row_type",
  "n_columns", "diff_columns", "n_distinct", "values", ".identity_order",
  ".non_equal", ".magnitude", ".diff_columns",
  ".is_x_only", ".is_y_only", ".n_x_only", ".n_y_only",
  # Difference-map columns
  ".", ".plot_row_id", ".row_anchor", "abs_diff", "binned", "column_display",
  "fraction_mismatched", "fill_class", "match_type", "matched",
  "n_mismatched", "n_rows",
  "n_rows_shown", "n_rows_total", "row_bin", "row_display", "row_end",
  "row_label", "row_order", "row_rank", "row_start", "symbol", "value_x",
  "value_y"
))
