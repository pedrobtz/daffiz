# Cell accessors --------------------------------------------------------------

validate_measure_filter <- function(columns, measures) {
  if (is.null(columns)) return(measures)
  if (!is.character(columns) || anyNA(columns) || any(columns == "")) {
    daffiz_abort(
      "daffiz_error_columns",
      "`columns` must be a character vector of compared measure names."
    )
  }
  unknown <- setdiff(columns, measures)
  if (length(unknown)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0(
        "`columns` includes measure(s) not present in this comparison: ",
        fmt_names(unknown), ".\n  Compared measures: ", fmt_names(measures)
      ),
      columns = unknown
    )
  }
  unique(columns)
}

#' Numeric cells that do not match
#'
#' Returns the auditable cell-level records classified as `diff`, `x_only`, or
#' `y_only`. Use `columns` to restrict the result to selected measures.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param columns Optional character vector of compared measure names.
#'
#' @return A `data.table` copy with the same schema as [all_cells()].
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20), score = c(1, 2))
#' y <- data.frame(id = 1:2, amount = c(10, 25), score = c(1, 3))
#' cmp <- compare_dt(x, y)
#' diff_cells(cmp)
#' diff_cells(cmp, columns = "amount")
diff_cells <- function(cmp, columns = NULL) {
  validate_comparison(cmp)
  columns <- validate_measure_filter(columns, cmp$settings$compare)
  # `[` already returns a fresh table, so no further copy is needed to keep
  # the cached cells private.
  cmp$cells[.match_type != "equal" & .metric %chin% columns]
}
