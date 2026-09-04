# Row accessors ---------------------------------------------------------------

row_group_columns <- function(cmp) {
  c(
    cmp$settings$by,
    if (cmp$settings$occurrence) ".occurrence" else character(),
    ".row_x",
    ".row_y"
  )
}

row_summary_columns <- function(groups) {
  c(groups, "row_type", "n_columns", "n_diff", "diff_columns",
    "mean_abs_diff", "max_abs_diff")
}

empty_row_summary <- function(cmp, groups) {
  out <- cmp$cells[0L, groups, with = FALSE]
  out[, `:=`(
    row_type = factor(character(), levels = daffiz_match_levels),
    n_columns = integer(),
    n_diff = integer(),
    diff_columns = vector("list", 0L),
    mean_abs_diff = numeric(),
    max_abs_diff = numeric()
  )]
  out[]
}

# `affected_only = TRUE` restricts the grouping to records holding at least one
# non-equal cell. Every report, ranking and count path wants exactly that set,
# and it keeps `print()` off the whole-table grouping: summarising the
# all-equal majority and then discarding it made the default report cost an
# order of magnitude more than the comparison it describes.
#
# The aggregates are plain `sum`/`mean`/`max` over precomputed columns so that
# data.table's optimised grouping applies. The previous multi-statement `j`,
# with a per-group list column and three `any()` scans, forced the slow
# R-level path and allocated one character vector per record.
build_row_summary <- function(cmp, affected_only = FALSE) {
  cells <- cmp$cells
  groups <- row_group_columns(cmp)
  if (!nrow(cells)) return(empty_row_summary(cmp, groups))

  work <- cells[, c(groups, ".metric", ".match_type", ".value_x", ".value_y",
                    ".abs_diff"), with = FALSE]
  work[, `:=`(
    .non_equal = .match_type != "equal",
    .is_x_only = .match_type == "x_only",
    .is_y_only = .match_type == "y_only",
    .magnitude = fifelse(
      is_finite_pair(.value_x, .value_y), .abs_diff, NA_real_
    )
  )]
  if (affected_only) {
    keys <- unique(work[(.non_equal), groups, with = FALSE])
    if (!nrow(keys)) return(empty_row_summary(cmp, groups))
    work <- work[keys, on = groups]
  }

  out <- suppressWarnings(work[, list(
    n_columns = .N,
    n_diff = sum(.non_equal),
    .n_x_only = sum(.is_x_only),
    .n_y_only = sum(.is_y_only),
    mean_abs_diff = mean(.magnitude, na.rm = TRUE),
    max_abs_diff = max(.magnitude, na.rm = TRUE)
  ), by = groups])
  # Groups with no finite pair aggregate to NaN / -Inf rather than NA.
  out[is.nan(mean_abs_diff), mean_abs_diff := NA_real_]
  out[is.infinite(max_abs_diff) & max_abs_diff < 0, max_abs_diff := NA_real_]

  out[, row_type := factor(
    fcase(
      .n_x_only > 0L, "x_only",
      .n_y_only > 0L, "y_only",
      n_diff > 0L, "diff",
      default = "equal"
    ),
    levels = daffiz_match_levels
  )]
  out[, c(".n_x_only", ".n_y_only") := NULL]

  # One shared empty vector for the unaffected records, overwritten in place
  # only where a record actually has changed measures.
  out[, diff_columns := rep(list(character()), .N)]
  changed <- work[(.non_equal), list(.diff_columns = list(.metric)), by = groups]
  if (nrow(changed)) {
    set(out, i = out[changed, on = groups, which = TRUE],
        j = "diff_columns", value = changed$.diff_columns)
  }

  setcolorder(out, row_summary_columns(groups))
  out[]
}

# The affected-record summary, ranked. Cached separately from the full
# `row_summary()` so that reporting never pays for the all-equal records.
affected_row_summary <- function(cmp) {
  cached_comparison_value(cmp, "affected_row_summary", function() {
    out <- build_row_summary(cmp, affected_only = TRUE)
    setorderv(
      out,
      c("n_diff", "max_abs_diff", ".row_x", ".row_y"),
      order = c(-1L, -1L, 1L, 1L),
      na.last = TRUE
    )
    out[]
  })
}

#' Summary statistics by aligned record
#'
#' Groups the canonical cell table into one row per aligned or side-only source
#' record. `n_diff` counts all non-equal measures for the record.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#'
#' @return A `data.table` with identity columns, source row numbers, row type,
#'   difference counts, changed measure names, and difference magnitudes.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 30))
#' row_summary(compare_dt(x, y))
row_summary <- function(cmp) {
  out <- cached_comparison_value(
    cmp,
    "row_summary",
    function() build_row_summary(cmp)
  )
  copy(out)
}

paired_rows <- function(cmp) {
  out <- copy(affected_row_summary(cmp))

  ix <- out$.row_x
  iy <- out$.row_y
  # Named `col_*` rather than `x_name`/`y_name`: everywhere else in the package
  # those two mean the *table* labels carried in `settings`, not column names.
  for (nm in cmp$settings$compare) {
    col_x <- paste0(nm, "_x")
    col_y <- paste0(nm, "_y")
    if (col_x %in% names(out)) col_x <- paste0(".measure_", col_x)
    if (col_y %in% names(out)) col_y <- paste0(".measure_", col_y)
    set(out, j = col_x, value = cmp$x_original[[nm]][ix])
    set(out, j = col_y, value = cmp$y_original[[nm]][iy])
  }
  out[]
}

#' Records containing numeric differences
#'
#' `view = "summary"` returns the filtered row summary. `"paired"` adds the
#' original measure values with `_x` and `_y` suffixes. `"x"` and `"y"`
#' recover affected original rows from the requested side.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param view One of `"summary"`, `"paired"`, `"x"`, or `"y"`.
#'
#' @return A `data.table` appropriate to the requested view.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 1:2, amount = c(10, 25))
#' cmp <- compare_dt(x, y)
#' diff_rows(cmp)
#' diff_rows(cmp, view = "paired")
diff_rows <- function(cmp, view = c("summary", "paired", "x", "y")) {
  validate_comparison(cmp)
  view <- match.arg(view)
  if (view == "x") return(original_rows(cmp, "x", "affected"))
  if (view == "y") return(original_rows(cmp, "y", "affected"))
  # `paired_rows()` already works on, and returns, its own copy.
  if (view == "paired") return(paired_rows(cmp))
  copy(affected_row_summary(cmp))
}

validate_side <- function(side) match.arg(side, c("x", "y"))

indices_for_type <- function(cmp, side, type) {
  row_column <- paste0(".row_", side)
  cells <- cmp$cells
  keep <- switch(
    type,
    affected = cells$.match_type != "equal",
    changed = cells$.match_type == "diff",
    only = cells$.match_type == paste0(side, "_only")
  )
  indices <- cells[[row_column]][keep]
  sort(unique(as.integer(indices[!is.na(indices)])))
}

#' Original row numbers affected by differences
#'
#' Returns source indices for matched numeric changes and rows present on only
#' one side. Repeated melted cells never duplicate an index.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param side Either `"x"` or `"y"`.
#'
#' @return A sorted, unique integer vector of one-based source row numbers.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 1:2, amount = c(10, 25))
#' cmp <- compare_dt(x, y)
#' diff_indices(cmp, "x")
diff_indices <- function(cmp, side = c("x", "y")) {
  validate_comparison(cmp)
  side <- validate_side(side)
  indices_for_type(cmp, side, "affected")
}

#' Recover rows from the original input snapshots
#'
#' The returned rows preserve the original column classes and values, and add
#' the appropriate `.row_x` or `.row_y` traceability column.
#'
#' Column *order* follows `x` on both sides: preflight aligns `y` to the
#' validated column order of `x` (design plan section 4.1, gate 4), so the two
#' sides stay directly comparable. For `side = "x"` that is also the input's
#' own order.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param side Either `"x"` or `"y"`.
#' @param type Which rows to recover: all affected rows, matched changed rows,
#'   side-only rows, or all original rows.
#'
#' @return A `data.table` copied from the stored input snapshot.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 2:3, amount = c(20, 30))
#' cmp <- compare_dt(x, y)
#' original_rows(cmp, "x")
#' original_rows(cmp, "y", type = "only")
original_rows <- function(
    cmp,
    side = c("x", "y"),
    type = c("affected", "changed", "only", "all")) {
  validate_comparison(cmp)
  side <- validate_side(side)
  type <- match.arg(type)
  snapshot <- if (side == "x") cmp$x_original else cmp$y_original
  indices <- if (type == "all") {
    seq_len(nrow(snapshot))
  } else {
    indices_for_type(cmp, side, type)
  }
  copy(snapshot[indices])
}

#' Rows present only in the first input
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @return A deduplicated `data.table` of original `x` rows.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 2:3, amount = c(20, 30))
#' x_only(compare_dt(x, y))
x_only <- function(cmp) original_rows(cmp, "x", "only")

#' Rows present only in the second input
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @return A deduplicated `data.table` of original `y` rows.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, amount = c(10, 20))
#' y <- data.frame(id = 2:3, amount = c(20, 30))
#' y_only(compare_dt(x, y))
y_only <- function(cmp) original_rows(cmp, "y", "only")
