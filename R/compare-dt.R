# Label capture (decision D4): deparse only a bare symbol. A symbol is the one
# case where the expression genuinely is a name; calls and wrapper parameters
# produce code fragments that read as noise in a report header. magrittr's `.`
# is itself a symbol, so it needs the explicit exclusion. The native pipe needs
# no handling -- it rewrites to an ordinary call at parse time.
capture_label <- function(expr, fallback) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (!identical(nm, ".") && nzchar(nm)) return(nm)
  }
  fallback
}

# P1-11: warn before building a very large melted table. Never changes the
# result model (design plan section 11, decision D3).
warn_projected_size <- function(n_records, n_measures, x_name, y_name) {
  # Both arguments arrive as integers from `compare_dt()` (`nrow()` and
  # `length()`), and their product overflows `.Machine$integer.max` at about
  # 2.1e9 cells. Integer overflow yields NA, which turned the comparison below
  # into `if (NA)` -- so the guard against oversized comparisons was itself the
  # first thing to fail on one, with a message that named nothing.
  projected <- as.numeric(n_records) * as.numeric(n_measures)
  # Benchmarked on 2026-09-01: the retained table is almost exactly 64 bytes
  # per unique-key cell, while construction used substantially more. Ten
  # million warns early enough for batching or narrowing to be useful. See
  # benchmarks/results/threshold-validation-2026-09-01.md.
  threshold <- getOption("daffiz.max_cells", 1e7)
  if (!is.finite(threshold) || projected <= threshold) return(invisible(projected))

  # The cells table carries 5 doubles, 2 integers and a character metric per
  # row; ~64 bytes is a deliberate under-estimate of the working set.
  bytes <- projected * 64
  daffiz_warn("daffiz_warning_size",
    sprintf(paste0(
      "Comparison projects to %s melted cells (%s rows x %d measures), ",
      "about %s for the retained cell table alone.\n  Narrow with `compare=` ",
      "or `exclude=`, use `batch=` to reduce construction memory, or raise ",
      "options(daffiz.max_cells=) to silence this."),
      format(projected, big.mark = ","),
      format(n_records, big.mark = ","), n_measures,
      format(structure(bytes, class = "object_size"), units = "auto")),
    projected = projected, threshold = threshold,
    x_name = x_name, y_name = y_name)
  invisible(projected)
}

#' Compare the numeric measures of two tables
#'
#' Compares two tables that are expected to be almost the same, and reports
#' which numeric measures and which source rows differ. Rows are aligned on
#' identity columns; unclassed double columns are compared as measures.
#'
#' By default every column that is an unclassed double in *both* inputs is a
#' measure, and every other column forms the row identity. `Date` and `POSIXct`
#' columns are doubles internally but are *not* measures by default; they
#' remain identity columns. Use `by=` and `compare=` to override either role,
#' and `by = daffiz_row_number()` to align rows by position.
#'
#' Reading both inputs keeps role inference symmetric. A column that is integer
#' on one side and double on the other belongs to neither default role, so it
#' is reported as an identity type mismatch in either argument order; name it
#' in `compare=` to compare it as a measure.
#'
#' A value matches when
#' `abs(x - y) <= max(abs_tol, rel_tol * max(abs(x), abs(y)))`. The rule is
#' symmetric, so swapping the inputs negates the finite differences without
#' changing any match decision.
#'
#' The result holds one row per aligned numeric cell, so it has
#' `nrow x length(compare)` rows. A warning is issued above
#' `getOption("daffiz.max_cells")`, which defaults to 1e7.
#'
#' @param x,y Tables to compare. `data.frame`, `data.table` and tibble inputs
#'   are accepted; neither input is modified.
#' @param by Identity columns used to align rows. `NULL` (default) infers every
#'   column that is not a default measure. Pass [daffiz_row_number()] to align
#'   by position instead.
#' @param compare Numeric measures to compare. `NULL` (default) infers every
#'   column that is an unclassed double in both inputs and is not claimed by
#'   `by`.
#' @param exclude Columns dropped before roles are resolved.
#' @param abs_tol,rel_tol Absolute and relative tolerance. Either a single
#'   value applied to every measure, or a named vector using `.default` for
#'   unspecified columns, for example `c(.default = 0, amount = 0.01)`.
#' @param duplicate_keys What to do when an identity is duplicated. `"pair"`
#'   sorts deterministically and compares paired rows; `"report"` reports the
#'   ambiguous groups and excludes them from cell comparison; `"error"` stops.
#' @param x_name,y_name Labels used in reports. Default to the argument
#'   expressions when those are bare symbols, otherwise `"x"` and `"y"`.
#' @param batch Maximum number of measures melted and joined at once. `NULL`
#'   (default) processes every measure in one batch. Smaller values can reduce
#'   peak intermediate memory without changing or discarding result cells.
#'
#' @return A `daffiz_comparison` object. See [all_cells()].
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(1, 2, 3))
#' y <- data.frame(id = 1:3, amount = c(1, 2, 3.5))
#' cmp <- compare_dt(x, y)
#' is_matching(cmp)
#' all_cells(cmp)
compare_dt <- function(x, y,
                       by = NULL,
                       compare = NULL,
                       exclude = NULL,
                       abs_tol = 0,
                       rel_tol = 0,
                       duplicate_keys = c("pair", "report", "error"),
                       x_name = NULL,
                       y_name = NULL,
                       batch = NULL) {
  by_inferred <- is.null(by)
  compare_inferred <- is.null(compare)
  if (is.null(x_name)) x_name <- capture_label(substitute(x), "x")
  if (is.null(y_name)) y_name <- capture_label(substitute(y), "y")
  x_name <- as.character(x_name)[1L]
  y_name <- as.character(y_name)[1L]
  duplicate_keys <- match.arg(duplicate_keys)

  # Step 1 -- gates 1-4.
  kept <- preflight_columns(x, y, exclude, x_name, y_name)
  unknown_exclude <- attr(kept, "unknown_exclude", exact = TRUE)
  attributes(kept) <- NULL

  # Steps 2-3 -- copy both inputs and stamp original row numbers. This must
  # happen on the copies: assigning by reference to a caller's data.table would
  # mutate their data.
  snap_x <- as.data.table(x)
  snap_y <- as.data.table(y)
  if (data.table::is.data.table(x)) snap_x <- copy(snap_x)
  if (data.table::is.data.table(y)) snap_y <- copy(snap_y)
  snap_x[, .row_x := seq_len(.N)]
  snap_y[, .row_y := seq_len(.N)]
  # Step 2 -- align y to the validated column order of x.
  setcolorder(snap_y, c(intersect(names(snap_x), names(snap_y)),
                        setdiff(names(snap_y), names(snap_x))))

  # Step 4 -- working tables and role resolution (gates 5, 8).
  # These are column subsets, so they share their vectors with the snapshots.
  # That is safe for everything done here: adding `.position` and replacing a
  # promoted integer measure both swap a column pointer rather than writing
  # through it. Only `setorderv()` during duplicate pairing permutes vectors in
  # place, and `apply_duplicate_policy()` takes its own deep copy before doing
  # so -- copying here unconditionally doubled peak memory for a branch most
  # comparisons never take.
  wx <- snap_x[, c(kept, ".row_x"), with = FALSE]
  wy <- snap_y[, c(kept, ".row_y"), with = FALSE]
  roles <- resolve_roles(wx, wy, kept, by, compare)

  # Gates 6-7.
  preflight_types(wx, wy, roles$by, roles$compare, x_name, y_name)
  # Gate 9.
  abs_res <- resolve_tolerance(abs_tol, roles$compare, "abs_tol")
  rel_res <- resolve_tolerance(rel_tol, roles$compare, "rel_tol")
  batch_size <- resolve_batch(batch, length(roles$compare))

  if (roles$positional) {
    wx[, .position := seq_len(.N)]
    wy[, .position := seq_len(.N)]
  }

  # Promote integer measures to double. Narrowing is never performed; the
  # snapshots keep the original integer type.
  for (nm in roles$compare) {
    if (is.integer(wx[[nm]])) set(wx, j = nm, value = as.double(wx[[nm]]))
    if (is.integer(wy[[nm]])) set(wy, j = nm, value = as.double(wy[[nm]]))
  }

  # Step 5 -- duplicates.
  dup <- apply_duplicate_policy(wx, wy, roles$by, roles$compare,
                                duplicate_keys, x_name, y_name)
  wx <- dup$x
  wy <- dup$y
  occ <- if (dup$occurrence) ".occurrence" else character()

  # The aligned-record count is exact, and free: the duplicate scan already
  # counted every identity group on both sides. `max(nrow(wx), nrow(wy))`
  # under-projected by up to 2x, worst exactly when the keys are disjoint and
  # the warning matters most.
  warn_projected_size(dup$n_records, length(roles$compare), x_name, y_name)

  # Steps 6-9 -- melt, join and classify each measure batch. Every cell is
  # retained; batching changes only peak intermediate construction memory.
  cells <- build_cells_batched(
    wx, wy, roles$by, occ, roles$compare, abs_res, rel_res, batch_size
  )
  setcolorder(cells, c(roles$by, occ, ".row_x", ".row_y", ".metric",
                       ".value_x", ".value_y", ".diff", ".abs_diff",
                       ".rel_diff", ".match_type"))
  setkeyv(cells, c(roles$by, occ, ".metric"))

  # Step 10 -- assemble the comparison object.
  new_comparison(
    settings = list(
      by = roles$by, compare = roles$compare, ignored = roles$ignored,
      excluded = as.character(exclude), positional = roles$positional,
      unknown_exclude = unknown_exclude,
      by_inferred = by_inferred, compare_inferred = compare_inferred,
      abs_tol = abs_res, rel_tol = rel_res,
      batch = batch_size,
      duplicate_keys = duplicate_keys, occurrence = dup$occurrence,
      x_name = x_name, y_name = y_name,
      n_row_x = nrow(snap_x), n_row_y = nrow(snap_y)
    ),
    x_original = snap_x,
    y_original = snap_y,
    cells = cells,
    duplicate_info = dup$info
  )
}

# Match classification, in the order given by design plan section 6.2. The
# order matters: each rule may assume the earlier ones did not fire, so the
# tolerance comparison only ever sees two finite values.
classify_cells <- function(cells, abs_res, rel_res) {
  vx <- cells$.value_x
  vy <- cells$.value_y
  diff <- vx - vy
  abs_diff <- abs(diff)
  denom <- pmax(abs(vx), abs(vy))

  # `abs_res` and `rel_res` are both named over the resolved measures, so one
  # name lookup serves both. Indexing each by `.metric` directly did two
  # character hash lookups the length of the whole melted table.
  metric <- match(cells$.metric, names(abs_res))
  tol <- pmax(abs_res[metric], rel_res[metric] * denom)

  match_type <- fcase(
    is.na(cells$.in_x),                                  "y_only",
    is.na(cells$.in_y),                                  "x_only",
    is.na(vx) & is.na(vy),                               "equal",
    is.na(vx) | is.na(vy),                               "diff",
    is.infinite(vx) & is.infinite(vy) & vx == vy,        "equal",
    is.infinite(vx) | is.infinite(vy),                   "diff",
    abs_diff <= tol,                                     "equal",
    default =                                            "diff"
  )

  rel_diff <- fifelse(
    !is.finite(vx) | !is.finite(vy), NA_real_,
    fifelse(denom == 0, 0, abs_diff / denom)
  )

  cells[, `:=`(
    .diff = diff,
    .abs_diff = abs_diff,
    .rel_diff = rel_diff,
    .match_type = factor(match_type, levels = daffiz_match_levels)
  )]
  cells[, c(".in_x", ".in_y") := NULL]
  cells[]
}
