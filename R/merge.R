# One-to-one merges ----------------------------------------------------------

#' Would these two tables join well?
#'
#' Answers with index-only joins: data.table returns row numbers and per-row
#' match counts, so nothing but integer vectors is allocated and the merged
#' table is never built.
#'
#' \code{est_merge_rows} is the exact size an inner join would have, and
#' \code{max_fanout} the most rows a single key would produce. Both are exact,
#' not estimates.
#'
#' \code{one_to_one} says whether the join is safe to run as it stands. When it
#' is FALSE a key repeats on one or both sides and the merge would multiply rows
#' out -- the case \code{allow.cartesian} exists to wave through, which we never
#' want. Give those rows an identity with \code{disambiguate_by_key()}, or stop
#' pretending they have one with \code{aggregate_by_key()}, then join.
#' \code{dups_x} / \code{dups_y} say which side needs the treatment.
#'
#' @param x The left table.
#' @param y The right table.
#' @param on The columns to join on, by default \code{x}'s key.
#' @return A one-row data.table: row and key counts per side, how many rows and
#'   what share of each side would match, \code{est_merge_rows},
#'   \code{max_fanout}, \code{dups_x}, \code{dups_y} and \code{one_to_one}.
#' @noRd
merge_dry_run <- function(x, y, on = key(x)) {
  if (!is.data.table(x) || !is.data.table(y)) {
    daffiz_abort("daffiz_error_input", "`x` and `y` must be data.tables")
  }
  if (!length(on)) {
    daffiz_abort("daffiz_error_keys", "no columns to join on")
  }
  missing_cols <- setdiff(on, intersect(names(x), names(y)))
  if (length(missing_cols)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0("not in both tables: ", paste(missing_cols, collapse = ", ")),
      columns = missing_cols
    )
  }

  # Count rows per key on each side, then join those counts: the join runs over
  # distinct keys, and the row counts of the merge fall out of nx * ny per key.
  # Nothing the size of the merged table is ever allocated.
  cx <- x[, .(n_x = .N), by = on]
  cy <- y[, .(n_y = .N), by = on]
  j <- cx[cy, on = on, nomatch = NULL]

  matched_x <- j[, sum(n_x)]
  matched_y <- j[, sum(n_y)]

  # Counts are integers and a wide join squares them, so 46341 rows on a key is
  # enough to overflow. Doubles carry the product exactly this side of 2^53.
  fanout <- if (nrow(j)) j[, as.double(n_x) * as.double(n_y)] else numeric(0)
  dups_x <- cx[n_x > 1L, .N]
  dups_y <- cy[n_y > 1L, .N]

  data.table(
    rows_x = nrow(x),
    rows_y = nrow(y),
    keys_x = nrow(cx),
    keys_y = nrow(cy),
    keys_common = nrow(j),
    matched_x = matched_x,
    matched_y = matched_y,
    pct_x = if (nrow(x)) 100 * matched_x / nrow(x) else NA_real_,
    pct_y = if (nrow(y)) 100 * matched_y / nrow(y) else NA_real_,
    est_merge_rows = sum(fanout),
    max_fanout = if (length(fanout)) max(fanout) else 0,
    dups_x = dups_x,
    dups_y = dups_y,
    # Fanout only sees keys both sides share, so a key that repeats on one side
    # alone leaves it at 1. That still breaks a one-to-one join under
    # all = TRUE, where the unmatched duplicates come through untouched, so the
    # gate asks about duplicates on the complete inputs too.
    one_to_one = (!length(fanout) || max(fanout) == 1) && dups_x == 0L && dups_y == 0L
  )
}

#' Merge two tables, but only one to one
#'
#' Merge \code{x} and \code{y}, but only when the keys line up exactly one to
#' one. The gate is \code{merge_dry_run()}: if any key would produce more than
#' one row the merge is refused, rather than waved through with
#' \code{allow.cartesian}. Give the rows an identity with
#' \code{disambiguate_by_key()}, or drop the pretence with
#' \code{aggregate_by_key()}, and try again.
#'
#' Columns of \code{y} whose name \code{x} already uses get \code{suffix};
#' \code{x}'s own names never move.
#'
#' @param x The left table.
#' @param y The right table.
#' @param on The columns to join on, by default \code{x}'s key.
#' @param mode \code{"new"} (the default) returns a new table, \code{x}
#'   untouched. \code{"in_place"} adds \code{y}'s columns to \code{x} by
#'   reference, matching on \code{on}. No second table is built, so a wide merge
#'   costs only the new columns. It is a left join by construction: every row of
#'   \code{x} stays, and rows with no match in \code{y} get NA.
#' @param all For \code{mode = "new"}, picks the join: FALSE keeps only matching
#'   rows, TRUE keeps everything. Does not apply to \code{"in_place"}.
#' @param suffix Appended to the names of \code{y}'s columns that clash with
#'   \code{x}'s.
#' @return For \code{mode = "new"}, the merged table. For \code{"in_place"},
#'   \code{x}, invisibly.
#' @noRd
merge_dt <- function(x, y, on = key(x), mode = c("new", "in_place"),
                     all = FALSE, suffix = "_y") {
  if (!is.data.table(x) || !is.data.table(y)) {
    daffiz_abort("daffiz_error_input", "`x` and `y` must be data.tables")
  }
  mode <- match.arg(mode)

  dry <- merge_dry_run(x, y, on = on)
  if (!dry$one_to_one) {
    # format(), not %d: the merged size and the fanout are doubles, precisely
    # because they can pass 2^31, and sprintf("%d") refuses a double that
    # large -- so the message for the join most worth refusing would itself
    # fail to build.
    n <- function(v) format(v, big.mark = ",", scientific = FALSE)
    daffiz_abort(
      "daffiz_error_keys",
      paste0(
        "keys are not one to one: ", n(dry$rows_x), " rows would become ",
        n(dry$est_merge_rows), " (max fanout ", n(dry$max_fanout), "; ",
        n(dry$dups_x), " duplicate key(s) in x, ", n(dry$dups_y), " in y). ",
        "Use disambiguate_by_key() or aggregate_by_key() first."
      ),
      dry_run = dry
    )
  }

  carried <- setdiff(names(y), on)
  targets <- fifelse(carried %in% names(x), paste0(carried, suffix), carried)

  # Suffixing a clashing name can land on a name x already uses, which would
  # overwrite it in place and duplicate it in a new table. Neither is a merge.
  taken <- unique(targets[targets %in% names(x)])
  if (length(taken) || anyDuplicated(targets)) {
    clash <- unique(c(taken, targets[duplicated(targets)]))
    daffiz_abort(
      "daffiz_error_columns",
      sprintf(
        "suffix \"%s\" would write over existing column(s): %s. Pick another suffix.",
        suffix, paste(clash, collapse = ", ")
      ),
      columns = clash
    )
  }

  if (mode == "new") {
    out <- merge(x, y, by = on, all = all, suffixes = c("", suffix))
    return(out[])
  }

  x[y, on = on, (targets) := mget(paste0("i.", carried))]
  invisible(x)
}
