# Rows the key cannot tell apart ---------------------------------------------

#' Does the key identify a single row per measure?
#'
#' For a melted table the key is the id.vars, so this asks the question row_id
#' sidesteps: do the real columns actually tell the source rows apart?
#'
#' @param dt A keyed data.table.
#' @param along Names the measure column, which is part of the identity of a
#'   molten row but not of the key.
#' @return TRUE or FALSE, with the offending groups and their counts on the
#'   \code{"duplicates"} attribute.
#' @noRd
unique_by_key <- function(dt, along = "metric") {
  stopifnot(is.data.table(dt))

  k <- key(dt)
  if (!length(k)) {
    stop("`dt` has no key to check", call. = FALSE)
  }

  by <- c(k, intersect(along, names(dt)))
  dups <- dt[, .(n_rows = .N), by = by][n_rows > 1L][order(-n_rows)]

  ok <- nrow(dups) == 0L
  attr(ok, "duplicates") <- dups
  ok
}

#' Manufacture identity for rows a key cannot tell apart
#'
#' Variant 1 of the two answers to a non-unique key: adds a sequence number
#' within each key group so the key becomes unique, and re-keys the table to
#' include it. See \code{aggregate_by_key()} for the variant that invents
#' nothing.
#'
#' @param long A keyed data.table, modified by reference.
#' @param method \code{"rowid"} numbers rows in arrival order. Stable when
#'   values change, so a fuzzed row keeps its number; it moves only if rows are
#'   reordered. \code{"value_rank"} numbers rows by the measured value
#'   (\code{frank}, ties broken by arrival). Convenient, but the identity is
#'   derived from the data being compared: change a value and the numbering can
#'   flip, pairing the wrong rows in a later join. Use it when you mean "match
#'   the values in sorted order".
#' @param by The key columns to number within.
#' @param along Names the measure column, grouped on alongside the key.
#' @param value.name The value column, read by \code{method = "value_rank"}.
#' @param seq.name Name of the sequence column to add.
#' @return \code{long}, invisibly, re-keyed on \code{by} plus \code{seq.name}.
#' @noRd
disambiguate_by_key <- function(long, method = c("rowid", "value_rank"),
                                by = key(long), along = "metric",
                                value.name = "value", seq.name = "KEY_SEQ") {
  stopifnot(is.data.table(long))
  method <- match.arg(method)
  if (!length(by)) {
    stop("no key to disambiguate by", call. = FALSE)
  }

  grp <- c(by, intersect(along, names(long)))
  if (method == "rowid") {
    long[, (seq.name) := seq_len(.N), by = grp]
  } else {
    long[, (seq.name) := frank(get(value.name), ties.method = "first"), by = grp]
  }

  setkeyv(long, c(by, seq.name))
  invisible(long)
}

#' Summarize rows a key cannot tell apart
#'
#' Variant 2 of the two answers to a non-unique key: do not invent identity. One
#' row per key group and measure, holding the count and a description of the
#' values in it. Nothing is matched up row by row, so nothing can be matched up
#' wrongly. See \code{disambiguate_by_key()} for the variant that manufactures
#' an identity.
#'
#' NAs are counted rather than dropped, so a group that gained one is visible.
#'
#' @param long A keyed data.table.
#' @param by The key columns to group by.
#' @param along Names the measure column, grouped on alongside the key.
#' @param value.name The value column to summarize.
#' @param stats \code{"moments"} (the default) keeps order-invariant summaries:
#'   a count, the sum and sum of squares, and the range. No single one of these
#'   separates \{1,3\} from \{2,2\} -- together they do. Use it when you want to
#'   compare with a tolerance. \code{"exact"} keeps the whole multiset instead,
#'   as its values in hex float (\code{\%a}, which round-trips exactly) sorted
#'   and pasted. Two groups compare equal only if they hold exactly the same
#'   values. Use it for "same or not".
#' @return A data.table keyed by \code{by}, one row per key group and measure.
#' @noRd
aggregate_by_key <- function(long, by = key(long), along = "metric",
                             value.name = "value",
                             stats = c("moments", "exact")) {
  stopifnot(is.data.table(long))
  stats <- match.arg(stats)
  if (!length(by)) {
    stop("no key to aggregate by", call. = FALSE)
  }

  # min/max of an all-NA group is -Inf/Inf with a warning; report NA instead.
  safe <- function(f, v) if (all(is.na(v))) NA_real_ else f(v[!is.na(v)])

  grp <- c(by, intersect(along, names(long)))
  # The branch stays outside [: j must yield the same columns for every group.
  out <- if (stats == "moments") {
    long[, {
      v <- get(value.name)
      .(
        n_rows = .N,
        n_na = sum(is.na(v)),
        value_sum = safe(sum, v),
        value_sumsq = safe(function(z) sum(z^2), v),
        value_min = safe(min, v),
        value_max = safe(max, v)
      )
    }, by = grp]
  } else {
    long[, {
      v <- get(value.name)
      .(
        n_rows = .N,
        n_na = sum(is.na(v)),
        value_key = paste(sprintf("%a", sort(v, na.last = TRUE)), collapse = "|")
      )
    }, by = grp]
  }

  setkeyv(out, by)[]
}
