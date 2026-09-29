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
  if (!is.data.table(dt)) {
    daffiz_abort("daffiz_error_input", "`dt` must be a data.table")
  }

  k <- key(dt)
  if (!length(k)) {
    daffiz_abort("daffiz_error_keys", "`dt` has no key to check")
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
#' Rows are numbered in arrival order. That is stable when values change, so a
#' changed row keeps its number; it moves only if rows are reordered, and then
#' the comparison reports changes rather than hiding them. Numbering by value
#' instead would pair rows so as to minimise the differences, and ranking within
#' each measure would pair a different source row for each measure.
#'
#' \code{diff_table()} numbers the wide table, one row per source row, before
#' melting, so a row keeps one number across all its measures by construction.
#' On a melted table the numbering runs within each measure (\code{along}).
#'
#' @param long A keyed data.table, modified by reference.
#' @param by The key columns to number within.
#' @param along Names the measure column, grouped on alongside the key.
#' @param seq.name Name of the sequence column to add.
#' @return \code{long}, invisibly, re-keyed on \code{by} plus \code{seq.name}.
#' @noRd
disambiguate_by_key <- function(long, by = key(long), along = "metric",
                                seq.name = "key_seq") {
  if (!is.data.table(long)) {
    daffiz_abort("daffiz_error_input", "`long` must be a data.table")
  }
  if (!length(by)) {
    daffiz_abort("daffiz_error_keys", "no key to disambiguate by")
  }

  grp <- c(by, intersect(along, names(long)))
  long[, (seq.name) := seq_len(.N), by = grp]

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
#' Without a value column -- a table with no measures -- only the rows are
#' counted, which compares the two tables as multisets of rows.
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
#' @param nan_is_na For \code{"exact"}: whether NaN and NA are the same missing
#'   value. \code{\%a} writes them differently, so NaN is mapped to NA first.
#' @return A data.table keyed by \code{by}, one row per key group and measure.
#' @noRd
aggregate_by_key <- function(long, by = key(long), along = "metric",
                             value.name = "value",
                             stats = c("moments", "exact"),
                             nan_is_na = TRUE) {
  if (!is.data.table(long)) {
    daffiz_abort("daffiz_error_input", "`long` must be a data.table")
  }
  stats <- match.arg(stats)
  if (!length(by)) {
    daffiz_abort("daffiz_error_keys", "no key to aggregate by")
  }

  grp <- c(by, intersect(along, names(long)))
  if (!value.name %in% names(long)) {
    return(setkeyv(long[, .(n_rows = .N), by = grp], by)[])
  }

  # min/max of an all-NA group is -Inf/Inf with a warning; report NA instead.
  safe <- function(f, v) if (all(is.na(v))) NA_real_ else f(v[!is.na(v)])

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
      # `+ 0` turns -0 into 0, which `==` already treats as equal but `%a`
      # writes differently. The encoded strings are sorted, not the values:
      # sort() leaves NA and NaN in arrival order among themselves, so the
      # same multiset could otherwise encode two ways. Radix sorting is in the
      # C locale, so the encoding does not depend on the session either.
      v <- get(value.name) + 0
      if (nan_is_na) {
        v[is.nan(v)] <- NA_real_
      }
      .(
        n_rows = .N,
        n_na = sum(is.na(v)),
        value_key = paste(sort(sprintf("%a", v), method = "radix"), collapse = "|")
      )
    }, by = grp]
  }

  setkeyv(out, by)[]
}
