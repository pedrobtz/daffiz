library(data.table)

#' Summarize the columns of a table
#'
#' One row per column of \code{dt}: its name, class and number of unique values.
#'
#' @param dt A data.frame (or data.table).
#' @return A data.table with one row per column of \code{dt} and columns
#'   \code{colname}, \code{type} and \code{n_unique}.
#' @noRd
summarize_dt <- function(dt) {
  stopifnot(is.data.frame(dt))

  data.table(
    colname = names(dt),
    type = vapply(dt, function(x) class(x)[1L], character(1L)),
    n_unique = vapply(dt, uniqueN, integer(1L))
  )
}

#' Compare two tables column by column
#'
#' Compare a candidate \code{y} against the baseline \code{x}, column by column.
#' \code{y} may carry extra columns; columns missing on either side get NA.
#'
#' \code{over_pct} is the distinct values shared by both sides over the distinct
#' values seen in either, i.e. \code{n_common / n_union}. 100 means the two
#' columns draw on exactly the same set of values; a value \code{y} invents or
#' one it never uses both pull it down.
#'
#' The overlap is measured only where the two types agree. \code{\%in\%} coerces
#' before matching, so a numeric 1 counts as a match for the string \code{"1"}
#' and two columns that are not the same thing would report a full overlap. A
#' column whose type changed gets NA for \code{n_common}, \code{n_union} and
#' \code{over_pct}; cast it with \code{cast_dt()} first to compare the values.
#'
#' @param x The baseline table.
#' @param y The candidate table.
#' @return A data.table with one row per column of either side, holding the two
#'   types and whether they match, the two unique counts, and the value overlap
#'   (\code{n_common}, \code{n_union}, \code{over_pct}), which is NA unless the
#'   column is on both sides with the same type. Rows follow \code{x}'s column
#'   order, then whatever is only in \code{y}.
#' @noRd
compare_dt <- function(x, y) {
  stopifnot(is.data.frame(x), is.data.frame(y))

  out <- merge(
    summarize_dt(x),
    summarize_dt(y),
    by = "colname",
    all = TRUE,
    suffixes = c("_x", "_y"),
    sort = FALSE
  )

  # Values are only comparable when the types agree: %in% coerces before
  # matching, so a numeric 1 would "match" the string "1" and report a full
  # overlap for two columns that are not the same thing. The rest are left out
  # here and pick up NA from the all.x merge below.
  common <- out[!is.na(type_x) & !is.na(type_y) & type_x == type_y, colname]
  overlap <- rbindlist(c(
    list(data.table(
      colname = character(),
      n_common = integer(),
      n_union = integer(),
      over_pct = numeric()
    )),
    lapply(common, function(nm) {
      ux <- unique(x[[nm]])
      uy <- unique(y[[nm]])
      n_common <- sum(ux %in% uy)
      n_union <- length(ux) + length(uy) - n_common
      data.table(
        colname = nm,
        n_common = n_common,
        n_union = n_union,
        over_pct = if (n_union) 100 * n_common / n_union else NA_real_
      )
    })
  ))

  out <- merge(out, overlap, by = "colname", all.x = TRUE, sort = FALSE)
  out[, type_match := type_x == type_y]

  setcolorder(
    out,
    c(
      "colname",
      "type_x",
      "type_y",
      "type_match",
      "n_unique_x",
      "n_unique_y",
      "n_common",
      "n_union",
      "over_pct"
    )
  )
  # x's columns first, in x's order, then whatever is only in y.
  out[order(match(colname, c(names(x), setdiff(names(y), names(x)))))]
}

# The only column types cast_dt() will produce. A reference column of any other
# class is refused rather than guessed at.
CAST_TYPES <- c("character", "logical", "Date", "numeric", "integer")

# Types a candidate column may arrive as. POSIXct converts like the timestamp
# it is; a factor only becomes its labels. Neither is a target type.
CAST_FROM <- c(CAST_TYPES, "POSIXct", "factor")

# How normalize_dt() converts a column that is not already a baseline type.
# These are exactly the types the cast rules accept as sources but cannot
# produce, so a table converted by this map is expressible as a cast target.
#
# A timestamp becomes the calendar day it falls on, which is what comparing
# timestamped rows usually means; the time of day is dropped, so two stamps on
# the same day become the same value. Pass your own `to` when that is not what
# you want -- which day a stamp belongs to is the caller's decision, not ours.
NORMALIZE_TO <- c(POSIXct = "Date", factor = "character")

# Which conversions cast_dt() will attempt, and on what terms. This table is
# the authority: cast_value() consults it before doing any work, so what you
# see here is what the code does. `note` is the condition the values must meet
# (an unmet condition is an error, never a silent NA).
CAST_RULES <- rbindlist(lapply(CAST_FROM, function(from) {
  data.table(from = from, to = CAST_TYPES)
}))[from != to][, `:=`(
  allowed = TRUE,
  note = "as is"
)][
  from == "character",
  note := "parsed from the text"
][
  to == "integer" & from %in% c("numeric", "Date", "POSIXct", "character"),
  note := "as.integer(): truncated toward zero; must be within integer range"
][
  from %in% c("Date", "POSIXct") & to %in% c("numeric", "integer"),
  note := "days (Date) or seconds (POSIXct) since 1970-01-01"
][
  from %in% c("numeric", "integer") & to == "Date",
  note := "days since 1970-01-01"
][
  from == "POSIXct" & to == "Date",
  note := "calendar day in the stamp's own timezone; time of day dropped"
][
  from == "factor" & to == "character",
  note := "the labels, never the integer codes"
][
  # Refusals come last so the reason is not overwritten by a note above.
  # Nothing but text becomes a logical: 0/1 is a number that happens to look
  # like a flag, and reading it as one is a guess about intent.
  (to == "logical" & from != "character") |
    (from == "logical" & to == "Date"),
  `:=`(allowed = FALSE, note = "no sound conversion")
][
  # A factor's labels can be parsed like any text, but doing it implicitly is
  # how codes get mistaken for values. Ask for as.character() out loud instead.
  from == "factor" & to != "character",
  `:=`(allowed = FALSE, note = "convert the labels with as.character() first")
][order(match(from, CAST_FROM), match(to, CAST_TYPES))]

#' Look up the casting rule for one type pair
#'
#' The rule for one pair, or a 1-row NA table when there is none. The index is
#' built outside the \code{[ ]} on purpose: inside \code{[.data.table} the names
#' \code{from} and \code{to} would resolve to the table's own columns instead of
#' these arguments.
#'
#' @param from Source type, as \code{col_type()} names it.
#' @param to Target type, one of \code{CAST_TYPES}.
#' @return The matching one-row slice of \code{CAST_RULES}, with \code{allowed}
#'   NA when the pair has no rule.
#' @noRd
cast_rule <- function(from, to) {
  idx <- match(
    paste0(from, "->", to),
    paste0(CAST_RULES$from, "->", CAST_RULES$to)
  )
  CAST_RULES[idx]
}

#' The casting rules, for reading
#'
#' The same rules as \code{CAST_RULES}, as a from x to grid for reading at a
#' glance.
#'
#' @param wide If TRUE (the default), a grid of source types by target types,
#'   with \code{"yes"} for an allowed conversion, \code{"-"} for a refused one
#'   and \code{"="} where source and target are the same type. If FALSE, a copy
#'   of the long rules table including each rule's \code{note}.
#' @return A data.table.
#' @noRd
cast_rules <- function(wide = TRUE) {
  if (!wide) {
    return(copy(CAST_RULES))
  }
  w <- dcast(CAST_RULES, from ~ to, value.var = "allowed")
  for (nm in setdiff(names(w), "from")) {
    set(w, j = nm, value = fifelse(is.na(w[[nm]]), "=", fifelse(w[[nm]], "yes", "-")))
  }
  setcolorder(w, c("from", CAST_TYPES))[order(match(from, CAST_FROM))]
}

#' The type of a column, as the cast rules name it
#'
#' Resolves a vector to a single type name, checking the classes that carry
#' extra meaning (Date, POSIXct) before the underlying storage mode, so a Date
#' is not reported as a number.
#'
#' @param v A vector.
#' @return A length-1 character: one of \code{CAST_FROM}, or the vector's first
#'   class when it is something else entirely.
#' @noRd
col_type <- function(v) {
  if (inherits(v, "Date")) {
    "Date"
  } else if (inherits(v, "POSIXct")) {
    "POSIXct"
  } else if (is.character(v)) {
    "character"
  } else if (is.logical(v)) {
    "logical"
  } else if (is.integer(v)) {
    "integer"
  } else if (is.numeric(v)) {
    "numeric"
  } else {
    class(v)[1L]
  }
}

#' Cast one column, refusing anything lossy
#'
#' One column of \code{v} cast to \code{to}. Refuses anything lossy or ambiguous
#' instead of letting R's coercion rules quietly produce NAs or truncate.
#'
#' @param v The vector to convert.
#' @param to The target type, one of \code{CAST_TYPES}.
#' @param nm The column's name, used in error messages.
#' @param origin The type the value arrived as. Text and factors are converted
#'   in two hops, and the message should name the type the caller actually
#'   passed, not the intermediate one. Defaults to \code{col_type(v)}.
#' @param date_format The \code{strptime} format dates are read and written in,
#'   \code{"\%Y-\%m-\%d"} by default. Text is parsed with it and must match it
#'   exactly, and a Date written back out to text is formatted with it, so the
#'   two directions stay inverse to one another.
#' @return \code{v} converted to \code{to}, or an error naming the column, the
#'   pair of types, the reason and up to three offending values.
#' @noRd
cast_value <- function(v, to, nm, origin = NULL, date_format = "%Y-%m-%d") {
  if (!is.character(date_format) || length(date_format) != 1L || is.na(date_format)) {
    stop("`date_format` must be a single format string", call. = FALSE)
  }
  from <- col_type(v)
  if (is.null(origin)) {
    origin <- from
  }
  if (identical(from, to)) {
    return(v)
  }
  refuse <- function(why, vals = NULL) {
    msg <- sprintf("column %s: %s -> %s, %s", nm, origin, to, why)
    if (length(vals)) {
      msg <- paste0(msg, ": ", paste(head(unique(vals), 3L), collapse = ", "))
    }
    stop(msg, call. = FALSE)
  }
  if (!from %in% CAST_FROM) {
    refuse("unsupported source type")
  }

  rule <- cast_rule(from, to)
  if (!isTRUE(rule$allowed)) {
    refuse(if (is.na(rule$allowed)) "no rule for this pair" else rule$note)
  }

  # A factor means its labels, never its integer codes -- as.numeric() on a
  # factor silently hands back the codes -- so convert the labels instead.
  if (from == "factor") {
    return(cast_value(as.character(v), to, nm, origin = origin, date_format = date_format))
  }

  # Text is parsed to the underlying value first, so the rules below see numbers
  # rather than strings and "3.7" cannot truncate its way into an integer.
  if (from == "character" && to %in% c("logical", "integer", "numeric")) {
    parsed <- if (to == "logical") {
      ok <- c("TRUE", "FALSE", "T", "F", "true", "false", "True", "False")
      if (any(!is.na(v) & !v %in% ok)) {
        refuse("not a logical literal", v[!is.na(v) & !v %in% ok])
      }
      as.logical(v)
    } else {
      num <- suppressWarnings(as.numeric(v))
      if (any(is.na(num) & !is.na(v))) {
        refuse("not a number", v[is.na(num) & !is.na(v)])
      }
      num
    }
    return(if (to == "numeric") {
      parsed
    } else {
      cast_value(parsed, to, nm, origin = origin, date_format = date_format)
    })
  }

  out <- switch(to,
    # A Date goes back out through the same format it would be read in by, so
    # the two directions are inverse. Everything else is as.character()'s job.
    character = if (from == "Date") {
      format(v, date_format)
    } else if (from == "numeric") {
      txt <- as.character(v)
      # as.character() gives 15 significant digits, so two doubles that differ
      # further out than that land on the same text and would join as one key.
      # Reading the text back is what catches it. Non-finite values are left to
      # the comparison: they print and re-read exactly.
      off <- is.finite(v) & suppressWarnings(as.numeric(txt)) != v
      if (any(off)) {
        # Printed at full precision: at as.character()'s 15 digits the offending
        # value looks identical to the one it collides with.
        refuse("loses precision as text", format(v[off], digits = 17L))
      }
      txt
    } else {
      as.character(v)
    },
    logical = {
      if (!from %in% c("integer", "numeric")) {
        refuse("no sound conversion")
      }
      off <- v[!is.na(v) & !v %in% c(0, 1)]
      if (length(off)) {
        refuse("only 0/1 convert to logical", off)
      }
      v == 1
    },
    integer = {
      if (from == "logical") {
        as.integer(v)
      } else {
        # Date -> integer is days since 1970-01-01, which is exact.
        num <- as.numeric(v)
        off <- num[!is.na(num) & abs(num) > .Machine$integer.max]
        if (length(off)) {
          refuse("outside integer range", off)
        }
        as.integer(num)
      }
    },
    numeric = as.numeric(v),
    Date = {
      if (from == "POSIXct") {
        # Take the calendar day in the stamp's own zone, not UTC, so the date
        # matches the timestamp as it prints. Time of day is dropped.
        tz <- attr(v, "tzone")
        as.Date(v, tz = if (is.null(tz) || !nzchar(tz)) Sys.timezone() else tz)
      } else if (from %in% c("integer", "numeric")) {
        as.Date(v, origin = "1970-01-01")
      } else if (from == "character") {
        d <- suppressWarnings(as.Date(v, format = date_format))
        if (any(is.na(d) & !is.na(v))) {
          refuse(sprintf("not a date in format %s", date_format), v[is.na(d) & !is.na(v)])
        }
        # as.Date() stops at the end of the format and ignores whatever trails
        # it, so "2024-01-01junk" parses clean and "24-01-01" quietly becomes
        # the year 24. Formatting the parsed date back out is what separates a
        # date from a string that merely starts with one.
        off <- !is.na(d) & format(d, date_format) != v
        if (any(off)) {
          refuse(sprintf("not a canonical %s date", date_format), v[off])
        }
        d
      } else {
        refuse("no sound conversion")
      }
    },
    refuse("unsupported target type")
  )

  # Backstop: nothing may turn into NA on the way across.
  if (any(is.na(out) & !is.na(v))) {
    refuse("conversion produced NA", v[is.na(out) & !is.na(v)])
  }
  out
}

#' Cast a table's columns to a reference table's types
#'
#' Cast the columns of \code{y} to the types they have in the reference
#' \code{x}. Every column is validated and converted before any is written, so a
#' refusal leaves \code{y} exactly as it was.
#'
#' @param x The reference table, whose column types are the targets.
#' @param y The table to convert. Modified by reference, so it must be a
#'   data.table; columns only in \code{y} are left alone.
#' @param cols The columns to cast, by default those present in both tables.
#' @param date_format The format dates are read and written in; see
#'   \code{cast_value()}.
#' @return \code{y}, invisibly.
#' @noRd
cast_dt <- function(x, y, cols = intersect(names(x), names(y)),
                    date_format = "%Y-%m-%d") {
  stopifnot(is.data.frame(x), is.data.table(y))

  unknown <- setdiff(cols, intersect(names(x), names(y)))
  if (length(unknown)) {
    stop("column(s) missing from x or y: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  targets <- vapply(cols, function(nm) col_type(x[[nm]]), character(1L))
  bad <- targets[!targets %in% CAST_TYPES]
  if (length(bad)) {
    stop(
      "x has column(s) of unsupported type: ",
      paste(sprintf("%s (%s)", names(bad), bad), collapse = ", "),
      call. = FALSE
    )
  }

  values <- Map(
    function(nm, to) cast_value(y[[nm]], to, nm, date_format = date_format),
    cols, targets
  )
  for (nm in cols) {
    set(y, j = nm, value = values[[nm]])
  }
  invisible(y)
}

#' Convert a table to the baseline column types
#'
#' Casts every column that is not already one of \code{CAST_TYPES} -- the types
#' the comparison works in -- according to \code{to}. Columns that are already
#' baseline types are left exactly as they are.
#'
#' \code{diff_table()} takes its target types from the baseline \code{x}, so
#' \code{x} has to be expressible in them; it refuses a table that is not
#' rather than guessing. This is the function that makes one so, run explicitly
#' so the lossy conversions are the caller's choice and not a silent default.
#'
#' The default POSIXct to Date conversion is lossy: it keeps the calendar day
#' in the stamp's own timezone and drops the time, so two stamps on the same
#' day become one value. Pass \code{to = c(POSIXct = "character")} to keep the
#' full stamp instead, or convert that column yourself beforehand.
#'
#' @param dt A data.frame (or data.table).
#' @param to Named character: which baseline type each non-baseline type
#'   becomes. Defaults to \code{NORMALIZE_TO}. A column whose type is not named
#'   here is an error, not a guess.
#' @param cols The columns to consider, by default all of them.
#' @return A data.table. \code{dt} is never modified; a copy is made only when
#'   there is something to convert, so the result is \code{dt} itself when there
#'   is not, and callers must treat it as read-only.
#' @export
normalize_dt <- function(dt, to = NORMALIZE_TO, cols = names(dt)) {
  stopifnot(is.data.frame(dt))
  dt <- as.data.table(dt)

  unknown <- setdiff(cols, names(dt))
  if (length(unknown)) {
    stop("unknown column(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  types <- vapply(cols, function(nm) col_type(dt[[nm]]), character(1L))
  need <- cols[!types %in% CAST_TYPES]
  if (!length(need)) {
    return(dt)
  }

  unmapped <- need[!types[need] %in% names(to)]
  if (length(unmapped)) {
    stop(
      "no conversion given for column(s): ",
      paste(sprintf("%s (%s)", unmapped, types[unmapped]), collapse = ", "),
      "; name the target type in `to`",
      call. = FALSE
    )
  }

  out <- copy(dt)
  for (nm in need) {
    set(out, j = nm, value = cast_value(out[[nm]], to[[types[[nm]]]], nm))
  }
  out
}

#' Melt a table, choosing the measures by type
#'
#' Long form of \code{dt}, measuring the columns \code{measures} selects and
#' carrying the rest along as ids. Picking measures by class keeps this working
#' when columns are renamed. The measure names come back as character rather
#' than the factor \code{melt()} defaults to, so they compare and join like any
#' label.
#'
#' A row_id column is added first, so a molten row can be traced back to the row
#' of \code{dt} it came from. It is added on a copy, so \code{dt} is not
#' touched, and always written fresh: any column of that name is overwritten,
#' since callers' columns are upper case and this one has to be ours.
#'
#' @param dt A data.frame (or data.table).
#' @param measures \code{"numeric"} (the default) melts only the double columns.
#'   \code{"numeric+integer"} melts the integers too; \code{melt()} then widens
#'   the whole value column to double, since one column holds one type.
#' @param variable.name Name of the column holding the measure names.
#' @param value.name Name of the column holding the measured values.
#' @return A long data.table keyed by the id columns the caller's data supplies.
#'   row_id is carried along as a column but left out of the key, so the key is
#'   made of real values.
#' @noRd
melt_dt <- function(dt, measures = c("numeric", "numeric+integer"),
                    variable.name = "metric", value.name = "value") {
  stopifnot(is.data.frame(dt))
  measures <- match.arg(measures)

  types <- if (measures == "numeric") "numeric" else c("numeric", "integer")
  measure <- summarize_dt(dt)[type %in% types, colname]
  if (!length(measure)) {
    stop("no columns of type: ", paste(types, collapse = ", "), call. = FALSE)
  }

  # row_id ties every molten row back to the row it came from. Added on a copy,
  # so `dt` is not touched, and always written fresh: downstream reads it as a
  # presence marker, so it has to be ours and it has to be non-missing. The
  # lower-case name is what keeps it out of the caller's way.
  wide <- copy(as.data.table(dt))
  set(wide, j = "row_id", value = seq_len(nrow(wide)))
  setcolorder(wide, "row_id")

  id_vars <- setdiff(names(wide), measure)
  long <- melt.data.table(
    wide,
    id.vars = id_vars,
    measure.vars = measure,
    variable.name = variable.name,
    value.name = value.name,
    variable.factor = FALSE
  )
  setcolorder(long, "row_id")
  # Keyed by the id.vars the caller's data supplies -- row_id is carried along
  # as a column but left out of the key, so the key is made of real values.
  setkeyv(long, setdiff(id_vars, "row_id"))[]
}

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
  stopifnot(is.data.table(x), is.data.table(y))
  if (!length(on)) {
    stop("no columns to join on", call. = FALSE)
  }
  missing_cols <- setdiff(on, intersect(names(x), names(y)))
  if (length(missing_cols)) {
    stop("not in both tables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
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
  stopifnot(is.data.table(x), is.data.table(y))
  mode <- match.arg(mode)

  dry <- merge_dry_run(x, y, on = on)
  if (!dry$one_to_one) {
    stop(
      sprintf(
        paste0(
          "keys are not one to one: %d rows would become %d ",
          "(max fanout %d; %d duplicate key(s) in x, %d in y). ",
          "Use disambiguate_by_key() or aggregate_by_key() first."
        ),
        dry$rows_x, dry$est_merge_rows, dry$max_fanout, dry$dups_x, dry$dups_y
      ),
      call. = FALSE
    )
  }

  carried <- setdiff(names(y), on)
  targets <- fifelse(carried %in% names(x), paste0(carried, suffix), carried)

  # Suffixing a clashing name can land on a name x already uses, which would
  # overwrite it in place and duplicate it in a new table. Neither is a merge.
  taken <- unique(targets[targets %in% names(x)])
  if (length(taken) || anyDuplicated(targets)) {
    clash <- unique(c(taken, targets[duplicated(targets)]))
    stop(
      sprintf(
        "suffix \"%s\" would write over existing column(s): %s. Pick another suffix.",
        suffix, paste(clash, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (mode == "new") {
    out <- merge(x, y, by = on, all = all, suffixes = c("", suffix))
    return(out[])
  }

  x[y, on = on, (targets) := mget(paste0("i.", carried))]
  invisible(x)
}

#' Diff two tables, value by value
#'
#' The whole pipeline in one call: cast \code{y} to \code{x}'s types, melt both,
#' make sure the rows can be told apart, gate the join, and report what changed.
#'
#' @param x The baseline table.
#' @param y The candidate table.
#' @param mode \code{"in_place"} (the default) merges by reference into the
#'   melted copy of \code{x} that this function owns, so no second wide table is
#'   built. Rows that exist only in \code{y} are picked up with an anti-join and
#'   appended, which an update join would otherwise drop. \code{"new"} builds
#'   the merged table instead, as a full outer join.
#' @param measures Which columns to treat as measures, passed to
#'   \code{melt_dt()}: \code{"numeric"} (the default) or
#'   \code{"numeric+integer"}.
#' @param cast Whether to cast \code{y}'s columns to \code{x}'s types first.
#'   When FALSE, nothing is converted and the shared columns must already agree
#'   on type, which is checked up front. Either way \code{x} itself must be in
#'   the baseline types; see \code{normalize_dt()}.
#' @param duplicates What to do when the id columns do not tell rows apart.
#'   \code{"disambiguate"} numbers them by arrival order, \code{"aggregate"}
#'   compares the value multisets per key instead (a different, coarser answer:
#'   \code{n_rows} and \code{value_key} rather than \code{value_x}/\code{value_y}),
#'   \code{"error"} refuses.
#' @param date_format The format dates are read and written in when
#'   \code{cast = TRUE}; see \code{cast_value()}. Text must match it exactly,
#'   so a string that merely starts with a date is refused rather than
#'   truncated into one.
#' @param tolerance The largest absolute difference still counted as
#'   \code{"same"}.
#' @return A data.table keyed by the id columns and the measure name, holding
#'   each side's value, their \code{diff}, and a \code{status} of
#'   \code{"same"}, \code{"changed"}, \code{"only_x"} or \code{"only_y"}.
#' @export
diff_table <- function(x, y, mode = c("in_place", "new"),
                       measures = c("numeric", "numeric+integer"),
                       cast = TRUE,
                       duplicates = c("disambiguate", "aggregate", "error"),
                       date_format = "%Y-%m-%d",
                       tolerance = 0) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  mode <- match.arg(mode)
  measures <- match.arg(measures)
  duplicates <- match.arg(duplicates)

  xx <- as.data.table(x)
  yy <- copy(as.data.table(y))
  # x supplies the target types, so every one of its columns has to be a type
  # we can cast to. Refused rather than converted here: which day a timestamp
  # belongs to, or what a factor's codes mean, is the caller's call to make
  # with normalize_dt(). y needs no such check -- the rules already take
  # POSIXct and factor as sources, and casting y against x's own type is what
  # keeps Date <- POSIXct timezone-aware.
  x_types <- vapply(names(xx), function(nm) col_type(xx[[nm]]), character(1L))
  x_bad <- x_types[!x_types %in% CAST_TYPES]
  if (length(x_bad)) {
    stop(
      "x has column(s) of unsupported type: ",
      paste(sprintf("%s (%s)", names(x_bad), x_bad), collapse = ", "),
      "; convert them with normalize_dt() first",
      call. = FALSE
    )
  }

  if (cast) {
    cast_dt(xx, yy, date_format = date_format)
  } else {
    # Nothing is being aligned, so the types have to line up already. Without
    # this the failure surfaces much later, as a missing key or as data.table's
    # own "Incompatible join types", neither of which names the real problem.
    shared <- intersect(names(xx), names(yy))
    tx <- vapply(shared, function(nm) col_type(xx[[nm]]), character(1L))
    ty <- vapply(shared, function(nm) col_type(yy[[nm]]), character(1L))
    off <- shared[tx != ty]
    if (length(off)) {
      stop(
        "cast = FALSE, but x and y disagree on the type of: ",
        paste(sprintf("%s (%s vs %s)", off, tx[off], ty[off]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  lx <- melt_dt(xx, measures)
  ly <- melt_dt(yy, measures)

  if (duplicates == "aggregate") {
    lx <- aggregate_by_key(lx, stats = "exact")
    ly <- aggregate_by_key(ly, stats = "exact")
  } else if (!unique_by_key(lx) || !unique_by_key(ly)) {
    if (duplicates == "error") {
      stop(
        "the id columns do not tell rows apart; ",
        "use duplicates = \"disambiguate\" or \"aggregate\"",
        call. = FALSE
      )
    }
    disambiguate_by_key(lx, "rowid")
    disambiguate_by_key(ly, "rowid")
  }

  on <- c(key(lx), intersect("metric", names(lx)))

  if (mode == "in_place") {
    merge_dt(lx, ly, on = on, mode = "in_place")
    out <- lx
    # An update join keeps only x's rows, so fetch y's extras separately.
    extra <- ly[!out, on = on]
    if (nrow(extra)) {
      setnames(extra, setdiff(names(extra), on), paste0(setdiff(names(extra), on), "_y"))
      out <- rbind(out, extra, fill = TRUE)
    }
  } else {
    out <- merge_dt(lx, ly, on = on, mode = "new", all = TRUE)
  }

  # x's own columns kept their names through the merge; label them now.
  carried <- setdiff(names(ly), on)
  setnames(out, carried, paste0(carried, "_x"))

  # Membership is read from a column that is never NA in a row that exists
  # (row_id from the melt, n_rows from the aggregate), never from the value
  # itself: a value of NA says nothing about whether the row was there.
  present <- if ("row_id_x" %in% names(out)) {
    c("row_id_x", "row_id_y")
  } else {
    c("n_rows_x", "n_rows_y")
  }
  in_x <- !is.na(out[[present[1L]]])
  in_y <- !is.na(out[[present[2L]]])

  if ("value_x" %in% names(out)) {
    out[, diff := value_y - value_x]
    # NA on both sides is not a change; NA on one side is.
    # Equality comes first: Inf - Inf is NaN, so a tolerance test alone reports
    # two identical infinities as a change.
    unchanged <- out[, (is.na(value_x) & is.na(value_y)) |
      (!is.na(value_x) & !is.na(value_y) &
        (value_x == value_y | abs(value_y - value_x) <= tolerance))]
  } else {
    unchanged <- out[, (is.na(value_key_x) & is.na(value_key_y)) |
      (!is.na(value_key_x) & !is.na(value_key_y) & value_key_x == value_key_y)]
  }

  out[, status := fcase(
    !in_x & in_y, "only_y",
    in_x & !in_y, "only_x",
    unchanged, "same",
    default = "changed"
  )]

  setkeyv(out, on)[]
}
