# Casting to the reference types ---------------------------------------------

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
