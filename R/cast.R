# Casting to the reference types ---------------------------------------------

#' Cast one column to a target type
#'
#' One column of \code{v} cast to \code{to}. Refuses anything ambiguous instead
#' of letting R's coercion rules quietly produce NAs. The one loss it will
#' accept is a truncation -- a fraction dropped on the way to a whole integer
#' or a whole day -- and only when \code{truncate} says so.
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
#' @param truncate Whether a cast may drop a fraction: a double to an integer
#'   (toward zero), a number to a Date (to the day it prints as), a timestamp
#'   to a Date (the time of day). When it does, the result carries a
#'   \code{"truncated"} attribute: the count and up to three examples. When
#'   FALSE, the cast is refused instead.
#' @param nan_is_na Whether NaN and NA are the same missing value. NaN has no
#'   integer or Date form and becomes NA on the way; with FALSE that would hide
#'   a difference, so the cast is refused instead.
#' @return \code{v} converted to \code{to}, or an error naming the column, the
#'   pair of types, the reason and up to three offending values.
#' @noRd
cast_value <- function(v, to, nm, origin = NULL, date_format = "%Y-%m-%d",
                       truncate = TRUE, nan_is_na = TRUE) {
  if (!is.character(date_format) || length(date_format) != 1L || is.na(date_format)) {
    daffiz_abort("daffiz_error_input", "`date_format` must be a single format string")
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
    daffiz_abort("daffiz_error_cast", msg, column = nm, from = origin, to = to)
  }
  if (!from %in% CAST_FROM) {
    refuse("unsupported source type")
  }

  rule <- cast_rule(from, to)
  if (!isTRUE(rule$allowed)) {
    refuse(if (is.na(rule$allowed)) "no rule for this pair" else rule$note)
  }

  hop <- function(value) {
    cast_value(value, to, nm,
      origin = origin, date_format = date_format,
      truncate = truncate, nan_is_na = nan_is_na
    )
  }

  # A factor means its labels, never its integer codes -- as.numeric() on a
  # factor silently hands back the codes -- so convert the labels instead.
  if (from == "factor") {
    return(hop(as.character(v)))
  }

  # Text is parsed to the underlying value first, so the rules below see numbers
  # rather than strings, and "3.7" meets the same truncation rule as 3.7.
  if (from == "character" && to %in% c("logical", "integer", "numeric")) {
    parsed <- if (to == "logical") {
      ok <- c("TRUE", "FALSE", "T", "F", "true", "false", "True", "False")
      if (any(!is.na(v) & !v %in% ok)) {
        refuse("not a logical literal", v[!is.na(v) & !v %in% ok])
      }
      as.logical(v)
    } else {
      # as.numeric() also accepts surrounding whitespace, hex ("0x10") and
      # "Inf"/"NaN". All of them are numbers, so they are let through.
      num <- suppressWarnings(as.numeric(v))
      if (any(is.na(num) & !is.na(v))) {
        refuse("not a number", v[is.na(num) & !is.na(v)])
      }
      num
    }
    return(if (to == "numeric") parsed else hop(parsed))
  }

  # NaN has no integer or Date form, so it becomes NA on the way. When NaN and
  # NA are the same missing value that loses nothing; when they are not, it
  # would turn a difference into a match.
  if (!nan_is_na && to %in% c("integer", "Date") && is.double(v) && any(is.nan(v))) {
    refuse("NaN has no such form (nan_is_na = FALSE)")
  }

  # `lost` marks the values a cast truncated; `why` says what was dropped.
  lost <- NULL
  why <- NULL
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
    integer = {
      if (from == "logical") {
        as.integer(v)
      } else {
        # Date -> integer is days since 1970-01-01, POSIXct -> integer seconds.
        num <- as.numeric(v)
        off <- num[!is.na(num) & abs(num) > .Machine$integer.max]
        if (length(off)) {
          refuse("outside integer range", off)
        }
        lost <- !is.na(num) & num != trunc(num)
        why <- "not a whole number"
        as.integer(num)
      }
    },
    numeric = as.numeric(v),
    Date = {
      if (from == "POSIXct") {
        # Take the calendar day in the stamp's own zone, not UTC, so the date
        # matches the timestamp as it prints. A stamp with no zone is read as
        # UTC rather than as the session's zone, so the answer does not depend
        # on the machine it runs on.
        tz <- attr(v, "tzone")
        tz <- if (is.null(tz) || !nzchar(tz[1L])) "UTC" else tz[1L]
        d <- as.Date(v, tz = tz)
        midnight <- as.numeric(as.POSIXct(format(d), tz = tz))
        # Where a zone skips midnight (a DST change at 00:00) there is no
        # midnight to match, and any stamp that day has a time of day.
        lost <- !is.na(v) & (is.na(midnight) | midnight != as.numeric(v))
        why <- "has a time of day"
        d
      } else if (from %in% c("integer", "numeric")) {
        num <- as.numeric(v)
        # A fractional day prints as the day it falls in but does not equal
        # it, so it is floored to that day rather than kept fractional.
        lost <- is.finite(num) & num != floor(num)
        why <- "not a whole day"
        as.Date(floor(num), origin = "1970-01-01")
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
        # The round trip alone cannot catch a short year on every platform:
        # glibc writes year 24 as "24" under %Y where macOS writes "0024", so
        # on Linux "24-01-01" formats back to itself. No real date precedes
        # the year 1000, and a two- or three-digit year lands exactly there,
        # so those are refused too, reading the year from the date itself.
        off <- off | (!is.na(d) & as.POSIXlt(d)$year + 1900L < 1000L)
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

  if (!is.null(lost) && any(lost)) {
    if (!truncate) {
      refuse(paste0(why, " (truncate = FALSE)"), v[lost])
    }
    # One example per distinct value: a repeated value says nothing new.
    at <- which(lost)
    shown <- head(at[!duplicated(v[at])], 3L)
    attr(out, "truncated") <- list(
      n = sum(lost),
      examples = vapply(shown, function(i) {
        paste(format(v[i]), "->", format(out[i]))
      }, character(1L))
    )
  }

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
#' A truncation is reported, never silent: the columns, counts and examples are
#' recorded on \code{y} as \code{attr(y, "truncated")} (an empty table when
#' nothing was truncated) and announced in one message of class
#' \code{daffiz_message_truncation}.
#'
#' @param x The reference table, whose column types are the targets.
#' @param y The table to convert. Modified by reference, so it must be a
#'   data.table; columns only in \code{y} are left alone.
#' @param cols The columns to cast, by default those present in both tables.
#' @param date_format,truncate,nan_is_na See \code{cast_value()}.
#' @param labels How each column is named in messages, by column; the column
#'   names themselves by default. \code{diff_table()} passes the caller's own
#'   spelling of names it has normalized.
#' @return \code{y}, invisibly.
#' @noRd
cast_dt <- function(x, y, cols = intersect(names(x), names(y)),
                    date_format = "%Y-%m-%d", truncate = TRUE, nan_is_na = TRUE,
                    labels = NULL) {
  if (!is.data.frame(x) || !is.data.table(y)) {
    daffiz_abort("daffiz_error_input", "`x` must be a data.frame and `y` a data.table")
  }

  unknown <- setdiff(cols, intersect(names(x), names(y)))
  if (length(unknown)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0("column(s) missing from x or y: ", paste(unknown, collapse = ", ")),
      columns = unknown
    )
  }

  targets <- vapply(cols, function(nm) col_type(x[[nm]]), character(1L))
  bad <- targets[!targets %in% CAST_TYPES]
  if (length(bad)) {
    daffiz_abort(
      "daffiz_error_types",
      paste0(
        "x has column(s) of unsupported type: ",
        paste(sprintf("%s (%s)", names(bad), bad), collapse = ", ")
      ),
      columns = names(bad)
    )
  }

  if (is.null(labels)) {
    labels <- structure(cols, names = cols)
  }
  values <- Map(
    function(nm, to) {
      cast_value(y[[nm]], to, labels[[nm]],
        date_format = date_format, truncate = truncate, nan_is_na = nan_is_na
      )
    },
    cols, targets
  )

  truncated <- rbindlist(c(
    list(data.table(column = character(), n = integer(), examples = character())),
    lapply(cols, function(nm) {
      t <- attr(values[[nm]], "truncated")
      if (is.null(t)) {
        return(NULL)
      }
      data.table(column = labels[[nm]], n = t$n, examples = paste(t$examples, collapse = ", "))
    })
  ))

  for (nm in cols) {
    v <- values[[nm]]
    attr(v, "truncated") <- NULL
    set(y, j = nm, value = v)
  }
  setattr(y, "truncated", truncated)

  if (nrow(truncated)) {
    daffiz_inform(
      "daffiz_message_truncation",
      paste0(
        "Truncated ", format(sum(truncated$n), big.mark = ","),
        " value(s) while casting to the reference types:\n",
        paste0("  ", truncated$column, ": ", format(truncated$n, big.mark = ","),
          " (", truncated$examples, ")",
          collapse = "\n"
        )
      ),
      truncated = truncated
    )
  }
  invisible(y)
}

#' Convert a table to the types daffiz compares
#'
#' [diff_table()] compares columns of five types: character, logical, Date,
#' numeric (double) and integer. In `mode = "benchmark"` the benchmark `x` must
#' already hold only those; in `mode = "equal"` both tables must. This converts
#' the columns that are something else, explicitly, so that any lossy
#' conversion is the caller's choice and not a silent default.
#'
#' A factor becomes its labels by default. A timestamp (POSIXct) has no
#' default: `c(POSIXct = "Date")` keeps the calendar day in the stamp's own
#' zone and drops the time, so two stamps on the same day become one value;
#' `c(POSIXct = "character")` keeps the full stamp as text. Name the one you
#' mean in `to`.
#'
#' @param dt A data.frame or data.table.
#' @param to Named character: which supported type each other type becomes.
#'   The default only maps `factor` to `"character"`. A column whose type is
#'   not named here is an error, not a guess.
#' @param cols The columns to consider, by default all of them.
#' @return A `data.table`. `dt` is never modified; a copy is made only when
#'   there is something to convert, so the result is `dt` itself when there is
#'   not, and callers must treat it as read-only.
#' @export
#' @examples
#' x <- data.frame(
#'   day = as.POSIXct("2024-01-01 10:00", tz = "UTC"),
#'   kind = factor("a"),
#'   amount = 1.5
#' )
#' normalize_dt(x, to = c(POSIXct = "Date", factor = "character"))
normalize_dt <- function(dt, to = NORMALIZE_TO, cols = names(dt)) {
  if (!is.data.frame(dt)) {
    daffiz_abort("daffiz_error_input", "`dt` must be a data.frame")
  }
  dt <- as.data.table(dt)

  unknown <- setdiff(cols, names(dt))
  if (length(unknown)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0("unknown column(s): ", paste(unknown, collapse = ", ")),
      columns = unknown
    )
  }

  types <- vapply(cols, function(nm) col_type(dt[[nm]]), character(1L))
  need <- cols[!types %in% CAST_TYPES]
  if (!length(need)) {
    return(dt)
  }

  unmapped <- need[!types[need] %in% names(to)]
  if (length(unmapped)) {
    daffiz_abort(
      "daffiz_error_types",
      paste0(
        "no conversion given for column(s): ",
        paste(sprintf("%s (%s)", unmapped, types[unmapped]), collapse = ", "),
        "; name the target type in `to`"
      ),
      columns = unmapped
    )
  }

  # The caller named these conversions, truncating ones included, so what they
  # drop is the point rather than news: the record is not kept.
  out <- copy(dt)
  for (nm in need) {
    v <- cast_value(out[[nm]], to[[types[[nm]]]], nm)
    attr(v, "truncated") <- NULL
    set(out, j = nm, value = v)
  }
  out
}
