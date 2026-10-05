# Column types and the cast rules --------------------------------------------

# The only column types cast_dt() will produce. A reference column of any other
# class is refused rather than guessed at.
CAST_TYPES <- c("character", "logical", "Date", "numeric", "integer")

# Types a candidate column may arrive as. POSIXct converts like the timestamp
# it is; a factor only becomes its labels; integer64 becomes a number or its
# text through bit64. None is a target type.
CAST_FROM <- c(CAST_TYPES, "POSIXct", "factor", "integer64")

# How normalize_dt() converts a column that is not already a baseline type,
# when the caller does not say. Only a factor has a default: its labels are
# what it means, so becoming them loses nothing.
#
# A timestamp has no default. Turning it into a Date drops the time of day, so
# two stamps on the same day become one value -- and, in a key column, one
# duplicated key. Keeping it as text keeps the time but loses the instant's
# zone. Which of those is right is the caller's decision, so normalize_dt()
# asks for `to = c(POSIXct = ...)` rather than guessing.
NORMALIZE_TO <- c(factor = "character")

# Which conversions cast_dt() will attempt, and on what terms. This table is
# the authority: cast_value() consults it before doing any work, so what you
# see here is what the code does. `note` is the condition the values must meet
# (an unmet condition is an error, never a silent NA).
#
# A cast that loses the fraction of a value -- to a whole integer, to a whole
# day -- is a truncation. It is allowed by default because the reference's
# types are taken as the truth: storage such as Spark or Delta turns a locally
# built double into an integer, and a comparison against the stored table has
# to follow it. `truncate = FALSE` refuses it instead, and either way it is
# reported (see cast_dt()).
CAST_RULES <- rbindlist(lapply(CAST_FROM, function(from) {
  data.table(from = from, to = CAST_TYPES)
}))[from != to][, `:=`(
  allowed = TRUE,
  note = "as is"
)][
  from == "character",
  note := "parsed from the text; a missing value must be NA, not the text \"NA\""
][
  from == "numeric" & to == "character",
  note := "fixed notation, up to 15 significant digits (scientific outside [1e-15, 2^53)); refused when the text does not read back as the same number"
][
  from == "POSIXct" & to == "character",
  note := "the local time in the stamp's own timezone, which the text does not record"
][
  to == "integer" & from %in% c("numeric", "Date", "POSIXct", "character"),
  note := "a fraction is truncated toward zero (refused with truncate = FALSE); must be within integer range"
][
  from %in% c("Date", "POSIXct") & to %in% c("numeric", "integer"),
  note := "days (Date) or seconds (POSIXct) since 1970-01-01"
][
  from %in% c("Date", "POSIXct") & to == "integer",
  note := "days (Date) or seconds (POSIXct) since 1970-01-01; a fraction is truncated (refused with truncate = FALSE)"
][
  from == "integer" & to == "Date",
  note := "days since 1970-01-01"
][
  from == "numeric" & to == "Date",
  note := "days since 1970-01-01; a fraction of a day is truncated (refused with truncate = FALSE)"
][
  from == "POSIXct" & to == "Date",
  note := "calendar day in the stamp's own timezone (the session's, for a stamp with none); a time of day is truncated (refused with truncate = FALSE)"
][
  from == "integer64" & to == "character",
  note := "the decimal text (needs bit64)"
][
  from == "integer64" & to == "numeric",
  note := "exact below 2^53, larger values are refused (needs bit64)"
][
  from == "integer64" & to == "integer",
  note := "must be within integer range (needs bit64)"
][
  from == "factor" & to == "character",
  note := "the labels, never the integer codes"
][
  # Refusals come last so the reason is not overwritten by a note above.
  # Nothing but text becomes a logical: 0/1 is a number that happens to look
  # like a flag, and reading it as one is a guess about intent.
  (to == "logical" & from != "character") |
    (from %in% c("logical", "integer64") & to == "Date"),
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

#' The type conversions daffiz will make
#'
#' In `mode = "benchmark"`, [diff_table()] casts each column of the candidate
#' to the type it has in the benchmark. These are the conversions it will
#' attempt and the ones it refuses. A conversion that would need a guess -- a
#' number read as a flag, a factor's integer codes read as values -- is
#' refused rather than made.
#'
#' A cast that drops the fraction of a value (a double to an integer, a
#' timestamp to its calendar day) is a *truncation*: it is made when
#' `truncate = TRUE`, which is the default, and refused otherwise.
#'
#' Text is parsed strictly: anything that is not a number, a logical literal
#' or a date in `date_format` is refused, and that includes the text `"NA"`,
#' since a missing value has to arrive as `NA`. `"NaN"` and `"Inf"` are
#' numbers. A double written as text is written in fixed notation, so
#' `100000` becomes `"100000"`, not `"1e+05"`, and a timestamp written as text
#' is its local time in its own timezone, which the text does not record. An
#' `integer64` column (from \pkg{bit64}) becomes a double when that is exact,
#' an integer within range, or its decimal text.
#'
#' @param wide If `TRUE` (the default), a grid of source types by target types,
#'   with `"yes"` for an allowed conversion, `"-"` for a refused one and `"="`
#'   where source and target are the same type. If `FALSE`, one row per pair,
#'   with each rule's `note`.
#' @return A `data.table`.
#' @export
#' @examples
#' cast_rules()
#' cast_rules(wide = FALSE)
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
#' extra meaning (Date, POSIXct, integer64) before the underlying storage mode,
#' so a Date is not reported as a number. This is the only type vocabulary in
#' the package: summaries, measure selection and casting all read it, so a
#' column cannot be one type to one of them and another type to the next.
#'
#' integer64 is stored as a double, and \code{is.numeric()} says so; read as a
#' number it would be silently wrong. It is named for what it is, and converted
#' through bit64 (see \code{CAST_RULES}); it is never a target type, so a
#' benchmark holding one is refused with a pointer to \code{normalize_dt()}.
#'
#' An ordered factor is a factor (its first class is \code{"ordered"}), and is
#' named as one.
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
  } else if (inherits(v, "integer64")) {
    "integer64"
  } else if (is.factor(v)) {
    "factor"
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
