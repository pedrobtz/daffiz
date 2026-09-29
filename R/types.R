# Column types and the cast rules --------------------------------------------

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
