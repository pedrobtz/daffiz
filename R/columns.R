# Column summaries and schema comparison -------------------------------------

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
