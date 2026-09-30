# Column summaries and schema comparison -------------------------------------

#' Summarize the columns of a table
#'
#' One row per column of \code{dt}: its name, type and number of unique values.
#' The type is \code{col_type()}'s, the one vocabulary every other part of the
#' package reads, so an IDate is a \code{Date} here as it is everywhere else.
#'
#' @param dt A data.frame (or data.table).
#' @return A data.table with one row per column of \code{dt} and columns
#'   \code{colname}, \code{type} and \code{n_unique}.
#' @noRd
summarize_dt <- function(dt) {
  if (!is.data.frame(dt)) {
    daffiz_abort("daffiz_error_input", "`dt` must be a data.frame")
  }

  data.table(
    colname = names(dt),
    type = vapply(dt, col_type, character(1L)),
    n_unique = vapply(dt, uniqueN, integer(1L))
  )
}

#' Compare the columns of two tables
#'
#' The schema view of a comparison: which columns each table has, their types,
#' and how much their values overlap. It reads every column of both tables,
#' including the ones [diff_table()] would drop, so it is the place to look
#' when a comparison refuses to run.
#'
#' Column names are normalized first, as [diff_table()] does: upper case, with
#' every run of other characters replaced by `_`. So `amount` in `x` and
#' `Amount` in `y` are one column. Two names in one table that normalize to
#' the same thing are an error.
#'
#' `over_pct` is the distinct values shared by both sides over the distinct
#' values seen in either, `n_common / n_union`. 100 means the two columns draw
#' on exactly the same set of values; a value `y` invents or one it never uses
#' both pull it down.
#'
#' The overlap is measured only where the two types agree. `%in%` coerces
#' before matching, so a numeric 1 would count as a match for the string `"1"`
#' and two columns that are not the same thing would report a full overlap. A
#' column whose type differs gets `NA` for `n_common`, `n_union` and
#' `over_pct`.
#'
#' @param x,y Tables to compare: data.frames or data.tables.
#' @return A `data.table` with one row per column of either side: the
#'   normalized name `colname`, the names as written in each table (`name_x`,
#'   `name_y`), the two types and whether they match, the two unique counts,
#'   and the value overlap (`n_common`, `n_union`, `over_pct`). Rows follow
#'   `x`'s column order, then whatever is only in `y`. A column missing from
#'   one side has `NA` for that side.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(1, 2, 3))
#' y <- data.frame(ID = c(1L, 2L, 4L), Amount = c("1", "2", "3"), note = "a")
#' compare_columns(x, y)
compare_columns <- function(x, y) {
  if (!is.data.frame(x) || !is.data.frame(y)) {
    daffiz_abort("daffiz_error_input", "`x` and `y` must be data.frames")
  }

  map_x <- check_names(name_map(x), "x")
  map_y <- check_names(name_map(y), "y")
  x <- shallow_dt(x, new = map_x$column)
  y <- shallow_dt(y, new = map_y$column)

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
  out[, `:=`(
    name_x = map_x$name[match(colname, map_x$column)],
    name_y = map_y$name[match(colname, map_y$column)]
  )]

  setcolorder(
    out,
    c(
      "colname",
      "name_x",
      "name_y",
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
