# The comparison -------------------------------------------------------------

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
