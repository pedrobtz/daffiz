# The comparison -------------------------------------------------------------

# `status` levels, in a fixed order so table() and summaries show every one.
STATUS_LEVELS <- c("same", "changed", "only_x", "only_y")

#' Compare two tables value by value
#'
#' `x` is the benchmark: in the default `mode = "benchmark"` it is the source
#' of truth, and `y` is the table being tested against it. With
#' `mode = "equal"` the two are peers, and the question is simply whether they
#' are equal. Rows are aligned on their identity columns and every numeric
#' measure is compared, one row per value, each traceable to its source row in
#' both tables.
#'
#' @section Modes:
#' * `"benchmark"`: does `y` match the benchmark `x`? Columns of `y` that `x`
#'   does not have are dropped, with a message. A column of `x` missing from
#'   `y` is an error. Each column of `y` is cast to the type it has in `x`
#'   (see [cast_rules()]), so a number stored as text, or a double stored as
#'   an integer, still compares.
#' * `"equal"`: are `x` and `y` equal? Every column must be in both tables and
#'   have the same type in both; nothing is cast. The result is symmetric:
#'   swapping `x` and `y` swaps `only_x` with `only_y` and negates `diff`.
#'
#' @section Column names:
#' Column names are normalized in both tables before anything else: upper
#' case, with every run of characters other than `A-Z` and `0-9` replaced by
#' `_`, and `_` trimmed from the ends. So `unit price`, `Unit.Price` and
#' `UNIT_PRICE` are one column, in either table. Two names in one table that
#' normalize to the same thing are an error to fix before comparing. The
#' result uses the normalized names; `attr(, "name_map")` maps them back.
#'
#' @section Truncation:
#' In benchmark mode a cast that drops a fraction -- a double to an integer, a
#' number or a timestamp to a whole day -- is made by default, because the
#' benchmark's types are taken as the truth: storage such as Spark or Delta
#' turns a locally built double into an integer, and the comparison has to
#' follow it. Each truncation is announced in a message and recorded in
#' `attr(, "truncated")`. Set `truncate = FALSE` to refuse such casts instead.
#'
#' @section Measures and identity:
#' The measures are the numeric columns compared value by value; `measures`
#' picks them by type or by name, always from `x`. The row identity is `by`
#' when given, otherwise every column that is not a measure. With `by` given,
#' a column in neither role is not compared: that is announced in benchmark
#' mode and an error in equal mode, which cannot call two tables equal while
#' ignoring a column. A numeric column that is part of the identity is matched
#' exactly, with no tolerance.
#'
#' With no measures at all the question becomes *do the two tables have the
#' same rows?*, and the result has one row per row (see Value).
#'
#' @section Well-formed tables:
#' A table must have at least one key column to be compared; it may have no
#' measures. When every column is a measure there is nothing to align rows
#' on, and `diff_table()` refuses unless it is given a key: `by`, fewer
#' `measures`, or a virtual one with `row_key`, which adds a `row_number` key
#' column to both tables:
#' * `"position"` numbers the rows as they come, so row *i* of `x` pairs with
#'   row *i* of `y`, and a different row order shows as differences.
#' * `"sorted"` numbers the rows after sorting both tables by all their
#'   compared columns, so row order does not matter. Compared exactly, that
#'   pairs the tables as multisets of rows, and cannot pass two tables that
#'   hold different rows. It can pair badly instead: one changed value can
#'   move its row within the sort, and the rows after it then pair with the
#'   wrong partners and show as changes too. With other key columns, rows
#'   are sorted and numbered within each of their groups.
#'
#' @section Duplicate keys:
#' When the identity does not tell rows apart, `duplicates` decides:
#' * `"disambiguate"` numbers the rows of each repeated key in arrival order
#'   (`key_seq`) and pairs them by that number, with a warning: the pairing is
#'   an assumption. Reordered rows then show as changes, never as matches.
#'   Without measures the repeated rows are identical in every compared
#'   column, so pairing them assumes nothing and there is no warning.
#' * `"aggregate"` compares the multiset of values of each key and measure
#'   instead, exactly (`tolerance` must be 0).
#' * `"error"` refuses.
#'
#' @section Values:
#' A value is `same` when both are equal, or when both are finite and differ
#' by at most `tolerance`. Two missing values are the same; with
#' `nan_is_na = TRUE` NaN and NA are the same missing value, in measures and in
#' key columns alike.
#'
#' @param x The benchmark table (in `"equal"` mode, simply the first table).
#' @param y The table to compare with it.
#' @param mode `"benchmark"` (the default) or `"equal"`. See Modes.
#' @param by Identity columns, by name, in either spelling. `NULL` (the
#'   default) uses every column that is not a measure.
#' @param row_key `"none"` (the default), `"position"` or `"sorted"`: add a
#'   virtual `row_number` key column. See Well-formed tables.
#' @param measures `"numeric"` (the default) compares the double columns;
#'   `"numeric+integer"` the integer columns too. Or column names, which must
#'   be numeric or integer.
#' @param tolerance The largest absolute difference still counted as `same`.
#' @param duplicates `"disambiguate"` (the default), `"aggregate"` or
#'   `"error"`. See Duplicate keys.
#' @param cast Whether to cast `y` to `x`'s types. Defaults to `TRUE` in
#'   benchmark mode; with `FALSE` the types must already agree. Equal mode
#'   never casts.
#' @param truncate Whether a cast may drop a fraction. See Truncation.
#' @param nan_is_na Whether NaN and NA are the same missing value.
#' @param date_format The format text dates are read in when cast to `Date`.
#'   Text must match it exactly, so a string that merely starts with a date is
#'   refused rather than read as one.
#'
#' @return A `data.table` keyed by the identity columns (and `key_seq`,
#'   `metric` when present), with `status` a factor with levels `same`,
#'   `changed`, `only_x` and `only_y`. Its shape is in `attr(, "shape")`:
#'   The key includes `row_number` when `row_key` added it.
#'   * `"cells"`, the usual one: one row per row and measure, with `key_seq`
#'     (only when duplicates were numbered), `metric`, `row_id_x`, `row_id_y`
#'     (the source rows, `NA` on the side the row is missing from),
#'     `value_x`, `value_y`, `diff` (`value_y - value_x`) and `status`.
#'   * `"groups"`, with `duplicates = "aggregate"`: one row per key and
#'     measure, with `n_rows_x`, `n_rows_y`, `n_na_x`, `n_na_y`,
#'     `value_key_x`, `value_key_y` (the exact multiset of values) and
#'     `status`. Without measures only the row counts are compared.
#'   * `"rows"`, when there are no measures: one row per row, with `key_seq`
#'     (when present), `row_id_x`, `row_id_y` and `status`, which is never
#'     `changed`.
#'
#'   The attributes `mode`, `name_map` (each column's name in `x` and `y`),
#'   `dropped` (columns of `y` dropped in benchmark mode), `ignored` (columns
#'   not compared) and `truncated` (the casts that truncated) record what was
#'   done on the way.
#' @export
#' @examples
#' benchmark <- data.frame(id = 1:4, amount = c(10, 20, 30, 40))
#' tested <- data.frame(
#'   ID = c("1", "2", "3", "5"),
#'   Amount = c(10, 20.5, 30, 50),
#'   loaded_at = "2024-01-01"
#' )
#' d <- diff_table(benchmark, tested)
#' d
#' d[status != "same"]
#'
#' # Equal mode: no casting, every column in both tables.
#' diff_table(benchmark, benchmark, mode = "equal")
#'
#' # No key at all: align rows by sorting both tables by their values.
#' diff_table(
#'   data.frame(a = c(3, 1, 2), b = c(30, 10, 20)),
#'   data.frame(a = c(1, 2, 3), b = c(10, 25, 30)),
#'   row_key = "sorted"
#' )
#'
#' # No measures: do the tables have the same rows?
#' diff_table(data.frame(k = c("a", "b")), data.frame(k = c("b", "c", "a")))
diff_table <- function(x, y,
                       mode = c("benchmark", "equal"),
                       by = NULL,
                       row_key = c("none", "position", "sorted"),
                       measures = "numeric",
                       tolerance = 0,
                       duplicates = c("disambiguate", "aggregate", "error"),
                       cast = mode == "benchmark",
                       truncate = TRUE,
                       nan_is_na = TRUE,
                       date_format = "%Y-%m-%d") {
  # 1. Arguments ----------------------------------------------------------------
  if (!is.data.frame(x) || !is.data.frame(y)) {
    daffiz_abort("daffiz_error_input", "`x` and `y` must be data.frames")
  }
  mode <- match.arg(mode)
  row_key <- match.arg(row_key)
  duplicates <- match.arg(duplicates)
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) ||
    tolerance < 0) {
    daffiz_abort("daffiz_error_input", "`tolerance` must be a single finite number >= 0")
  }
  for (arg in c("truncate", "nan_is_na")) {
    if (!is_flag(get(arg))) {
      daffiz_abort("daffiz_error_input", sprintf("`%s` must be TRUE or FALSE", arg))
    }
  }
  if (mode == "equal" && !missing(cast) && isTRUE(cast)) {
    daffiz_abort(
      "daffiz_error_input",
      "mode = \"equal\" never casts: neither table is the reference for the other"
    )
  }
  if (!is_flag(cast)) {
    daffiz_abort("daffiz_error_input", "`cast` must be TRUE or FALSE")
  }
  if (!is.null(by) && (!is.character(by) || !length(by) || anyNA(by))) {
    daffiz_abort("daffiz_error_input", "`by` must be NULL or column names")
  }
  # The multiset of a key's values is compared exactly; a tolerance would
  # silently do nothing, so it is refused instead.
  if (duplicates == "aggregate" && tolerance > 0) {
    daffiz_abort(
      "daffiz_error_input",
      "duplicates = \"aggregate\" compares values exactly; `tolerance` must be 0"
    )
  }

  # 2. Names --------------------------------------------------------------------
  map_x <- check_names(name_map(x), "x")
  map_y <- name_map(y)
  # In benchmark mode y's other columns are dropped below, so only a collision
  # on a name x uses is ambiguous.
  check_names(map_y, "y", only = if (mode == "benchmark") map_x$column)

  # 3. Columns ------------------------------------------------------------------
  only_x <- map_x[!column %in% map_y$column]
  only_y <- map_y[!column %in% map_x$column]
  if (mode == "benchmark") {
    if (nrow(only_x)) {
      daffiz_abort(
        "daffiz_error_columns",
        paste0(
          "`y` is missing column(s) the benchmark `x` has: ",
          fmt_names(only_x$label), "."
        ),
        columns = only_x$name
      )
    }
    if (nrow(only_y)) {
      daffiz_inform(
        "daffiz_message_dropped_columns",
        paste0(
          "Dropped column(s) of `y` that the benchmark `x` does not have: ",
          fmt_names(only_y$name), "."
        ),
        columns = only_y$name
      )
    }
  } else if (nrow(only_x) || nrow(only_y)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0(
        "mode = \"equal\" needs the same columns in both tables.\n",
        "  only in `x`: ", fmt_names(only_x$label), "\n",
        "  only in `y`: ", fmt_names(only_y$label)
      ),
      only_x = only_x$name, only_y = only_y$name
    )
  }

  # Both tables now hold x's columns, in x's order, under normalized names.
  # They share the caller's vectors: every step below may replace a column,
  # never write into one.
  at_y <- match(map_x$column, map_y$column)
  xx <- shallow_dt(x, seq_along(map_x$column), map_x$column)
  yy <- shallow_dt(y, at_y, map_x$column)
  label_x <- structure(map_x$label, names = map_x$column)
  label_y <- structure(map_y$label[at_y], names = map_x$column)

  # 4. Types --------------------------------------------------------------------
  types_x <- vapply(xx, col_type, character(1L))
  types_y <- vapply(yy, col_type, character(1L))
  # x supplies the target types, so every one of its columns has to be a type
  # we can cast to. Refused rather than converted here: which day a timestamp
  # belongs to, or what a factor's codes mean, is the caller's call to make
  # with normalize_dt().
  unsupported <- types_x[!types_x %in% CAST_TYPES]
  if (length(unsupported)) {
    daffiz_abort(
      "daffiz_error_types",
      paste0(
        "x has column(s) of unsupported type: ",
        paste(sprintf("%s (%s)", label_x[names(unsupported)], unsupported), collapse = ", "),
        "; convert them with normalize_dt() first"
      ),
      columns = names(unsupported)
    )
  }
  if (cast) {
    cast_dt(xx, yy,
      date_format = date_format, truncate = truncate, nan_is_na = nan_is_na,
      labels = label_y
    )
  } else {
    # Nothing is being aligned, so the types have to line up already. Without
    # this the failure surfaces much later, as a missing key or as data.table's
    # own "Incompatible join types", neither of which names the real problem.
    off <- names(types_x)[types_x != types_y]
    if (length(off)) {
      daffiz_abort(
        "daffiz_error_types",
        paste0(
          if (mode == "equal") "mode = \"equal\" does not cast" else "cast = FALSE",
          ", but x and y disagree on the type of: ",
          paste(sprintf("%s (%s vs %s)", label_x[off], types_x[off], types_y[off]), collapse = ", ")
        ),
        columns = off
      )
    }
  }
  truncated <- attr(yy, "truncated")
  if (is.null(truncated)) {
    truncated <- data.table(column = character(), n = integer(), examples = character())
  }

  # 5. Roles --------------------------------------------------------------------
  if (!is.null(by)) {
    by <- canonical_names(by)
    problems <- c(
      if (anyDuplicated(by)) paste0("names a column more than once: ", fmt_names(unique(by[duplicated(by)]))),
      if (length(setdiff(by, names(xx)))) paste0("names column(s) not in the tables: ", fmt_names(setdiff(by, names(xx))))
    )
    if (length(problems)) {
      daffiz_abort("daffiz_error_columns", paste0("`by` ", paste(problems, collapse = "; ")))
    }
  }
  keyword <- is.character(measures) && length(measures) == 1L &&
    measures %in% names(MEASURE_TYPES)
  if (keyword) {
    pool <- setdiff(names(xx), by)
    measure <- select_measures(shallow_dt(xx, pool), measures)
  } else {
    if (is.character(measures)) {
      measures <- canonical_names(measures)
    }
    measure <- select_measures(xx, measures)
    both <- intersect(measure, by)
    if (length(both)) {
      daffiz_abort(
        "daffiz_error_measures",
        paste0("`by` and `measures` both name: ", fmt_names(label_x[both]))
      )
    }
  }
  ids <- if (is.null(by)) setdiff(names(xx), measure) else by

  # A table is well-formed when it has at least one key column; it may have no
  # measures. Without a key, rows cannot be aligned at all, so the caller
  # either names one or asks for a virtual one: the row number, as the rows
  # come or after sorting both tables by what they hold.
  if (row_key != "none") {
    set(xx, j = "row_number", value = virtual_key(xx, row_key, ids, measure, nan_is_na))
    set(yy, j = "row_number", value = virtual_key(yy, row_key, ids, measure, nan_is_na))
    label_x[["row_number"]] <- "row_number"
    ids <- c(ids, "row_number")
  }
  if (!length(ids)) {
    daffiz_abort(
      "daffiz_error_keys",
      paste0(
        "The tables are not well-formed: they have no key column, since every ",
        "column is a measure, so rows cannot be aligned. Name the key with ",
        "`by=`, leave some columns out of `measures`, or add a virtual key: ",
        "row_key = \"position\" pairs the rows as they come, and ",
        "row_key = \"sorted\" pairs them after sorting both tables by their values."
      )
    )
  }
  ignored <- setdiff(names(xx), c(ids, measure))
  if (length(ignored)) {
    if (mode == "equal") {
      daffiz_abort(
        "daffiz_error_columns",
        paste0(
          "mode = \"equal\" compares every column, but these are in neither ",
          "`by` nor `measures`: ", fmt_names(label_x[ignored]), ". Add them to ",
          "`by` (matched exactly, as part of the row identity) or, if numeric, ",
          "to `measures`."
        ),
        columns = ignored
      )
    }
    daffiz_inform(
      "daffiz_message_ignored_columns",
      paste0(
        "Not compared, being in neither `by` nor `measures`: ",
        fmt_names(label_x[ignored]), "."
      ),
      columns = ignored
    )
  }
  if (!length(measure)) {
    daffiz_inform(
      "daffiz_message_no_measures",
      paste0(
        "No measure columns (measures = ", paste(deparse(measures), collapse = ""),
        "), so rows are compared as wholes: do the two tables have the same rows?"
      )
    )
  }

  # 6-7. Index ------------------------------------------------------------------
  # Our own copies of the id and measure columns, numbered and keyed.
  wx <- index_dt(xx, ids, measure, nan_to_na = nan_is_na)
  wy <- index_dt(yy, ids, measure, nan_to_na = nan_is_na)

  # 8. Duplicate keys -----------------------------------------------------------
  # Checked on the wide tables, one row per source row: the answer is the same
  # as on the melted ones, the work is smaller by the number of measures, and
  # key_seq is the same for every measure of a row by construction.
  if (duplicates != "aggregate") {
    ux <- unique_by_key(wx)
    uy <- unique_by_key(wy)
    if (!ux || !uy) {
      groups <- rbindlist(
        list(x = attr(ux, "duplicates"), y = attr(uy, "duplicates")),
        idcol = "side"
      )
      head_line <- sprintf(
        "the id columns do not tell rows apart: %s key(s) repeat (identity: %s)",
        format(uniqueN(groups, by = ids), big.mark = ","), fmt_names(ids)
      )
      if (duplicates == "error") {
        daffiz_abort(
          "daffiz_error_duplicates",
          paste0(head_line, "; use duplicates = \"disambiguate\" or \"aggregate\""),
          duplicates = groups
        )
      }
      disambiguate_by_key(wx)
      disambiguate_by_key(wy)
      if (length(measure)) {
        daffiz_warn(
          "daffiz_warning_duplicates",
          paste0(
            head_line, ". Rows within each were paired in arrival order ",
            "(key_seq), which assumes both tables list them in the same order."
          ),
          duplicates = groups
        )
      }
    }
  }

  # 9. Alignment ----------------------------------------------------------------
  # A comparison in which no row aligns compares nothing: every row would be
  # only_x or only_y. That is almost always a wrong `by`, or key values that
  # agree in type but not in format (padded codes, trimmed strings). Checked
  # before the melt, since it is the case that would build the largest table.
  # Two empty tables are equal, not ill-formed.
  if (nrow(wx) + nrow(wy) > 0L) {
    dry <- merge_dry_run(wx, wy, on = ids)
    if (dry$keys_common == 0L) {
      whole_rows <- is.null(by) && !length(measure)
      daffiz_abort(
        "daffiz_error_disjoint",
        paste0(
          if (whole_rows) {
            "No row of `x` appears in `y`"
          } else {
            paste0("No identity value of `x` appears in `y` (identity: ", fmt_names(ids), ")")
          },
          ", so nothing can be compared",
          if (!nrow(wx) || !nrow(wy)) " (one table is empty)",
          if (!whole_rows) ". Check `by`, and the format of the key values",
          "."
        ),
        dry_run = dry
      )
    }
  }

  # 10. Melt --------------------------------------------------------------------
  melt_side <- function(w) if (length(measure)) melt_dt(w, measure) else w
  lx <- melt_side(wx)
  ly <- melt_side(wy)
  if (duplicates == "aggregate") {
    lx <- aggregate_by_key(lx, by = ids, stats = "exact", nan_is_na = nan_is_na)
    ly <- aggregate_by_key(ly, by = ids, stats = "exact", nan_is_na = nan_is_na)
  }
  shape <- if (duplicates == "aggregate") "groups" else if (length(measure)) "cells" else "rows"

  # 11. Merge -------------------------------------------------------------------
  # In place, into the melted copy of x this function owns, so no second table
  # is built. An update join keeps only x's rows, so y's extras are fetched
  # separately with an anti-join and appended.
  on <- c(key(lx), intersect("metric", names(lx)))
  merge_dt(lx, ly, on = on, mode = "in_place")
  out <- lx
  extra <- ly[!out, on = on]
  if (nrow(extra)) {
    setnames(extra, setdiff(names(extra), on), paste0(setdiff(names(extra), on), "_y"))
    out <- rbind(out, extra, fill = TRUE)
  }
  # x's own columns kept their names through the merge; label them now. Every
  # carried column is x's too: step 3 removed whatever only y had.
  carried <- setdiff(names(ly), on)
  setnames(out, carried, paste0(carried, "_x"))

  # 12. Classify ----------------------------------------------------------------
  # Membership is read from a column that is never NA in a row that exists
  # (row_id from the index, n_rows from the aggregate), never from the value
  # itself: a value of NA says nothing about whether the row was there.
  present <- if (shape == "groups") "n_rows" else "row_id"
  in_x <- !is.na(out[[paste0(present, "_x")]])
  in_y <- !is.na(out[[paste0(present, "_y")]])

  unchanged <- switch(shape,
    cells = {
      vx <- out$value_x
      vy <- out$value_y
      out[, diff := value_y - value_x]
      # Two missing values are the same, and with nan_is_na a NaN is just a
      # missing value. Equality comes before the tolerance: Inf - Inf is NaN,
      # so a tolerance test alone would call two identical infinities a change.
      both_missing <- if (nan_is_na) {
        is.na(vx) & is.na(vy)
      } else {
        (is.nan(vx) & is.nan(vy)) | (is.na(vx) & !is.nan(vx) & is.na(vy) & !is.nan(vy))
      }
      both_missing |
        (!is.na(vx) & !is.na(vy) & (vx == vy | abs(vy - vx) <= tolerance))
    },
    groups = {
      same_n <- out$n_rows_x == out$n_rows_y
      if ("value_key_x" %in% names(out)) same_n & out$value_key_x == out$value_key_y else same_n
    },
    rows = rep(TRUE, nrow(out))
  )
  out[, status := factor(
    fcase(
      !in_x & in_y, "only_y",
      in_x & !in_y, "only_x",
      unchanged, "same",
      default = "changed"
    ),
    levels = STATUS_LEVELS
  )]

  # 13. Result ------------------------------------------------------------------
  setcolorder(out, intersect(c(
    ids, "key_seq", "metric",
    "row_id_x", "row_id_y", "value_x", "value_y", "diff",
    "n_rows_x", "n_rows_y", "n_na_x", "n_na_y", "value_key_x", "value_key_y",
    "status"
  ), names(out)))
  setkeyv(out, on)
  setattr(out, "shape", shape)
  setattr(out, "mode", mode)
  setattr(out, "name_map", data.table(
    column = map_x$column,
    name_x = map_x$name,
    name_y = map_y$name[at_y]
  ))
  setattr(out, "dropped", only_y$name)
  setattr(out, "ignored", ignored)
  setattr(out, "truncated", truncated)
  out[]
}

is_flag <- function(v) is.logical(v) && length(v) == 1L && !is.na(v)
