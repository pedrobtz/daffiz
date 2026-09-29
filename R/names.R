# Column names ------------------------------------------------------------------
#
# Every user column is renamed before anything else happens, to a name made of
# A-Z, 0-9 and `_` only. Two things follow from that:
#
# - The columns daffiz creates (`row_id`, `metric`, `value_x`, `status`,
#   `key_seq`, ...) all contain a lowercase letter, so no user column can ever
#   collide with one. A test asserts this for every internal name; keep it so.
# - `amount` in one table and `AMOUNT` or `Amount` in the other are the same
#   column. Storage such as Spark and Delta is case-insensitive about column
#   names and often lowercases them, so a table that went through it still
#   pairs with the one that did not.

# Upper-case, then replace every run of characters outside A-Z and 0-9 with a
# single `_`, then trim `_` from both ends. Trimming is what lets `amount ` (a
# stray space in a CSV header) pair with `amount`. The regex runs with
# perl = TRUE, whose character classes do not depend on the session locale.
canonical_names <- function(nms) {
  out <- gsub("[^A-Z0-9]+", "_", toupper(enc2utf8(nms)), perl = TRUE)
  gsub("^_+|_+$", "", out, perl = TRUE)
}

# One row per column of `dt`: `name` as the caller wrote it, `column` as it is
# after canonical_names(), and `label`, the form messages use -- the caller's
# own spelling, followed by the normalized name when that differs.
name_map <- function(dt) {
  nms <- names(dt)
  column <- canonical_names(nms)
  data.table(
    name = nms,
    column = column,
    label = ifelse(nms == column, column, sprintf("\"%s\" (%s)", nms, column))
  )
}

# Refuses names that normalize to nothing, and names that normalize to the
# same thing. Both are for the caller to fix: guessing which of `amount` and
# `Amount` they meant, or suffixing one of them, would compare the wrong
# column. `only` restricts the check to the normalized names that matter; in
# benchmark mode that is the names `x` uses, since `y`'s other columns are
# dropped anyway.
check_names <- function(map, side, only = NULL) {
  if (!is.null(only)) {
    map <- map[column %in% only]
  }
  empty <- map[!nzchar(column), name]
  if (length(empty)) {
    daffiz_abort(
      "daffiz_error_columns",
      paste0(
        "`", side, "` has column name(s) with no letter or digit, so nothing ",
        "is left of them after normalization: ",
        paste0("\"", empty, "\"", collapse = ", "),
        ". Rename these columns before comparing."
      ),
      side = side, columns = empty
    )
  }
  dup <- map[column %in% column[duplicated(column)]]
  if (nrow(dup)) {
    groups <- dup[, paste0("\"", name, "\"", collapse = ", "), by = column]
    daffiz_abort(
      "daffiz_error_columns",
      paste0(
        "`", side, "` has columns whose names are the same after ",
        "normalization to A-Z, 0-9 and _:\n",
        paste0("  ", groups$column, " <- ", groups$V1, collapse = "\n"),
        "\nRename or drop these columns before comparing."
      ),
      side = side, columns = dup$name
    )
  }
  invisible(map)
}

# The columns of `dt` named in `cols`, renamed to `new`, as a data.table that
# shares its column vectors with `dt`. Later steps may replace a column of it
# (`set(j = , value = )` swaps a pointer) but must never write into one.
shallow_dt <- function(dt, cols = names(dt), new = cols) {
  out <- as.list(dt)[cols]
  names(out) <- new
  setDT(out)[]
}
