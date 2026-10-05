# Melting to one row per measured value --------------------------------------

# The type keywords `measures` accepts, and the col_type()s each selects. The
# keywords are lower case and normalized column names never are, so a keyword
# can never be mistaken for a column.
MEASURE_TYPES <- list(
  numeric = "numeric",
  `numeric+integer` = c("numeric", "integer")
)

#' Resolve which columns are measures
#'
#' @param dt A data.frame.
#' @param measures A type keyword from \code{MEASURE_TYPES}, or column names.
#'   Named columns must be numeric or integer: they are melted into one double
#'   value column, and a Date or a string has no place there.
#' @return The measure column names, in \code{dt}'s column order for a keyword
#'   and in the given order for names. Possibly empty.
#' @noRd
select_measures <- function(dt, measures = "numeric") {
  if (!is.character(measures) || !length(measures) || anyNA(measures)) {
    daffiz_abort(
      "daffiz_error_measures",
      "`measures` must be \"numeric\", \"numeric+integer\" or column names"
    )
  }
  types <- vapply(dt, col_type, character(1L))
  if (length(measures) == 1L && measures %in% names(MEASURE_TYPES)) {
    return(names(dt)[types %in% MEASURE_TYPES[[measures]]])
  }

  # A repeated name would melt the column twice on each side, and the join
  # would then go cartesian and silently double every count.
  dup <- unique(measures[duplicated(measures)])
  unknown <- setdiff(measures, names(dt))
  wrong <- setdiff(measures, c(unknown, names(dt)[types %in% c("numeric", "integer")]))
  problems <- c(
    if (length(dup)) paste0("named more than once: ", paste(dup, collapse = ", ")),
    if (length(unknown)) paste0("not a column: ", paste(unknown, collapse = ", ")),
    if (length(wrong)) {
      paste0(
        "not numeric or integer: ",
        paste(sprintf("%s (%s)", wrong, types[wrong]), collapse = ", ")
      )
    }
  )
  if (length(problems)) {
    daffiz_abort(
      "daffiz_error_measures",
      paste0("`measures` ", paste(problems, collapse = "; ")),
      columns = c(dup, unknown, wrong)
    )
  }
  measures
}

#' Number and key the rows of a table
#'
#' The wide table the rest of the pipeline works on: \code{row_id} first, then
#' the id columns and the measures, keyed by the ids. \code{row_id} ties every
#' row, and later every molten value, back to the row of \code{dt} it came
#' from; downstream reads it as a presence marker, so it is never missing.
#'
#' The selected columns are copied. \code{setkeyv()} reorders rows in place,
#' writing into the column vectors, and those may be the caller's: a column of
#' a data.table subset, or of a table renamed by \code{shallow_dt()}, is the
#' caller's vector, not a copy of it. Columns in neither role are left out.
#'
#' Integer measures are promoted to double here: \code{melt()} puts all
#' measures into one value column, and warns when it has to widen them itself.
#'
#' @param dt A data.frame.
#' @param ids The row identity: the key.
#' @param measures The measure columns.
#' @param nan_to_na Map NaN to NA in double id columns. data.table joins and
#'   groups NA and NaN as different keys, so without this a row keyed NaN on
#'   one side and NA on the other would not align.
#' @return A keyed data.table. Every missing value in a double id column holds
#'   one bit pattern; see \code{canonical_missing()}.
#' @noRd
index_dt <- function(dt, ids, measures = character(), nan_to_na = FALSE) {
  cols <- c(ids, measures)
  wide <- lapply(as.list(dt)[cols], copy)
  for (nm in measures) {
    if (is.integer(wide[[nm]])) {
      wide[[nm]] <- as.double(wide[[nm]])
    }
  }
  for (nm in ids) {
    wide[[nm]] <- canonical_missing(wide[[nm]], nan_to_na)
  }
  wide <- setDT(c(list(row_id = seq_len(nrow(dt))), wide))
  if (length(ids)) {
    setkeyv(wide, ids)
  }
  wide[]
}

#' One bit pattern per missing value in a double key
#'
#' R calls NA any NaN whose low word is 1954, and arithmetic on NA
#' (\code{NA + 0}, \code{as.Date(NA) + 1}, \code{x / 1000}) sets the quiet bit
#' on the way, so a computed key column often holds two NA patterns.
#' data.table joins them as one key, but its keyed grouping keeps them apart,
#' so duplicate numbering and the fanout check disagreed with the join.
#' Writing every NA back as \code{NA_real_} and every NaN as \code{NaN} gives
#' data.table one pattern for each, in every class built on a double (Date
#' and POSIXct included).
#'
#' @param v A vector; only doubles are touched.
#' @param nan_to_na Whether NaN becomes NA too.
#' @return \code{v}, with its class and attributes.
#' @noRd
canonical_missing <- function(v, nan_to_na = FALSE) {
  if (!is.double(v) || !anyNA(v)) {
    return(v)
  }
  at <- attributes(v)
  v <- as.vector(unclass(v), "double")
  nan <- is.nan(v)
  v[is.na(v) & (nan_to_na | !nan)] <- NA_real_
  if (!nan_to_na) {
    v[nan] <- NaN
  }
  attributes(v) <- at
  v
}

#' Melt a table, choosing the measures by type or by name
#'
#' Long form of \code{dt}, measuring the columns \code{measures} selects and
#' carrying the rest along as ids. The measure names come back as character
#' rather than the factor \code{melt()} defaults to, so they compare and join
#' like any label.
#'
#' A table that already carries \code{row_id} is taken as indexed by
#' \code{index_dt()} and melted as it is, keeping its key (\code{key_seq}
#' included, when duplicates were numbered). Any other table is indexed first,
#' with every column that is not a measure as its identity. User columns can
#' never be called \code{row_id}: \code{diff_table()} normalizes their names to
#' upper case first.
#'
#' @param dt A data.frame (or data.table).
#' @param measures \code{"numeric"} (the default) melts only the double columns.
#'   \code{"numeric+integer"} melts the integers too, as doubles. Column names
#'   melt exactly those.
#' @param variable.name Name of the column holding the measure names.
#' @param value.name Name of the column holding the measured values.
#' @return A long data.table keyed by the id columns, with \code{row_id} carried
#'   along as a column but left out of the key, so the key is made of real
#'   values. With no measures there is nothing to melt, and the indexed table
#'   comes back as it is: one row per row.
#' @noRd
melt_dt <- function(dt, measures = "numeric",
                    variable.name = "metric", value.name = "value") {
  if (!is.data.frame(dt)) {
    daffiz_abort("daffiz_error_input", "`dt` must be a data.frame")
  }

  if ("row_id" %in% names(dt)) {
    own <- setdiff(names(dt), c("row_id", "key_seq"))
    measure <- select_measures(shallow_dt(dt, own), measures)
  } else {
    measure <- select_measures(dt, measures)
    dt <- index_dt(dt, setdiff(names(dt), measure), measure)
  }
  if (!length(measure)) {
    return(dt)
  }

  id_vars <- setdiff(names(dt), measure)
  long <- melt.data.table(
    dt,
    id.vars = id_vars,
    measure.vars = measure,
    variable.name = variable.name,
    value.name = value.name,
    variable.factor = FALSE
  )
  setcolorder(long, "row_id")
  # Keyed by the id.vars the caller's data supplies -- row_id is carried along
  # as a column but left out of the key, so the key is made of real values.
  key_cols <- setdiff(id_vars, "row_id")
  if (length(key_cols)) {
    setkeyv(long, key_cols)
  }
  long[]
}
