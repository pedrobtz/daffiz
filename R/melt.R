# Melting to one row per measured value --------------------------------------

#' Melt a table, choosing the measures by type
#'
#' Long form of \code{dt}, measuring the columns \code{measures} selects and
#' carrying the rest along as ids. Picking measures by class keeps this working
#' when columns are renamed. The measure names come back as character rather
#' than the factor \code{melt()} defaults to, so they compare and join like any
#' label.
#'
#' A row_id column is added first, so a molten row can be traced back to the row
#' of \code{dt} it came from. It is added on a copy, so \code{dt} is not
#' touched, and always written fresh: any column of that name is overwritten,
#' since callers' columns are upper case and this one has to be ours.
#'
#' @param dt A data.frame (or data.table).
#' @param measures \code{"numeric"} (the default) melts only the double columns.
#'   \code{"numeric+integer"} melts the integers too; \code{melt()} then widens
#'   the whole value column to double, since one column holds one type.
#' @param variable.name Name of the column holding the measure names.
#' @param value.name Name of the column holding the measured values.
#' @return A long data.table keyed by the id columns the caller's data supplies.
#'   row_id is carried along as a column but left out of the key, so the key is
#'   made of real values.
#' @noRd
melt_dt <- function(dt, measures = c("numeric", "numeric+integer"),
                    variable.name = "metric", value.name = "value") {
  stopifnot(is.data.frame(dt))
  measures <- match.arg(measures)

  types <- if (measures == "numeric") "numeric" else c("numeric", "integer")
  measure <- summarize_dt(dt)[type %in% types, colname]
  if (!length(measure)) {
    stop("no columns of type: ", paste(types, collapse = ", "), call. = FALSE)
  }

  # row_id ties every molten row back to the row it came from. Added on a copy,
  # so `dt` is not touched, and always written fresh: downstream reads it as a
  # presence marker, so it has to be ours and it has to be non-missing. The
  # lower-case name is what keeps it out of the caller's way.
  wide <- copy(as.data.table(dt))
  set(wide, j = "row_id", value = seq_len(nrow(wide)))
  setcolorder(wide, "row_id")

  id_vars <- setdiff(names(wide), measure)
  long <- melt.data.table(
    wide,
    id.vars = id_vars,
    measure.vars = measure,
    variable.name = variable.name,
    value.name = value.name,
    variable.factor = FALSE
  )
  setcolorder(long, "row_id")
  # Keyed by the id.vars the caller's data supplies -- row_id is carried along
  # as a column but left out of the key, so the key is made of real values.
  setkeyv(long, setdiff(id_vars, "row_id"))[]
}
