#' Compare rows by position rather than by value
#'
#' Pass as `by` to [compare_dt()] to align rows by their position in each input
#' instead of by the value of identity columns. Positional comparison must be
#' requested explicitly; it is never inferred (design plan section 4.2).
#'
#' @return A sentinel recognized by [compare_dt()].
#' @export
#' @examples
#' x <- data.frame(v = c(1, 2, 3))
#' y <- data.frame(v = c(1, 2, 3.5))
#' compare_dt(x, y, by = daffiz_row_number())
daffiz_row_number <- function() {
  structure(list(), class = "daffiz_row_number")
}

is_row_number <- function(x) inherits(x, "daffiz_row_number")

# A default measure is an *unclassed* double. `is.double()` alone is not
# sufficient: it is also TRUE for Date and POSIXct, which are identity/context
# columns here (design plan section 3, correction 9).
is_default_measure <- function(column) {
  typeof(column) == "double" && !is.object(column)
}

# Role inference reads *both* inputs: the design plan's default measure set is
# the shared unclassed doubles. Inferring from `x` alone made `compare_dt()`
# asymmetric -- a column that was integer in `x` and double in `y` became an
# identity column and tripped gate 6, while the same two tables compared fine
# with the arguments swapped.
is_shared_default_measure <- function(cx, cy) {
  is_default_measure(cx) && is_default_measure(cy)
}

# Resolves the by/compare roles from the defaults and any explicit overrides,
# implementing the resolution table in design plan section 4.3. Also runs
# preflight gates 5 and 8.
resolve_roles <- function(x, y, kept, by, compare) {
  positional <- is_row_number(by)
  if (positional) by <- NULL

  if (!is.null(by) && !is.character(by)) {
    daffiz_abort("daffiz_error_roles",
      "`by` must be a character vector, NULL, or daffiz_row_number().")
  }
  if (!is.null(compare) && !is.character(compare)) {
    daffiz_abort("daffiz_error_roles",
      "`compare` must be a character vector or NULL.")
  }

  candidate_measure <- kept[vapply(
    kept, function(nm) is_shared_default_measure(x[[nm]], y[[nm]]), logical(1)
  )]
  candidate_by <- setdiff(kept, candidate_measure)

  # Gate 5: explicit columns are named once, exist, and do not overlap.
  #
  # A repeated name is not harmless. `compare = c("a", "a")` melts the measure
  # twice on each side, so the join goes cartesian and every cell, count and
  # statistic is silently multiplied -- a three-row comparison reported twelve
  # cells and two identical `column_summary()` rows. A repeated `by` name
  # reached `setorderv()` and failed with an internal length mismatch.
  for (role in list(list(v = by, nm = "by"), list(v = compare, nm = "compare"))) {
    repeated <- unique(role$v[duplicated(role$v)])
    if (length(repeated)) {
      daffiz_abort("daffiz_error_roles",
        paste0("`", role$nm, "` names column(s) more than once: ",
               fmt_names(repeated), "."),
        columns = repeated, role = role$nm)
    }
  }

  explicit <- unique(c(by, compare))
  if (length(explicit)) {
    unknown <- setdiff(explicit, kept)
    if (length(unknown)) {
      daffiz_abort("daffiz_error_roles",
        paste0("Requested column(s) not available for comparison: ",
               fmt_names(unknown),
               ".\n  Available after `exclude`: ", fmt_names(kept)),
        columns = unknown, available = kept)
    }
    overlap <- intersect(by, compare)
    if (length(overlap)) {
      daffiz_abort("daffiz_error_roles",
        paste0("Column(s) requested as both `by` and `compare`: ",
               fmt_names(overlap), "."),
        columns = overlap)
    }
  }

  if (is.null(by) && is.null(compare)) {
    by_out <- candidate_by
    compare_out <- candidate_measure
  } else if (!is.null(by) && is.null(compare)) {
    by_out <- by
    compare_out <- setdiff(candidate_measure, by)
  } else if (is.null(by) && !is.null(compare)) {
    # An unselected double must not silently become an identity column.
    by_out <- setdiff(candidate_by, compare)
    compare_out <- compare
  } else {
    by_out <- by
    compare_out <- compare
  }

  if (positional) by_out <- ".position"

  # Gate 8: at least one identity and one measure column resolved.
  if (!length(compare_out)) {
    # When the only numeric columns are integer on one side and double on the
    # other, gate 6 never gets to explain itself -- there is no measure left to
    # reach it. Say so here instead of sending the caller looking for a column
    # that does not exist.
    skew <- kept[vapply(
      kept, function(nm) is_numeric_type_skew(x[[nm]], y[[nm]]), logical(1)
    )]
    hint <- if (length(skew)) {
      paste0(
        "\n  Column(s) ", fmt_names(skew), " are unclassed integer on one ",
        "side and double on the other, so they are not shared doubles.\n",
        "  Name them in `compare=` to compare them as measures."
      )
    } else {
      ""
    }
    daffiz_abort("daffiz_error_roles",
      paste0("No numeric measures to compare.\n",
             "  Default measures are columns that are unclassed doubles in ",
             "both inputs;\n  Date and POSIXct columns are treated as ",
             "identity.\n",
             "  Use `compare=` to name measures explicitly.", hint),
      role = "compare", columns = skew)
  }
  if (!length(by_out)) {
    daffiz_abort("daffiz_error_roles",
      paste0("No identity columns to align rows on.\n",
             "  Use `by=` to name them, or `by = daffiz_row_number()` ",
             "to compare by position."),
      role = "by")
  }

  list(
    by = by_out,
    compare = compare_out,
    positional = positional,
    ignored = setdiff(kept, c(by_out, compare_out))
  )
}
