# Fail-fast preflight ---------------------------------------------------------
#
# Comparison is a sequence of gates (design plan section 4.1). A failed gate
# stops immediately, before any sorting, melting, joining or summary work. Gate
# order is load-bearing and is covered by tests: a column-set mismatch must be
# reported before duplicate detection or melting, because it usually means the
# output contract changed and a partial numeric comparison would hide that.

is_table_like <- function(x) is.data.frame(x)

# Gates 1-4. Returns the kept (post-exclude) column names in `x` order.
preflight_columns <- function(x, y, exclude, x_name, y_name) {
  unknown <- character()
  # Gate 1: table-like inputs with unique column names.
  if (!is_table_like(x) || !is_table_like(y)) {
    daffiz_abort("daffiz_error_input",
      "Both inputs must be data.frames (data.table and tibble are accepted).")
  }
  for (side in list(list(d = x, nm = x_name), list(d = y, nm = y_name))) {
    nms <- names(side$d)
    if (is.null(nms) || anyNA(nms) || any(nms == "")) {
      daffiz_abort("daffiz_error_columns",
        paste0("`", side$nm, "` has missing or empty column names."),
        side = side$nm)
    }
    dup <- unique(nms[duplicated(nms)])
    if (length(dup)) {
      daffiz_abort("daffiz_error_columns",
        paste0("`", side$nm, "` has duplicate column names: ",
               fmt_names(dup), "."),
        side = side$nm, columns = dup)
    }
  }

  # Gate 3: resolve `exclude` logically. The inputs are not mutated here.
  # Resolved before gate 2 so that a reserved-named column can be dropped with
  # `exclude=` instead of forcing the caller to rename it.
  if (length(exclude)) {
    if (!is.character(exclude)) {
      daffiz_abort("daffiz_error_columns", "`exclude` must be a character vector.")
    }
    unknown <- setdiff(exclude, union(names(x), names(y)))
    if (length(unknown)) {
      daffiz_warn("daffiz_warning_exclude",
        paste0("Excluded column(s) not found in either input: ",
               fmt_names(unknown), "."),
        columns = unknown)
    }
  }
  keep_x <- setdiff(names(x), exclude)
  keep_y <- setdiff(names(y), exclude)

  # Gate 2: no collision with the reserved result column names, among the
  # columns that survive `exclude`.
  clash <- intersect(union(keep_x, keep_y), daffiz_reserved_names)
  if (length(clash)) {
    daffiz_abort("daffiz_error_columns",
      paste0("Input column name(s) are reserved by daffiz results: ",
             fmt_names(clash), ".\n  Rename them, or drop them with `exclude=`."),
      columns = clash)
  }

  # Gate 4: identical column sets. Order may differ.
  only_x <- setdiff(keep_x, keep_y)
  only_y <- setdiff(keep_y, keep_x)
  if (length(only_x) || length(only_y)) {
    daffiz_abort("daffiz_error_columns",
      paste0(
        "Column sets differ after `exclude`.\n",
        "  only in `", x_name, "`: ", fmt_names(only_x), "\n",
        "  only in `", y_name, "`: ", fmt_names(only_y)
      ),
      only_x = only_x, only_y = only_y)
  }

  attr(keep_x, "unknown_exclude") <- unknown
  keep_x
}

# Attributes that change what an equal underlying value *means*, and so must
# agree before two identity columns can be aligned. `levels` reinterprets a
# factor's integer codes. `units` rescales the numbers a difftime (or an hms,
# or a units-package vector) stores: 60 "secs" and 1 "mins" are the same
# duration but different doubles, and the join compares the doubles.
#
# `tzone` is deliberately absent. Two POSIXct columns in different time zones
# hold the *same* epoch seconds, so they align correctly; rejecting them would
# be a false alarm.
daffiz_identity_attributes <- c("levels", "units")

# Compact description of a column's join-relevant identity.
column_signature <- function(column) {
  sig <- list(type = typeof(column), class = paste(class(column), collapse = "/"))
  for (nm in daffiz_identity_attributes) {
    value <- attr(column, nm, exact = TRUE)
    if (!is.null(value)) sig[[nm]] <- value
  }
  sig
}

signature_text <- function(sig) {
  out <- paste0(sig$class, " (", sig$type, ")")
  for (nm in daffiz_identity_attributes) {
    if (!is.null(sig[[nm]])) {
      out <- paste0(out, " ", nm, ": ",
                    fmt_names(as.character(sig[[nm]]), 5L))
    }
  }
  out
}

# An unclassed integer or double. Both sides being one of these, but not the
# same one, is the common `read.csv` vs `fread` skew; it earns its own hint
# because the generic type-mismatch message sends the caller looking for a
# class difference that is not there.
is_plain_numeric <- function(column) {
  typeof(column) %in% c("integer", "double") && !is.object(column)
}

# Both sides plain numeric, but not the *same* plain numeric type. Two integer
# columns are an ordinary integer identity, not a skew.
is_numeric_type_skew <- function(cx, cy) {
  is_plain_numeric(cx) && is_plain_numeric(cy) &&
    !identical(typeof(cx), typeof(cy))
}

# Gates 6-7.
preflight_types <- function(x, y, by, compare, x_name, y_name) {
  # Gate 6a: identity columns must have a type data.table can join on. This is
  # an allow-list rather than a deny-list, so an unsupported type fails here
  # with a daffiz condition instead of deep inside the grouped duplicate scan
  # with a raw data.table message. `list`, `complex` and `raw` all reach that
  # point otherwise, and `is.atomic()` does not separate them: it is TRUE for
  # complex and raw. Classed identity columns ride on these types -- factor is
  # integer, Date/POSIXct/difftime/integer64 are double.
  bad <- character()
  detail <- character()
  for (nm in by) {
    if (identical(nm, ".position")) next
    for (side in list(list(d = x, nm = x_name), list(d = y, nm = y_name))) {
      col <- side$d[[nm]]
      if (!typeof(col) %in% daffiz_joinable_types) {
        bad <- c(bad, nm)
        detail <- c(detail, paste0(
          "  ", nm, " in ", side$nm, ": ",
          signature_text(column_signature(col))
        ))
      }
    }
  }
  if (length(bad)) {
    daffiz_abort("daffiz_error_types",
      paste0(
        "Identity column(s) have a type that cannot align rows:\n",
        paste(unique(detail), collapse = "\n"),
        "\n  Identities must be ", fmt_names(daffiz_joinable_types),
        ", or a class built on one of those.",
        "\n  Drop them with `exclude=`, or reduce them to a supported key."
      ),
      columns = unique(bad), role = "by")
  }

  # Gate 6b: identity columns must agree in type, class and join attributes.
  bad <- character()
  detail <- character()
  numeric_skew <- character()
  for (nm in by) {
    if (identical(nm, ".position")) next
    sx <- column_signature(x[[nm]])
    sy <- column_signature(y[[nm]])
    if (!identical(sx, sy)) {
      bad <- c(bad, nm)
      detail <- c(detail, paste0(
        "  ", nm, ": ", x_name, " is ", signature_text(sx),
        "; ", y_name, " is ", signature_text(sy)
      ))
      if (is_numeric_type_skew(x[[nm]], y[[nm]])) {
        numeric_skew <- c(numeric_skew, nm)
      }
    }
  }
  if (length(bad)) {
    hint <- if (length(numeric_skew)) {
      paste0(
        "\n  Column(s) ", fmt_names(numeric_skew), " are unclassed integer on ",
        "one side and double on the other, so neither side's default role ",
        "applies.\n  Pass them in `compare=` to compare them as measures, or ",
        "coerce both sides to the same type to keep them as identity."
      )
    } else {
      ""
    }
    daffiz_abort("daffiz_error_types",
      paste0("Identity column(s) differ between inputs:\n",
             paste(detail, collapse = "\n"), hint),
      columns = bad, role = "by")
  }

  # Gate 7: measures must be unclassed integer/double, promotable to double.
  bad <- character()
  detail <- character()
  for (nm in compare) {
    for (side in list(list(d = x, nm = x_name), list(d = y, nm = y_name))) {
      col <- side$d[[nm]]
      if (!typeof(col) %in% c("integer", "double") || is.object(col)) {
        bad <- c(bad, nm)
        detail <- c(detail, paste0(
          "  ", nm, " in ", side$nm, ": ", signature_text(column_signature(col))
        ))
      }
    }
  }
  if (length(bad)) {
    daffiz_abort("daffiz_error_types",
      paste0(
        "Measure column(s) are not unclassed integer or double:\n",
        paste(unique(detail), collapse = "\n"),
        "\n  Integer measures are promoted to double; classed columns ",
        "(Date, POSIXct, factor) are never compared as measures."
      ),
      columns = unique(bad), role = "compare")
  }

  invisible(TRUE)
}

# Gate 9. Resolves a scalar or `.default`-bearing named tolerance into a named
# numeric vector covering every resolved measure.
resolve_tolerance <- function(tol, measures, arg_name) {
  if (is.null(tol)) tol <- 0

  if (!is.numeric(tol) || !length(tol)) {
    daffiz_abort("daffiz_error_tolerance",
      paste0("`", arg_name, "` must be a non-empty numeric vector."),
      argument = arg_name)
  }
  if (anyNA(tol) || any(!is.finite(tol)) || any(tol < 0)) {
    daffiz_abort("daffiz_error_tolerance",
      paste0("`", arg_name, "` must be finite and non-negative."),
      argument = arg_name)
  }

  nms <- names(tol)
  if (is.null(nms)) {
    if (length(tol) != 1L) {
      daffiz_abort("daffiz_error_tolerance",
        paste0("Unnamed `", arg_name, "` must be a single value; ",
               "use a named vector with `.default` for per-column tolerances."),
        argument = arg_name)
    }
    out <- rep(as.numeric(tol), length(measures))
    names(out) <- measures
    return(out)
  }

  if (any(nms == "")) {
    daffiz_abort("daffiz_error_tolerance",
      paste0("Every element of a named `", arg_name,
             "` must be named; use `.default` for the fallback."),
      argument = arg_name)
  }
  repeated <- unique(nms[duplicated(nms)])
  if (length(repeated)) {
    daffiz_abort("daffiz_error_tolerance",
      paste0("`", arg_name, "` names column(s) more than once: ",
             fmt_names(repeated), ".\n  Only the first value would be used."),
      argument = arg_name, columns = repeated)
  }
  unknown <- setdiff(setdiff(nms, ".default"), measures)
  if (length(unknown)) {
    daffiz_abort("daffiz_error_tolerance",
      paste0("`", arg_name, "` names column(s) that are not compared measures: ",
             fmt_names(unknown), ".\n  Measures: ", fmt_names(measures)),
      argument = arg_name, columns = unknown)
  }

  default <- if (".default" %in% nms) as.numeric(tol[[".default"]]) else 0
  out <- rep(default, length(measures))
  names(out) <- measures
  named <- setdiff(nms, ".default")
  if (length(named)) out[named] <- as.numeric(tol[named])
  out
}
