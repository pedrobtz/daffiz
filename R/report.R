# A text report of a comparison ------------------------------------------------
#
# The layout follows the summary of the Python package diffly (Quantco): a
# section each for the columns, the rows and the measures, a picture of how
# the rows of the two tables align, and the most frequent changes of each
# measure with a key to look one up by. The report is computed first, as plain
# data, and drawn by format(), so the numbers can be tested apart from the
# drawing.
#
# Box-drawing characters are written as \u escapes: R code must be ASCII.
# format() falls back to ASCII when the session cannot print them.

#' Report on a comparison, as text
#'
#' A bounded, readable account of a [diff_table()] result: which columns were
#' compared, how the rows of the two tables align, and, for each measure that
#' changed, how often it matches and its most frequent changes, each with an
#' example key. Printing the report draws it; the numbers are in the returned
#' list.
#'
#' A row counts as `changed` when any of its measures changed. With
#' `duplicates = "aggregate"` the units are keys rather than rows.
#'
#' The layout is adapted from the summary of the Python package
#' [diffly](https://github.com/Quantco/diffly).
#'
#' @param d A result of [diff_table()].
#' @param x_name,y_name The names the report gives the two tables.
#' @param top_k The number of most frequent changes shown per measure; 0
#'   hides them.
#' @param sample_k The number of keys shown of the rows found in only one
#'   table; 0 hides them.
#' @return A `daffiz_report`: a list holding the counts the report draws,
#'   with `format()` and `print()` methods. `format()` takes `unicode`, whether
#'   to draw with box-drawing characters (by default, when the session's
#'   encoding is UTF-8), and returns the lines of text.
#' @export
#' @examples
#' x <- data.frame(id = c("a", "b", "c"), value = c(1, 2, 3))
#' y <- data.frame(id = c("a", "b", "d"), value = c(1, 2.5, 4))
#' diff_report(diff_table(x, y))
#'
#' print(diff_report(diff_table(x, y)), unicode = FALSE)
diff_report <- function(d, x_name = "x", y_name = "y", top_k = 3L, sample_k = 5L) {
  check_diff_result(d)
  for (arg in c("x_name", "y_name")) {
    v <- get(arg)
    if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
      daffiz_abort("daffiz_error_input", sprintf("`%s` must be a single non-empty string", arg))
    }
  }
  for (arg in c("top_k", "sample_k")) {
    v <- get(arg)
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0 || v != trunc(v)) {
      daffiz_abort("daffiz_error_input", sprintf("`%s` must be a single whole number >= 0", arg))
    }
  }

  shape <- attr(d, "shape")
  map <- attr(d, "name_map")
  label_of <- function(cols) {
    lab <- map$name_x[match(cols, map$column)]
    lab[is.na(lab)] <- cols[is.na(lab)]
    as.character(lab)
  }
  # The identity is every user column of the result: user names never contain
  # a lowercase letter, internal ones always do. `row_number` is the one
  # internal key column. Read off the names, not key(d), so that a filtered
  # or re-sorted result still reports.
  ids <- names(d)[!grepl("[a-z]", names(d)) | names(d) == "row_number"]
  measures <- if ("metric" %in% names(d)) {
    m <- unique(as.character(d$metric))
    m[order(match(m, map$column))]
  } else {
    character()
  }

  # Rows: one per row of the result for the rows shape, one per row and
  # measure otherwise, so collapse the measures. Membership comes from row_id
  # (n_rows for groups), never from the value.
  present <- if (shape == "groups") "n_rows" else "row_id"
  row_by <- intersect(c(ids, "key_seq"), names(d))
  w <- d[, row_by, with = FALSE]
  set(w, j = "in_x", value = as.integer(!is.na(d[[paste0(present, "_x")]])))
  set(w, j = "in_y", value = as.integer(!is.na(d[[paste0(present, "_y")]])))
  set(w, j = "is_changed", value = as.integer(d$status == "changed"))
  # max() of an empty group warns, so two empty tables skip the grouping.
  rows <- if (nrow(w)) w[, lapply(.SD, max), by = row_by, .SDcols = c("in_x", "in_y", "is_changed")] else w
  in_x <- rows$in_x == 1L
  in_y <- rows$in_y == 1L
  joined <- in_x & in_y
  row_counts <- list(
    n_x = sum(in_x),
    n_y = sum(in_y),
    n_only_x = sum(in_x & !in_y),
    n_same = sum(joined & rows$is_changed == 0L),
    n_changed = sum(joined & rows$is_changed == 1L),
    n_only_y = sum(!in_x & in_y)
  )

  # Measures: the counts of diff_summary(), on the joined values only.
  by_measure <- NULL
  changes <- NULL
  if (length(measures)) {
    s <- diff_summary(d)
    by_measure <- data.table(
      measure = label_of(s$metric),
      n_same = s$n_same,
      n_changed = s$n_changed,
      match_rate = ifelse(s$n_same + s$n_changed > 0, s$n_same / (s$n_same + s$n_changed), NA_real_)
    )
    if (shape == "cells") {
      set(by_measure, j = "max_abs_diff", value = s$max_abs_diff)
      changes <- top_changes(d, ids, top_k)
      distinct <- changes$distinct
      set(by_measure, j = "n_distinct_changes",
        value = distinct$n_distinct[match(as.character(s$metric), distinct$metric)]
      )
      changes <- changes$top
      changes[, metric := label_of(metric)]
      setnames(changes, "metric", "measure")
    }
  }

  sample_keys <- function(side) {
    keep <- if (side == "x") in_x & !in_y else !in_x & in_y
    out <- rows[keep, ids, with = FALSE]
    out <- out[!duplicated(out)]
    out <- utils::head(out, sample_k)
    setnames(out, ids, label_of(ids))
    out
  }

  structure(
    list(
      x_name = x_name,
      y_name = y_name,
      mode = attr(d, "mode"),
      shape = shape,
      unit = if (shape == "groups") "keys" else "rows",
      identity = label_of(ids),
      measures = label_of(measures),
      dropped = attr(d, "dropped"),
      ignored = label_of(attr(d, "ignored")),
      truncated = attr(d, "truncated"),
      paired = "key_seq" %in% names(d),
      equal = all(d$status == "same"),
      rows = row_counts,
      by_measure = by_measure,
      changes = changes,
      only_x = sample_keys("x"),
      only_y = sample_keys("y"),
      top_k = as.integer(top_k)
    ),
    class = "daffiz_report"
  )
}

# The `top_k` most frequent (value_x, value_y) pairs among the changed values
# of each measure, the largest difference first among equally frequent ones,
# each with the key of its first row as an example.
top_changes <- function(d, ids, top_k) {
  ch <- d[status == "changed"]
  pairs <- ch[, list(n = .N, at = .I[1L], abs_diff = abs(diff[1L])), by = c("metric", "value_x", "value_y")]
  n_distinct <- pairs[, list(n_distinct = .N), by = "metric"]
  setorderv(pairs, c("metric", "n", "abs_diff"), order = c(1L, -1L, -1L), na.last = TRUE)
  top <- pairs[, utils::head(.SD, top_k), by = "metric"]
  top[, metric := as.character(metric)]
  top[, example := fmt_keys(ch[at, ids, with = FALSE])]
  top[, c("at", "abs_diff") := NULL]
  n_distinct[, metric := as.character(metric)]
  list(top = top[], distinct = n_distinct[])
}

#' @export
format.daffiz_report <- function(x, unicode = l10n_info()[["UTF-8"]], ...) {
  if (!is_flag(unicode)) {
    daffiz_abort("daffiz_error_input", "`unicode` must be TRUE or FALSE")
  }
  g <- report_glyphs(unicode)
  r <- x$rows
  noun <- function(n, one, many = paste0(one, "s")) paste(fmt_count(n), ifelse(n == 1, one, many))

  intro <- c(
    paste0("Identity: ", fmt_names(x$identity)),
    paste0(
      "Mode:     ", x$mode,
      if (x$mode == "benchmark") sprintf(" (%s is the benchmark)", x$x_name)
    ),
    if (x$paired && x$rows$n_x + x$rows$n_y > 0) {
      "Repeated keys were paired in arrival order (key_seq)."
    },
    if (x$shape == "groups") "Repeated keys: each key's values are compared as a multiset."
  )

  columns <- c(
    sprintf(
      "Compared %s: %s, %s.",
      noun(length(x$identity) + length(x$measures), "column"),
      noun(length(x$identity), "key"),
      noun(length(x$measures), "measure")
    ),
    if (length(x$dropped)) sprintf("Only in %s, dropped: %s.", x$y_name, fmt_names(x$dropped)),
    if (length(x$ignored)) sprintf("In neither the identity nor the measures: %s.", fmt_names(x$ignored)),
    if (nrow(x$truncated)) {
      c(
        sprintf("Truncated by the cast to %s's types:", x$x_name),
        sprintf("  %s: %s (%s)", x$truncated$column, noun(x$truncated$n, "value"), x$truncated$examples)
      )
    }
  )
  notes <- length(x$dropped) || length(x$ignored) || nrow(x$truncated)

  body <- indent(intro)
  if (x$equal) {
    empty <- r$n_x + r$n_y == 0
    verdict <- if (empty) {
      sprintf("--- %s and %s are both empty ---", x$x_name, x$y_name)
    } else {
      sprintf(
        "--- %s and %s match exactly (%s, %s) ---", x$x_name, x$y_name,
        noun(r$n_x, sub("s$", "", x$unit), x$unit), noun(length(x$measures), "measure")
      )
    }
    body <- c(body, "", indent(verdict))
    if (notes) body <- c(body, section("Columns", columns, g))
  } else {
    body <- c(
      body,
      section("Columns", columns, g),
      section("Rows", c(row_counts_lines(x), "", row_diagram(x, g)), g),
      if (length(x$measures)) section("Measures", measure_lines(x, g), g),
      sample_lines(x, "x", g),
      sample_lines(x, "y", g)
    )
  }

  body <- sub("\\s+$", "", body)
  width <- max(40L, nchar(body, type = "width"))
  title <- "daffiz report"
  left <- (width - 2L - nchar(title)) %/% 2L
  header <- c(
    paste0(g$htl, strrep(g$hh, width - 2L), g$htr),
    paste0(g$hv, strrep(" ", left), title, strrep(" ", width - 2L - nchar(title) - left), g$hv),
    paste0(g$hbl, strrep(g$hh, width - 2L), g$hbr)
  )
  c(header, body)
}

#' @export
print.daffiz_report <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# Layout ------------------------------------------------------------------------

indent <- function(lines, n = 3L) {
  ifelse(nzchar(lines), paste0(strrep(" ", n), lines), lines)
}

section <- function(title, lines, g) {
  c("", paste0(" ", title), paste0(" ", strrep(g$under, nchar(title))), indent(lines))
}

# "x rows   (+33.33%)   y rows" over the two counts, each column centred.
row_counts_lines <- function(x) {
  r <- x$rows
  change <- if (r$n_x == 0) {
    ""
  } else if (r$n_y == r$n_x) {
    "(no change)"
  } else {
    sprintf("(%s%s)", if (r$n_y > r$n_x) "+" else "-", fmt_pct(abs(r$n_y / r$n_x - 1), clamp = FALSE))
  }
  top <- c(paste(x$x_name, x$unit), "", paste(x$y_name, x$unit))
  bottom <- c(fmt_count(r$n_x), change, fmt_count(r$n_y))
  width <- pmax(nchar(top, type = "width"), nchar(bottom, type = "width")) + c(0L, 4L, 0L)
  c(paste(centre(top, width), collapse = ""), paste(centre(bottom, width), collapse = ""))
}

# Two stacks of boxes, x on the left and y on the right, offset so that the
# rows found in both tables sit side by side: x's own rows above them, y's own
# below. Each band is labelled with its count and share: of x's rows for the
# rows only in x, of the joined rows for `same` and `changed`, of y's rows for
# the rows only in y.
row_diagram <- function(x, g) {
  r <- x$rows
  n_joined <- r$n_same + r$n_changed
  # diff_table() refuses a comparison that aligns nothing, and two empty
  # tables are equal, so this report always has a joined band.
  if (n_joined == 0) return(character())

  bands <- list(
    only_x = c(r$n_only_x, paste("only in", x$x_name), fmt_pct(r$n_only_x / r$n_x)),
    same = c(r$n_same, "same", fmt_pct(r$n_same / n_joined)),
    changed = c(r$n_changed, "changed", fmt_pct(r$n_changed / n_joined)),
    only_y = c(r$n_only_y, paste("only in", x$y_name), fmt_pct(r$n_only_y / r$n_y))
  )
  shown <- c(r$n_only_x, r$n_same, r$n_changed, r$n_only_y) > 0
  bands <- bands[shown]
  counts <- vapply(bands, function(b) fmt_count(as.numeric(b[[1L]])), "")
  kinds <- vapply(bands, `[[`, "", 2L)
  pcts <- vapply(bands, function(b) paste0("(", b[[3L]], ")"), "")
  labels <- paste(
    formatC(counts, width = max(nchar(counts))),
    formatC(kinds, width = -max(nchar(kinds, type = "width"))),
    formatC(pcts, width = max(nchar(pcts)))
  )
  names(labels) <- names(bands)
  label_width <- max(nchar(labels, type = "width"))

  strip <- function(l, m, r, fill) paste0(l, paste(rep(fill, 5L), collapse = m), r)
  top <- strip(g$btl, g$btt, g$btr, g$bh)
  sep <- strip(g$bml, g$bmx, g$bmr, g$bmh)
  bottom <- strip(g$bbl, g$bbt, g$bbr, g$bh)
  cells <- function(ch) strip(g$bv, g$biv, g$bv, ch)
  blank <- strrep(" ", nchar(top))
  dashes <- strrep(g$dash, 3L)
  run <- strrep(g$dash, label_width + 4L)
  label <- function(band) paste0("  ", formatC(labels[[band]], width = -label_width), "  ")

  has <- function(band) band %in% names(labels)
  out <- character()
  add <- function(...) out <<- c(out, paste0(...))
  if (has("only_x")) {
    add(top)
    add(cells("-"), "   ", blank, label("only_x"))
  }
  add(if (has("only_x")) sep else top, dashes, top, run, g$open)
  if (has("same")) add(cells(" "), " = ", cells(" "), label("same"), g$bar)
  if (has("same") && has("changed")) {
    add(sep, dashes, sep, run, g$tee, "  ", fmt_count(n_joined), " joined")
  }
  if (has("changed")) add(cells(" "), " ", g$ne, " ", cells(" "), label("changed"), g$bar)
  add(bottom, dashes, if (has("only_y")) sep else bottom, run, g$close)
  if (has("only_y")) {
    add(blank, "   ", cells("+"), label("only_y"))
    add(blank, "   ", bottom)
  }
  out
}

measure_lines <- function(x, g) {
  m <- x$by_measure
  changed <- m[n_changed > 0]
  rest <- nrow(m) - nrow(changed)
  if (!nrow(changed)) {
    return(sprintf("Every measure matches on every row found in both tables (%s).", fmt_count(nrow(m))))
  }
  cells <- x$shape == "cells"
  show_changes <- cells && x$top_k > 0L
  header <- c("Measure", "Match rate", if (cells) "Max |diff|", if (show_changes) "Top changes")
  rows <- lapply(seq_len(nrow(changed)), function(i) {
    row <- list(changed$measure[i], fmt_pct(changed$match_rate[i]))
    if (cells) {
      v <- changed$max_abs_diff[i]
      row <- c(row, if (is.na(v)) "-" else format(v, digits = 4L))
    }
    if (show_changes) {
      ch <- x$changes[measure == changed$measure[i]]
      lines <- sprintf(
        "%s -> %s (%sx, e.g. %s)",
        fmt_values(ch$value_x), fmt_values(ch$value_y), fmt_count(ch$n), ch$example
      )
      more <- changed$n_distinct_changes[i] - nrow(ch)
      if (more > 0) lines <- c(lines, sprintf("(...and %s %s)", fmt_count(more), if (more == 1) "other" else "others"))
      row <- c(row, list(lines))
    }
    row
  })
  c(
    draw_table(header, rows, c("left", "right", if (cells) "right", if (show_changes) "left"), g),
    if (rest) {
      sprintf(
        "%s %s on every row found in both tables.",
        fmt_count(rest), if (rest == 1) "other measure matches" else "other measures match"
      )
    }
  )
}

sample_lines <- function(x, side, g) {
  keys <- x[[paste0("only_", side)]]
  n <- x$rows[[paste0("n_only_", side)]]
  if (!nrow(keys) || !n) return(character())
  max_cols <- 5L
  shown <- utils::head(names(keys), max_cols)
  header <- c(shown, if (length(keys) > max_cols) "...")
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    c(as.list(vapply(shown, function(col) fmt_cell(keys[[col]][i]), "")), if (length(keys) > max_cols) "...")
  })
  name <- if (side == "x") x$x_name else x$y_name
  more <- n - nrow(keys)
  section(
    sprintf("%s only in %s", if (x$unit == "keys") "Keys" else "Rows", name),
    c(
      draw_table(header, rows, rep("left", length(header)), g),
      if (more > 0) sprintf("(...and %s more)", fmt_count(more))
    ),
    g
  )
}

# A boxed table. `rows` is a list of rows, each a list of cells; a cell may
# hold several lines, and then every row is ruled off from the next.
draw_table <- function(header, rows, align, g) {
  width <- vapply(seq_along(header), function(j) {
    max(nchar(header[j], type = "width"), unlist(lapply(rows, function(r) nchar(r[[j]], type = "width"))))
  }, numeric(1L))
  height <- vapply(rows, function(r) max(lengths(r)), integer(1L))
  pad <- function(s, w, a) {
    gap <- strrep(" ", w - nchar(s, type = "width"))
    if (a == "right") paste0(gap, s) else paste0(s, gap)
  }
  rule <- function(l, m, r) paste0(l, paste(strrep(g$th, width + 2L), collapse = m), r)
  line <- function(cells) paste0(g$tv, paste0(" ", mapply(pad, cells, width, align), " ", collapse = g$tv), g$tv)
  out <- c(rule(g$ttl, g$ttt, g$ttr), line(header), rule(g$tml, g$tmx, g$tmr))
  for (i in seq_along(rows)) {
    for (k in seq_len(height[i])) {
      out <- c(out, line(vapply(rows[[i]], function(cell) if (k <= length(cell)) cell[[k]] else "", "")))
    }
    if (i < length(rows) && any(height > 1L)) out <- c(out, rule(g$tml, g$tmx, g$tmr))
  }
  c(out, rule(g$tbl, g$tbt, g$tbr))
}

centre <- function(s, width) {
  gap <- width - nchar(s, type = "width")
  left <- gap %/% 2L
  paste0(strrep(" ", left), s, strrep(" ", gap - left))
}

# Formatting ------------------------------------------------------------------

# Counts can pass 2^31, so they are never formatted with %d.
fmt_count <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# A share as a percentage with two decimals. With `clamp`, it reads 0% or 100%
# only when it is exactly that: one changed row in a billion is not 0.00%.
fmt_pct <- function(p, clamp = TRUE) {
  p <- 100 * p
  if (clamp) {
    inside <- !is.na(p) & p > 0 & p < 100
    p[inside] <- pmin(pmax(p[inside], 0.01), 99.99)
  }
  ifelse(is.na(p), "-", sprintf("%.2f%%", p))
}

# One value at a time, so each gets its own digits: format() on a vector pads
# every element to the precision of the widest.
fmt_cell <- function(v) {
  if (is.na(v)) return("NA")
  if (is.character(v)) return(paste0("\"", v, "\""))
  if (is.double(v) && is.null(oldClass(v))) return(format(v, digits = 7L))
  as.character(format(v))
}

fmt_values <- function(v) vapply(seq_along(v), function(i) fmt_cell(v[i]), "")

# Keys as text: the value of a single key column, a tuple of several.
fmt_keys <- function(keys) {
  if (!length(keys)) return(rep("", nrow(keys)))
  out <- do.call(paste, c(lapply(keys, fmt_values), sep = ", "))
  if (length(keys) > 1L) paste0("(", out, ")") else out
}

report_glyphs <- function(unicode) {
  if (unicode) {
    list(
      # the title: heavy
      htl = "\u250f", hh = "\u2501", htr = "\u2513", hv = "\u2503", hbl = "\u2517", hbr = "\u251b",
      # the row diagram: heavy edges, light inside
      btl = "\u250f", btt = "\u252f", btr = "\u2513", bh = "\u2501", bv = "\u2503", biv = "\u2502",
      bml = "\u2520", bmx = "\u253c", bmr = "\u2528", bmh = "\u2500",
      bbl = "\u2517", bbt = "\u2537", bbr = "\u251b",
      dash = "\u254c", open = "\u256e", bar = "\u2502", tee = "\u251c\u2574", close = "\u256f",
      ne = "\u2260",
      # tables: light
      ttl = "\u250c", ttt = "\u252c", ttr = "\u2510", th = "\u2500", tv = "\u2502",
      tml = "\u251c", tmx = "\u253c", tmr = "\u2524",
      tbl = "\u2514", tbt = "\u2534", tbr = "\u2518",
      under = "\u2594"
    )
  } else {
    list(
      htl = "+", hh = "=", htr = "+", hv = "|", hbl = "+", hbr = "+",
      btl = "+", btt = "+", btr = "+", bh = "=", bv = "|", biv = "|",
      bml = "+", bmx = "+", bmr = "+", bmh = "-",
      bbl = "+", bbt = "+", bbr = "+",
      dash = ".", open = "+", bar = "|", tee = "+-", close = "+",
      ne = "!",
      ttl = "+", ttt = "+", ttr = "+", th = "-", tv = "|",
      tml = "+", tmx = "+", tmr = "+",
      tbl = "+", tbt = "+", tbr = "+",
      under = "-"
    )
  }
}
