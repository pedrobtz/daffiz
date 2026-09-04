# Difference map --------------------------------------------------------------
#
# The visualization layer is deliberately one view: an Amelia-style difference
# map, observations down the y axis and numeric measures across the x axis,
# filled green where the two tables agree and red where they do not. It is a
# view over the canonical cell table and takes no part in alignment, tolerance,
# or classification.
#
# Measure- and record-level diagnostics are answered by data accessors
# (`column_summary()`, `diff_columns()`, `row_summary()`, `diff_rows()`) rather
# than by additional chart types.

# Okabe-Ito bluish green and vermillion. They read as "green" and "red" to
# trichromats, and unlike true red/green they separate on both the blue-yellow
# axis and in lightness, so they stay distinguishable under deuteranopia and
# protanopia. The neutral mid-point is only reached by binned tiles, which are
# genuinely a mixture rather than one class or the other.
daffiz_match_colour <- "#009E73"
daffiz_mismatch_colour <- "#D55E00"
daffiz_mixed_colour <- "#E6E6E6"

# Redundant, non-colour encoding of the four cell classes. The equal majority is
# deliberately unmarked: absence of a mark is itself distinguishable, and it
# keeps the map from drowning in ink where nothing happened.
#
# ASCII only. A delta glyph reads better but fails to encode on the default
# `pdf()` device ("conversion failure in mbcsToSbcs"), which is the device
# `R CMD check` renders examples to.
daffiz_tile_symbols <- c(
  equal = "", diff = "!", x_only = "X", y_only = "Y"
)

resolve_plot_limit <- function(x, name) {
  if (is.null(x)) return(Inf)
  if (!is.numeric(x) || is.object(x) || length(x) != 1L ||
      is.na(x) || x < 1 || (!is.infinite(x) && x != trunc(x))) {
    daffiz_abort(
      "daffiz_error_plot_limit",
      sprintf("`%s` must be a positive whole number, Inf, or NULL.", name),
      argument = name,
      value = x
    )
  }
  as.double(x)
}

format_plot_row <- function(row_x, row_y) {
  x <- ifelse(is.na(row_x), "-", as.character(row_x))
  y <- ifelse(is.na(row_y), "-", as.character(row_y))
  paste0("x:", x, " / y:", y)
}

format_bin_label <- function(row_start, row_end) {
  fifelse(
    row_start == row_end,
    paste0("row ", row_start),
    paste0("rows ", row_start, "-", row_end)
  )
}

# One record per aligned or side-only source row, in *original position* order.
#
# Position order is the point of a difference map: it is what makes a run of
# regressions in one region of the input visible as a band. Ranking rows by how
# affected they are would destroy exactly the pattern the plot exists to show,
# so truncation bins contiguous rows instead of selecting the worst ones.
plot_row_index <- function(cmp, columns) {
  cells <- cmp$cells[.metric %chin% columns]
  if (!nrow(cells)) {
    return(list(
      cells = cells,
      rows = data.table(
        .plot_row_id = integer(),
        .row_x = integer(),
        .row_y = integer(),
        row_order = integer()
      )
    ))
  }

  cells <- copy(cells)
  cells[, .plot_row_id := .GRP, by = c(".row_x", ".row_y")]
  rows <- cells[, list(.row_x = .row_x[1L], .row_y = .row_y[1L]),
                by = .plot_row_id]
  rows[, .row_anchor := fifelse(!is.na(.row_x), .row_x, .row_y)]
  setorderv(rows, c(".row_anchor", ".row_x", ".row_y"), na.last = TRUE)
  rows[, `:=`(row_order = seq_len(.N), .row_anchor = NULL)]
  list(cells = cells, rows = rows)
}

empty_map_data <- function() {
  data.table(
    row_rank = integer(),
    row_label = character(),
    row_start = integer(),
    row_end = integer(),
    .row_x = integer(),
    .row_y = integer(),
    column = character(),
    match_type = factor(character(), levels = daffiz_match_levels),
    symbol = character(),
    matched = logical(),
    n_cells = integer(),
    n_mismatched = integer(),
    fraction_mismatched = numeric(),
    value_x = numeric(),
    value_y = numeric(),
    diff = numeric(),
    abs_diff = numeric(),
    binned = logical(),
    n_rows = integer(),
    n_rows_total = integer(),
    n_rows_shown = integer()
  )
}

# One tile per source row and measure. `fraction_mismatched` is 0 or 1 here, so
# the same fill mapping serves both this and the binned variant.
plot_map_rows <- function(prepared, columns, n_total) {
  selected <- prepared$rows[, .(
    .plot_row_id, .row_x, .row_y,
    row_rank = row_order,
    row_label = format_plot_row(.row_x, .row_y)
  )]

  out <- selected[prepared$cells, on = ".plot_row_id", nomatch = 0L]
  out[, `:=`(
    column = .metric,
    match_type = factor(as.character(.match_type), levels = daffiz_match_levels),
    symbol = daffiz_tile_symbols[
      as.character(.match_type)
    ],
    matched = .match_type == "equal",
    n_cells = 1L,
    n_mismatched = as.integer(.match_type != "equal"),
    fraction_mismatched = as.numeric(.match_type != "equal"),
    value_x = .value_x,
    value_y = .value_y,
    diff = .diff,
    abs_diff = .abs_diff,
    binned = FALSE,
    row_start = row_rank,
    row_end = row_rank,
    n_rows = 1L,
    n_rows_total = n_total,
    n_rows_shown = n_total,
    .measure_order = match(.metric, columns)
  )]
  setorderv(out, c("row_rank", ".measure_order"))
  out[, .(
    row_rank, row_label, row_start, row_end, .row_x, .row_y, column,
    match_type, symbol, matched, n_cells, n_mismatched, fraction_mismatched,
    value_x, value_y, diff, abs_diff, binned, n_rows, n_rows_total,
    n_rows_shown
  )]
}

# One tile per contiguous row bin and measure, filled by the share of cells in
# the bin that do not match. Every eligible row is represented; nothing is
# dropped, which is what keeps the map honest at scale.
plot_map_bins <- function(prepared, columns, n_total, row_bins) {
  rows <- copy(prepared$rows)
  n_bins <- as.integer(min(n_total, row_bins))
  rows[, row_bin := pmin(n_bins, as.integer(ceiling(row_order * n_bins / .N)))]
  rows[, `:=`(
    row_start = min(row_order),
    row_end = max(row_order),
    n_rows = .N
  ), by = row_bin]

  work <- rows[prepared$cells, on = ".plot_row_id", nomatch = 0L]
  out <- work[, .(
    row_start = row_start[1L],
    row_end = row_end[1L],
    n_rows = n_rows[1L],
    n_cells = .N,
    n_mismatched = sum(.match_type != "equal")
  ), by = .(row_rank = row_bin, column = .metric)]

  out[, `:=`(
    row_label = format_bin_label(row_start, row_end),
    match_type = factor(NA_character_, levels = daffiz_match_levels),
    symbol = NA_character_,
    matched = n_mismatched == 0L,
    fraction_mismatched = n_mismatched / n_cells,
    .row_x = NA_integer_,
    .row_y = NA_integer_,
    value_x = NA_real_,
    value_y = NA_real_,
    diff = NA_real_,
    abs_diff = NA_real_,
    binned = TRUE,
    n_rows_total = n_total,
    n_rows_shown = n_bins,
    .measure_order = match(column, columns)
  )]
  setorderv(out, c("row_rank", ".measure_order"))
  out[, .(
    row_rank, row_label, row_start, row_end, .row_x, .row_y, column,
    match_type, symbol, matched, n_cells, n_mismatched, fraction_mismatched,
    value_x, value_y, diff, abs_diff, binned, n_rows, n_rows_total,
    n_rows_shown
  )]
}

#' Data behind the difference map
#'
#' Returns the exact table [plot_diff()] draws, without requiring `ggplot2`.
#' This makes row ordering, binning, and the match classification of every tile
#' visible and testable.
#'
#' There is one record per tile. Rows appear in original source position order,
#' never ranked, so that regressions concentrated in one region of the input
#' remain visible as a band. When the comparison has more than `max_rows`
#' aligned or side-only records, contiguous rows are aggregated into at most
#' `row_bins` bins and `binned` is `TRUE`; every eligible row is represented
#' either way, and no row is ever discarded.
#'
#' `fraction_mismatched` is the share of cells behind the tile that are not
#' `equal`. Unbinned tiles cover one cell, so it is `0` or `1` there, and
#' `match_type`, `symbol` and the source values are populated. Binned tiles
#' cover several cells, so those per-cell columns are `NA`. `row_start` and
#' `row_end` give the tile's extent in display order either way.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param columns Optional character vector of compared measures to include.
#'   Defaults to every compared measure, in input-column order.
#' @param max_rows Largest number of records drawn one tile per row. Above
#'   this, rows are binned. `NULL` or `Inf` never bins.
#' @param row_bins Maximum number of row bins used when binning applies.
#'
#' @return A `data.table` with one row per tile.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27))
#' cmp <- compare_dt(x, y)
#' plot_data(cmp)
plot_data <- function(cmp, columns = NULL, max_rows = 500L, row_bins = 100L) {
  validate_comparison(cmp)
  columns <- validate_measure_filter(columns, cmp$settings$compare)
  max_rows <- resolve_plot_limit(max_rows, "max_rows")
  row_bins <- resolve_plot_limit(row_bins, "row_bins")

  # Keep the caller's measure order when they gave one, otherwise the order the
  # measures have in the inputs. Either way it is a stable, meaningful order
  # rather than a ranking, so adjacent measures stay adjacent on the axis.
  columns <- cmp$settings$compare[cmp$settings$compare %chin% columns]

  prepared <- plot_row_index(cmp, columns)
  n_total <- nrow(prepared$rows)
  if (!n_total) return(empty_map_data())

  if (n_total <= max_rows) {
    plot_map_rows(prepared, columns, n_total)
  } else {
    plot_map_bins(prepared, columns, n_total, row_bins)
  }
}

# Plot construction -----------------------------------------------------------

check_ggplot2 <- function(available = requireNamespace("ggplot2", quietly = TRUE)) {
  if (!isTRUE(available)) {
    daffiz_abort(
      "daffiz_error_ggplot2",
      paste0(
        "`plot_diff()` requires the suggested package 'ggplot2'.\n",
        "  Install it with `install.packages(\"ggplot2\")`."
      )
    )
  }
  invisible(TRUE)
}

map_subtitle <- function(data) {
  if (!nrow(data)) return("No eligible rows")
  total <- format(data$n_rows_total[1L], big.mark = ",")
  if (!data$binned[1L]) {
    return(sprintf("All %s aligned or side-only rows", total))
  }
  # Kept short: a longer line is silently clipped at typical figure widths, and
  # a clipped disclosure discloses nothing.
  sprintf(
    "All %s rows, aggregated into %s contiguous bins",
    total,
    format(data$n_rows_shown[1L], big.mark = ",")
  )
}

#' Plot the difference map
#'
#' Draws an Amelia-style difference map from [plot_data()]: one column per
#' numeric measure, one row per source record, green where the two tables agree
#' and red where they do not. Comparison and every data accessor continue to
#' work when `ggplot2` is not installed.
#'
#' Rows keep their original source order, so regressions concentrated in one
#' part of the input read as a band. Large comparisons aggregate contiguous
#' rows into bins rather than dropping rows, and the subtitle says so; a binned
#' tile is shaded by the share of its cells that differ.
#'
#' The fill colours are the Okabe-Ito bluish green and vermillion, which read as
#' green and red but stay distinguishable under the common forms of colour
#' blindness. When the map is small enough to label, each tile also carries a
#' mark: `!` different, `X` only in `x`, `Y` only in `y`, and nothing at all
#' where the two tables agree. The classification therefore never depends on
#' colour alone.
#'
#' @inheritParams plot_data
#' @return A `ggplot` object.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27))
#' cmp <- compare_dt(x, y)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_diff(cmp)
#' }
plot_diff <- function(cmp, columns = NULL, max_rows = 500L, row_bins = 100L) {
  check_ggplot2()
  data <- plot_data(
    cmp,
    columns = columns,
    max_rows = max_rows,
    row_bins = row_bins
  )

  data <- copy(data)
  data[, `:=`(
    column_display = factor(column, levels = unique(column)),
    row_display = factor(row_label, levels = rev(unique(row_label))),
    # A binned tile is a proportion and needs a continuous ramp; an unbinned
    # tile is one of two classes, and mapping that onto a colourbar invents a
    # gradient of intermediate states the data cannot contain.
    fill_class = factor(
      fifelse(matched, "Match", "Differs"),
      levels = c("Match", "Differs")
    )
  )]
  binned <- nrow(data) > 0L && data$binned[1L]

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = column_display, y = row_display)
  ) +
    (if (binned) {
      ggplot2::geom_tile(ggplot2::aes(fill = fraction_mismatched))
    } else {
      ggplot2::geom_tile(ggplot2::aes(fill = fill_class))
    }) +
    ggplot2::labs(
      title = "Difference map",
      subtitle = map_subtitle(data),
      x = "Numeric measure",
      y = if (binned) "Source rows (original order)" else
        "Source row numbers (original order)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = if (nrow(data) && data$n_rows_shown[1L] > 40L) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_text()
      }
    )

  plot <- plot + if (binned) {
    ggplot2::scale_fill_gradientn(
      colours = c(daffiz_match_colour, daffiz_mixed_colour,
                  daffiz_mismatch_colour),
      limits = c(0, 1),
      labels = function(x) paste0(round(100 * x), "%"),
      name = "Cells that\ndiffer"
    )
  } else {
    ggplot2::scale_fill_manual(
      values = c(Match = daffiz_match_colour, Differs = daffiz_mismatch_colour),
      limits = c("Match", "Differs"),
      drop = FALSE,
      name = NULL
    )
  }

  # Redundant symbol encoding, so the four cell classes do not depend on fill
  # alone. Only worth drawing while the tiles are large enough to read.
  if (!binned && nrow(data) > 0L && nrow(data) <= 400L) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = symbol),
      colour = "white",
      size = 3,
      na.rm = TRUE
    )
  }
  plot
}

#' Plot method for a comparison
#'
#' Equivalent to building [plot_diff()] and printing it, so that the
#' conventional `plot(cmp)` call draws the difference map instead of falling
#' through to `plot.default()`.
#'
#' @param x A `daffiz_comparison` from [compare_dt()].
#' @param y Unused; present for compatibility with the [plot()] generic.
#'   Supplying it is an error rather than a silent no-op, because the sibling
#'   functions take `columns` in that position.
#' @param ... Passed to [plot_diff()].
#' @return The plot object, invisibly.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' y <- data.frame(id = 1:3, amount = c(10, 25, 27))
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(compare_dt(x, y))
#' }
plot.daffiz_comparison <- function(x, y, ...) {
  validate_comparison(x)
  # `y` belongs to the `plot()` generic and is unused here. Left unchecked it
  # silently swallows a positional second argument, so `plot(cmp, "amount")` --
  # the natural mirror of `plot_diff(cmp, "amount")` -- drew every measure and
  # reported nothing.
  if (!missing(y)) {
    daffiz_abort(
      "daffiz_error_plot_argument",
      paste0(
        "`plot()` ignores its second positional argument.\n",
        "  Name the argument instead, for example ",
        "`plot(cmp, columns = \"amount\")`."
      )
    )
  }
  out <- plot_diff(x, ...)
  print(out)
  invisible(out)
}
