# Batched construction --------------------------------------------------------
#
# The canonical table has roughly n_rows x n_measures records. Batching over
# measures reduces the size of the two melted intermediates and their join.
# Every classified cell is still retained, so batching is transparent to the
# comparison object and every Phase 2 accessor.

resolve_batch <- function(batch, n_measures) {
  if (is.null(batch)) return(n_measures)
  # `is.numeric()` is already FALSE for complex, and `is.finite()` is already
  # FALSE for NA, so neither needs its own test.
  if (!is.numeric(batch) || is.object(batch) || length(batch) != 1L ||
      !is.finite(batch) || batch < 1 || batch != trunc(batch)) {
    daffiz_abort("daffiz_error_batch",
      "`batch` must be a single positive whole number, or NULL for one batch.",
      batch = batch)
  }
  as.integer(min(batch, n_measures))
}

# Melt, join and classify one subset of measures.
build_cells <- function(wx, wy, by, occ, measures, abs_res, rel_res) {
  mx <- melt(wx, id.vars = c(by, occ, ".row_x"), measure.vars = measures,
             variable.name = ".metric", value.name = ".value_x",
             variable.factor = FALSE)
  my <- melt(wy, id.vars = c(by, occ, ".row_y"), measure.vars = measures,
             variable.name = ".metric", value.name = ".value_y",
             variable.factor = FALSE)
  mx[, .in_x := TRUE]
  my[, .in_y := TRUE]

  # `.row_x`/`.row_y` travel as data, not as join keys, so rows at different
  # original positions still join.
  cells <- merge(mx, my, by = c(by, occ, ".metric"), all = TRUE)
  classify_cells(cells, abs_res, rel_res)
}

# Construct all cells in deterministic measure batches.
build_cells_batched <- function(
    wx, wy, by, occ, measures, abs_res, rel_res, batch_size) {
  if (batch_size >= length(measures)) {
    return(build_cells(wx, wy, by, occ, measures, abs_res, rel_res))
  }

  batch_id <- ceiling(seq_along(measures) / batch_size)
  measure_batches <- split(measures, batch_id)
  parts <- lapply(
    measure_batches,
    function(batch_measures) {
      build_cells(
        wx, wy, by, occ, batch_measures, abs_res, rel_res
      )
    }
  )
  rbindlist(parts, use.names = TRUE)
}
