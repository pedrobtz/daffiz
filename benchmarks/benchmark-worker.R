#!/usr/bin/env Rscript

# Runs one comparison in a fresh R process. The parent runner uses isolated
# processes so R's allocator high-water mark from one case cannot leak into the
# next case.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 7L) {
  stop(paste(
    "usage: benchmark-worker.R ROWS MEASURES DIFF_RATE DUPLICATES",
    "BATCH OUTPUT REPOSITORY"
  ))
}

n_rows <- as.integer(args[[1L]])
n_measures <- as.integer(args[[2L]])
diff_rate <- as.numeric(args[[3L]])
duplicates <- identical(args[[4L]], "duplicated")
batch <- if (identical(args[[5L]], "all")) NULL else as.integer(args[[5L]])
output <- args[[6L]]
repository <- normalizePath(args[[7L]], mustWork = TRUE)

if (is.na(n_rows) || is.na(n_measures) || is.na(diff_rate) ||
    n_rows < 1L || n_measures < 1L || diff_rate < 0 || diff_rate > 1) {
  stop("invalid benchmark dimensions")
}

suppressPackageStartupMessages(pkgload::load_all(repository, quiet = TRUE))

heap_high_water <- function() {
  g <- gc()
  # R's current vector heap uses 8-byte Vcells. Ncell size is platform
  # dependent; 56 bytes is reported by R on the 64-bit benchmark platforms.
  as.double(g["Vcells", "max used"]) * 8 +
    as.double(g["Ncells", "max used"]) * 56
}

base_value <- as.double(seq_len(n_rows))
id <- if (duplicates) rep_len(seq_len(ceiling(n_rows / 2)), n_rows) else
  seq_len(n_rows)
x <- data.table::data.table(id = id)
for (j in seq_len(n_measures)) {
  data.table::set(x, j = paste0("m", j), value = base_value + j / 1000)
}
y <- data.table::copy(x)

n_cells <- as.double(n_rows) * n_measures
n_changed <- min(n_cells, floor(n_cells * diff_rate))
if (n_changed > 0) {
  complete_measures <- floor(n_changed / n_rows)
  remainder <- n_changed %% n_rows
  if (complete_measures > 0) {
    for (j in seq_len(complete_measures)) {
      column <- paste0("m", j)
      data.table::set(y, j = column, value = y[[column]] + 1)
    }
  }
  if (remainder > 0) {
    column <- paste0("m", complete_measures + 1L)
    data.table::set(
      y,
      i = seq_len(remainder),
      j = column,
      value = y[[column]][seq_len(remainder)] + 1
    )
  }
}

gc(reset = TRUE)
construction <- system.time({
  cmp <- suppressWarnings(compare_dt(
    x,
    y,
    duplicate_keys = "pair",
    batch = batch,
    x_name = "x",
    y_name = "y"
  ))
})
construction_peak <- heap_high_water()

cells_bytes <- as.double(object.size(cmp$cells))
comparison_bytes <- as.double(object.size(cmp))

gc(reset = TRUE)
column_time <- system.time(column_summary(cmp))
column_peak <- heap_high_water()

gc(reset = TRUE)
row_time <- system.time(row_summary(cmp))
row_peak <- heap_high_water()

result <- data.frame(
  rows = n_rows,
  measures = n_measures,
  projected_cells = n_cells,
  diff_rate = diff_rate,
  key_shape = if (duplicates) "duplicated" else "unique",
  batch = if (is.null(batch)) "all" else as.character(batch),
  construction_elapsed_s = unname(construction[["elapsed"]]),
  column_summary_elapsed_s = unname(column_time[["elapsed"]]),
  row_summary_elapsed_s = unname(row_time[["elapsed"]]),
  cells_bytes = cells_bytes,
  comparison_bytes = comparison_bytes,
  construction_peak_r_heap_bytes = construction_peak,
  column_summary_peak_r_heap_bytes = column_peak,
  row_summary_peak_r_heap_bytes = row_peak,
  cells_bytes_per_cell = cells_bytes / n_cells,
  construction_peak_r_heap_bytes_per_cell = construction_peak / n_cells,
  stringsAsFactors = FALSE
)
utils::write.csv(result, output, row.names = FALSE)
