#!/usr/bin/env Rscript

# Reproducible benchmark matrix for the canonical long-table workload.
#
# Profiles:
#   validation (default): representative cases up to 5 million cells
#   full: the design-plan matrix (potentially 100 million cells per case)
#
# Safety cap:
#   DAFFIZ_BENCH_MAX_CELLS defaults to 5e6. Cases above it are recorded as
#   skipped. Raise it deliberately only on a machine sized for the final table.

args <- commandArgs(trailingOnly = TRUE)
repository <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else
  normalizePath(".", mustWork = TRUE)
profile <- Sys.getenv("DAFFIZ_BENCH_PROFILE", "validation")
max_cells <- as.numeric(Sys.getenv("DAFFIZ_BENCH_MAX_CELLS", "5e6"))
output <- Sys.getenv(
  "DAFFIZ_BENCH_OUTPUT",
  file.path(repository, "benchmarks", "results", "benchmark-latest.csv")
)

if (!profile %in% c("validation", "full")) {
  stop("DAFFIZ_BENCH_PROFILE must be 'validation' or 'full'")
}
if (!is.finite(max_cells) || max_cells < 1) {
  stop("DAFFIZ_BENCH_MAX_CELLS must be a positive finite number")
}

if (profile == "validation") {
  cases <- data.frame(
    rows = c(1000L, 10000L, 50000L, 50000L, 50000L,
             100000L, 100000L, 100000L, 100000L),
    measures = c(5L, 20L, 20L, 20L, 20L, 20L, 20L, 50L, 50L),
    diff_rate = c(0, .01, 0, .2, .01, .001, .001, .01, .01),
    key_shape = c("unique", "unique", "unique", "unique", "duplicated",
                  "unique", "unique", "unique", "unique"),
    batch = c("all", "all", "all", "all", "all", "all", "5", "all", "5"),
    stringsAsFactors = FALSE
  )
} else {
  cases <- expand.grid(
    rows = c(1e3, 1e5, 1e6),
    measures = c(5L, 20L, 100L),
    diff_rate = c(0, .001, .01, .2),
    key_shape = c("unique", "duplicated"),
    stringsAsFactors = FALSE
  )
  cases$batch <- "all"
}
cases$projected_cells <- cases$rows * cases$measures
cases$status <- ifelse(cases$projected_cells <= max_cells, "pending", "skipped_cap")

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
worker <- file.path(repository, "benchmarks", "benchmark-worker.R")
rscript <- file.path(R.home("bin"), "Rscript")
results <- list()

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  if (case$status != "pending") next
  message(sprintf(
    "[%d/%d] %s rows x %s measures, rate=%g, keys=%s, batch=%s",
    i, nrow(cases), format(case$rows, scientific = FALSE), case$measures,
    case$diff_rate, case$key_shape, case$batch
  ))
  case_output <- tempfile(fileext = ".csv")
  log <- tempfile(fileext = ".log")
  status <- system2(
    rscript,
    c(
      worker,
      as.character(case$rows),
      as.character(case$measures),
      as.character(case$diff_rate),
      case$key_shape,
      case$batch,
      case_output,
      repository
    ),
    stdout = log,
    stderr = log
  )
  if (status == 0L && file.exists(case_output)) {
    results[[length(results) + 1L]] <- utils::read.csv(
      case_output,
      stringsAsFactors = FALSE
    )
    cases$status[i] <- "completed"
  } else {
    cases$status[i] <- "failed"
    warning(paste(readLines(log, warn = FALSE), collapse = "\n"))
  }
  unlink(c(case_output, log))
}

completed <- if (length(results)) do.call(rbind, results) else data.frame()
skipped <- cases[cases$status != "completed", c(
  "rows", "measures", "projected_cells", "diff_rate", "key_shape", "batch",
  "status"
)]
if (nrow(completed)) completed$status <- "completed"
all_results <- if (nrow(completed) && nrow(skipped)) {
  missing <- setdiff(names(completed), names(skipped))
  for (nm in missing) skipped[[nm]] <- NA
  skipped <- skipped[, names(completed)]
  rbind(completed, skipped)
} else if (nrow(completed)) {
  completed
} else {
  skipped
}

utils::write.csv(all_results, output, row.names = FALSE, na = "")
message("Wrote ", output)
