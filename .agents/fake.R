library(data.table)

# The default column layout: how many columns of each type, in this order.
FAKE_COLS <- c(character = 10L, date = 2L, integer = 3L, numeric = 5L)

# Column names and generator types for a layout, in layout order.
fake_layout <- function(n_cols = FAKE_COLS) {
  if (is.null(names(n_cols))) {
    stop("`n_cols` must be named, e.g. c(character = 2, numeric = 1)", call. = FALSE)
  }
  unknown <- setdiff(names(n_cols), names(FAKE_COLS))
  if (length(unknown)) {
    stop("unknown column type(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (any(n_cols < 0L) || anyNA(n_cols)) {
    stop("`n_cols` counts must be >= 0", call. = FALSE)
  }

  # Missing types mean none of that type; the order is always FAKE_COLS's.
  counts <- FAKE_COLS
  counts[] <- 0L
  counts[names(n_cols)] <- as.integer(n_cols)
  if (sum(counts) == 0L) {
    stop("`n_cols` asks for no columns at all", call. = FALSE)
  }

  gen <- c(character = "chr", date = "date", integer = "int", numeric = "num")
  tag <- c(character = "CHR", date = "DATE", integer = "INT", numeric = "NUM")

  parts <- lapply(names(counts)[counts > 0L], function(ty) {
    list(
      type = rep(gen[[ty]], counts[[ty]]),
      name = sprintf("%s_%02d", tag[[ty]], seq_len(counts[[ty]]))
    )
  })

  list(
    counts = counts,
    type = unlist(lapply(parts, `[[`, "type"), use.names = FALSE),
    name = unlist(lapply(parts, `[[`, "name"), use.names = FALSE)
  )
}

# Default cardinality per column, for whatever layout is asked for.
# The last few character columns scale with the row count, so they stay
# high-cardinality as the table grows; Inf leaves the numeric columns
# unconstrained (one value per row).
default_distint <- function(n, n_cols = FAKE_COLS) {
  counts <- fake_layout(n_cols)$counts
  pct <- function(p) max(1, round(n * p))

  n_chr <- counts[["character"]]
  chr <- if (n_chr == 0L) {
    numeric()
  } else {
    scaled <- c(0.05, 0.25, 0.50)
    scaled <- utils::tail(scaled, min(3L, n_chr))
    c(rep(3, max(0L, n_chr - length(scaled))), vapply(scaled, pct, numeric(1L)))
  }

  c(
    chr,
    rep_len(c(2, 5), counts[["date"]]),
    rep_len(c(4, 1, 10), counts[["integer"]]),
    rep(Inf, counts[["numeric"]])
  )
}

# Build a data.table of fake data. `n_cols` sets how many columns of each type,
# named for the classes they produce, e.g. c(character = 2, integer = 1,
# date = 1, numeric = 2). Types left out give no columns; the column order is
# always character, date, integer, numeric.
#
# n_distinct: how many different values each column can take, in column order.
#   Either one value for every column, or one per column. Inf or NA means
#   unconstrained: every row can get its own value.
#   Defaults to default_distint(n, n_cols); NULL is the same as all Inf.
fake_dt <- function(n = 100L, n_cols = FAKE_COLS,
                    n_distinct = default_distint(n, n_cols), seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  layout <- fake_layout(n_cols)
  types <- layout$type
  nms <- layout$name

  if (is.null(n_distinct)) {
    n_distinct <- Inf
  }
  if (length(n_distinct) == 1L) {
    n_distinct <- rep(n_distinct, length(types))
  }
  if (length(n_distinct) != length(types)) {
    stop("`n_distinct` must have length 1 or ", length(types), call. = FALSE)
  }
  # Inf/NA: no cap, so the pool is as large as the number of rows.
  unlimited <- is.na(n_distinct) | is.infinite(n_distinct)
  n_distinct[unlimited] <- n
  if (any(n_distinct < 1L)) {
    stop("`n_distinct` must be >= 1, or Inf/NA for no limit", call. = FALSE)
  }

  # Prefixes stay distinct past the 26th character column.
  prefix <- function(i) {
    if (i <= 26L) LETTERS[i] else paste0(LETTERS[((i - 1L) %% 26L) + 1L], (i - 1L) %/% 26L)
  }

  # Pool of `k` distinct values for a column, then n draws from that pool.
  pool <- function(type, k, i) {
    switch(type,
      chr = sprintf("%s_%04d", prefix(i), seq_len(k)),
      # The 10-year window holds at most 3650 distinct days.
      date = as.Date("2020-01-01") + sample.int(3650L, min(k, 3650L)),
      int = sample.int(.Machine$integer.max %/% 2L, k),
      num = rnorm(k)
    )
  }

  # Index the pool rather than sample() it: sample(x, ...) on a length-1
  # numeric would draw from seq_len(x) instead of from the pool itself.
  cols <- Map(
    function(type, k, i) {
      p <- pool(type, k, i)
      # A pool that already has one value per row is used as is, so an
      # uncapped column is simply rnorm(n) & friends: n distinct values.
      if (length(p) == n) p else p[sample.int(length(p), n, replace = TRUE)]
    },
    types, as.integer(n_distinct), seq_along(types)
  )
  names(cols) <- nms

  as.data.table(cols)
}

# Fresh values of the same class as `x`, none of them present in `x`.
novel_values <- function(x, m) {
  reject <- function(v, draw) {
    tries <- 0L
    while (any(bad <- v %in% x) && tries < 10L) {
      v[bad] <- draw(sum(bad))
      tries <- tries + 1L
    }
    v
  }

  if (inherits(x, "Date")) {
    cand <- as.Date("2020-01-01") + seq_len(3650L)
    cand <- cand[!cand %in% x]
    if (!length(cand)) {
      stop("no unused dates left in the window", call. = FALSE)
    }
    cand[sample.int(length(cand), m, replace = TRUE)]
  } else if (is.character(x)) {
    # Keep the column's "<prefix>_<digits>" shape when it has one, so a new
    # value still looks like the data it replaces.
    parts <- regmatches(x[1L], regexec("^(.*)_([0-9]+)$", x[1L]))[[1L]]
    if (length(parts) == 3L && !is.na(x[1L])) {
      used <- suppressWarnings(as.integer(sub("^.*_", "", x)))
      from <- max(c(0L, used), na.rm = TRUE)
      sprintf("%s_%0*d", parts[2L], nchar(parts[3L]), from + sample.int(10L * m, m))
    } else {
      draw <- function(k) sprintf("FUZZ_%d", sample.int(.Machine$integer.max %/% 2L, k))
      reject(draw(m), draw)
    }
  } else if (is.integer(x)) {
    draw <- function(k) sample.int(.Machine$integer.max %/% 2L, k)
    reject(draw(m), draw)
  } else if (is.numeric(x)) {
    reject(rnorm(m), rnorm)
  } else {
    stop("cannot fuzz a column of class ", class(x)[1L], call. = FALSE)
  }
}

# `m` values for fuzzed cells of column `x`. `new_ratio` is the share of those
# cells that get a value the column has never held; the rest are drawn from the
# values it already has. The brand-new values come from a pool no larger than
# the column's own distinct count, so fuzzing a 3-value column adds a few new
# categories rather than hundreds of one-off ones.
fuzz_values <- function(x, m, new_ratio = 0.5) {
  u <- unique(x)
  # A non-zero ratio always yields at least one new value: round() alone would
  # turn a single fuzzed cell into round(0.5) == 0 and change nothing.
  n_new <- if (new_ratio > 0) max(1L, round(new_ratio * m)) else 0L

  old <- if (m - n_new > 0L) u[sample.int(length(u), m - n_new, replace = TRUE)] else u[0L]
  new <- if (n_new > 0L) {
    pool <- novel_values(x, min(n_new, max(1L, length(u))))
    pool[sample.int(length(pool), n_new, replace = TRUE)]
  } else {
    u[0L]
  }

  v <- c(old, new)
  v[sample.int(m)] # shuffle, so the new values are not all at the end
}

# Fuzz a fake_dt() table with two independent knobs:
#   pct_replace: share of the existing rows whose values are overwritten.
#                The same rows are hit in every column of `cols`.
#   pct_new:     extra rows appended, as a share of the original row count.
#   new_ratio:   share of the fuzzed cells that get a value the column has
#                never held. 0 only reshuffles existing values (overlap stays
#                at 100%), 1 makes every fuzzed cell a new value.
# Returns a copy; `dt` is left untouched.
fuzz_dt <- function(dt, pct_replace = 0.1, pct_new = 0, cols = names(dt),
                    new_ratio = 0.5, seed = NULL) {
  stopifnot(
    is.data.frame(dt),
    length(pct_replace) == 1L, pct_replace >= 0, pct_replace <= 1,
    length(pct_new) == 1L, pct_new >= 0,
    length(new_ratio) == 1L, new_ratio >= 0, new_ratio <= 1
  )
  if (!is.null(seed)) {
    set.seed(seed)
  }

  unknown <- setdiff(cols, names(dt))
  if (length(unknown)) {
    stop("unknown column(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  draw <- function(v, m) fuzz_values(v, m, new_ratio)

  out <- copy(as.data.table(dt))
  n <- nrow(out)

  n_replace <- if (pct_replace > 0) max(1L, round(pct_replace * n)) else 0L
  if (n_replace > 0L && length(cols)) {
    rows <- sample.int(n, n_replace)
    for (nm in cols) {
      set(out, i = rows, j = nm, value = draw(out[[nm]], n_replace))
    }
  }

  # Appended rows are fuzzed in `cols` and sampled from the baseline elsewhere,
  # so a new row is complete whichever columns were selected.
  n_new <- if (pct_new > 0) max(1L, round(pct_new * n)) else 0L
  if (n_new > 0L && n > 0L) {
    extra <- lapply(names(out), function(nm) {
      v <- out[[nm]]
      if (nm %in% cols) draw(v, n_new) else v[sample.int(length(v), n_new, replace = TRUE)]
    })
    names(extra) <- names(out)
    out <- rbind(out, as.data.table(extra))
  }

  out
}
