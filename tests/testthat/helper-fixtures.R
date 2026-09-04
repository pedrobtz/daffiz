# Deterministic fixtures in base R. Replaces the draft's `charlatan`-based
# generator, which pulled a data-generation package into the runtime
# dependencies (design plan section 13).

rand_code <- function(n, len = 6L, alphabet = c(LETTERS, 0:9)) {
  vapply(seq_len(n), function(i)
    paste0(sample(alphabet, len, replace = TRUE), collapse = ""),
    character(1))
}

# A wide table exercising every type-dispatch path: character, factor, Date,
# POSIXct, logical, integer and double.
make_fixture <- function(n = 50L, seed = 1L) {
  set.seed(seed)
  data.table::data.table(
    code       = rand_code(n),
    category   = sample(c("A", "B", "C"), n, replace = TRUE),
    grade      = factor(sample(c("low", "high"), n, replace = TRUE),
                        levels = c("low", "high")),
    event_date = as.Date("2020-01-01") + sample.int(2000L, n, replace = TRUE),
    seen_at    = as.POSIXct("2020-01-01", tz = "UTC") + sample.int(1e6L, n, replace = TRUE),
    is_active  = sample(c(TRUE, FALSE), n, replace = TRUE),
    age        = sample(18L:80L, n, replace = TRUE),
    amount     = round(runif(n, 0, 1000), 2),
    score      = round(runif(n, 0, 100), 3),
    ratio      = runif(n)
  )
}

# Two-column numeric fixture with a unique key, for focused semantics tests.
simple_pair <- function(vx, vy, id = seq_along(vx)) {
  list(
    x = data.frame(id = id, v = as.double(vx)),
    y = data.frame(id = id, v = as.double(vy))
  )
}

# Classifies a single (vx, vy) pair, returning the `.match_type` label.
classify_one <- function(vx, vy, ...) {
  p <- simple_pair(vx, vy, id = 1L)
  cmp <- compare_dt(p$x, p$y, ...)
  as.character(all_cells(cmp)$.match_type)
}

# Phase 2 fixture with one x-only row, one equal row, one changed row, and one
# y-only row across two measures.
mixed_comparison <- function() {
  x <- data.frame(
    id = 1:3,
    a = c(1, 2, 3),
    b = c(10, 20, 30)
  )
  y <- data.frame(
    id = 2:4,
    a = c(2, 4, 4),
    b = c(20, 30, 40)
  )
  list(x = x, y = y, cmp = compare_dt(x, y))
}
