library(data.table)

source("fake.R")
source("compare.R")

# Every case here is two tiny tables, A and B, and one call to diff_table().
# A is always the baseline. The tables are small enough to hold in your head,
# so each expectation names the exact rows it is about.

RESULTS <- data.table(label = character(), ok = logical())

check <- function(label, ok) {
  ok <- isTRUE(ok)
  RESULTS <<- rbind(RESULTS, data.table(label = label, ok = ok))
  cat(if (ok) "ok    " else "FAIL  ", label, "\n", sep = "")
  invisible(ok)
}

check_error <- function(label, expr, pattern = NULL) {
  e <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )
  hit <- !is.null(e) && (is.null(pattern) || grepl(pattern, conditionMessage(e)))
  if (!is.null(e) && !hit) {
    cat("      (got: ", conditionMessage(e), ")\n", sep = "")
  }
  check(label, hit)
}

# `expr` must be TRUE and must warn; `pattern` pins which warning.
check_warns <- function(label, expr, pattern = NULL) {
  w <- NULL
  val <- withCallingHandlers(
    expr,
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  hit <- length(w) > 0L && (is.null(pattern) || any(grepl(pattern, w)))
  if (length(w) && !hit) {
    cat("      (got: ", w[1L], ")\n", sep = "")
  }
  check(label, hit && isTRUE(val))
}

# `expr` must be TRUE and must not warn at all.
check_clean <- function(label, expr) {
  w <- NULL
  val <- withCallingHandlers(
    expr,
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  if (length(w)) {
    cat("      (warned: ", w[1L], ")\n", sep = "")
  }
  check(label, is.null(w) && isTRUE(val))
}

section <- function(x) cat("\n--- ", x, " ---\n", sep = "")

# How many rows of each status came back. Absent statuses read as 0.
tally <- function(res) {
  want <- c("same", "changed", "only_x", "only_y")
  out <- setNames(integer(length(want)), want)
  got <- res[, .N, by = status]
  out[got$status] <- got$N
  out
}

is_tally <- function(res, same = 0L, changed = 0L, only_x = 0L, only_y = 0L) {
  identical(tally(res), c(same = same, changed = changed, only_x = only_x, only_y = only_y))
}

# A: 4 rows, 5 columns. VAL is the only numeric, so it is the measure and the
# other four columns are the ids.
A <- data.table(
  ID = c("a", "b", "c", "d"),
  GRP = c("x", "x", "y", "y"),
  DAY = as.Date("2024-01-01") + 0:3,
  CNT = 1:4,
  VAL = c(10, 20, 30, 40)
)


section("B is A")

check_clean("identical tables: every row same, and nothing warns",
  is_tally(diff_table(A, copy(A)), same = 4L))
check("identical tables: no diff", diff_table(A, copy(A))[, all(diff == 0)])

B_shuffled <- A[c(3, 1, 4, 2)]
check("row order does not matter", is_tally(diff_table(A, B_shuffled), same = 4L))


section("rows added and removed")

check("B is a subset: the missing row is only_x",
  is_tally(diff_table(A, A[1:3]), same = 3L, only_x = 1L))

B_extra <- rbind(A, data.table(ID = "e", GRP = "z", DAY = as.Date("2024-02-01"), CNT = 9L, VAL = 90))
check("B has an extra row: it is only_y", is_tally(diff_table(A, B_extra), same = 4L, only_y = 1L))

check("B shares no ids with A: everything is one-sided",
  is_tally(diff_table(A, copy(A)[, ID := paste0(ID, "!")]), only_x = 4L, only_y = 4L))

# Changing an id column is an identity change, not a value change.
B_id <- copy(A)[ID == "b", GRP := "q"]
check("a changed id splits into only_x + only_y",
  is_tally(diff_table(A, B_id), same = 3L, only_x = 1L, only_y = 1L))


section("values change")

B_val <- copy(A)[ID == "c", VAL := 31]
check("one changed value", is_tally(diff_table(A, B_val), same = 3L, changed = 1L))
check("the diff is reported", diff_table(A, B_val)[status == "changed", diff == 1])

B_all <- copy(A)[, VAL := VAL * 2]
check("every value changed", is_tally(diff_table(A, B_all), changed = 4L))

# One row dropped, one added, one edited, one untouched.
B_mix <- rbind(
  copy(A)[2:4][ID == "c", VAL := 99],
  data.table(ID = "e", GRP = "z", DAY = as.Date("2024-02-01"), CNT = 9L, VAL = 90)
)
check("added, removed, changed and untouched at once",
  is_tally(diff_table(A, B_mix), same = 2L, changed = 1L, only_x = 1L, only_y = 1L))


section("tolerance")

B_tiny <- copy(A)[ID == "a", VAL := VAL + 1e-9]
check("a tiny difference counts by default", is_tally(diff_table(A, B_tiny), same = 3L, changed = 1L))
check("tolerance absorbs it", is_tally(diff_table(A, B_tiny, tolerance = 1e-6), same = 4L))
check("tolerance is not a free pass", is_tally(diff_table(A, B_val, tolerance = 1e-6), same = 3L, changed = 1L))


section("modes agree")

check("in_place and new give the same answer",
  identical(tally(diff_table(A, B_mix)), tally(diff_table(A, B_mix, mode = "new"))))
check("in_place leaves A alone", {
  before <- copy(A)
  invisible(diff_table(A, B_mix))
  identical(before, A)
})
check("in_place leaves B alone", {
  before <- copy(B_mix)
  invisible(diff_table(A, B_mix))
  identical(before, B_mix)
})


section("types")

B_chr <- copy(A)[, VAL := as.character(VAL)]
check("a drifted type is cast back", is_tally(diff_table(A, B_chr), same = 4L))
check_error("without casting the type mismatch is refused",
  diff_table(A, B_chr, cast = FALSE), "disagree on the type")

B_int <- copy(A)[ID == "a", CNT := 99L]
check("integers are ids by default, so CNT is an identity",
  is_tally(diff_table(A, B_int), same = 3L, only_x = 1L, only_y = 1L))
# Melting an integer column alongside a double one puts both in one value
# column, so CNT widens to double. melt() says so, and it should keep saying so.
check_warns("measures = numeric+integer makes CNT a measure",
  is_tally(diff_table(A, B_int, measures = "numeric+integer"), same = 7L, changed = 1L),
  "not all of the same type")


section("columns")

B_wide <- copy(A)[, NOTE := "hi"]
check("an extra column in B does not stop the compare",
  is_tally(diff_table(A, B_wide), same = 4L))
check_error("a missing column in B is refused",
  diff_table(A, copy(A)[, GRP := NULL]), "not in both tables")


section("NA values")

A_na <- copy(A)[ID == "a", VAL := NA_real_]
B_na <- copy(A_na)
check("NA on both sides is not a change", is_tally(diff_table(A_na, B_na), same = 4L))
check("NA appearing in B is a change",
  is_tally(diff_table(A, B_na), same = 3L, changed = 1L))
check("NA disappearing in B is a change",
  is_tally(diff_table(A_na, A), same = 3L, changed = 1L))
check("a row missing from B is only_x even when its value is NA",
  is_tally(diff_table(A_na, A_na[2:4]), same = 3L, only_x = 1L))


section("ids that do not tell rows apart")

# Two rows identical in every id column, differing only in the measure.
A_dup <- data.table(
  ID = c("a", "a", "b"),
  GRP = c("x", "x", "y"),
  DAY = as.Date("2024-01-01"),
  CNT = c(1L, 1L, 2L),
  VAL = c(10, 11, 20)
)
B_dup <- copy(A_dup)

check("duplicate ids: disambiguated by arrival order", is_tally(diff_table(A_dup, B_dup), same = 3L))
check("duplicate ids: a change inside the group is found",
  is_tally(diff_table(A_dup, copy(A_dup)[3, VAL := 21]), same = 2L, changed = 1L))
check_error("duplicates = error refuses", diff_table(A_dup, B_dup, duplicates = "error"),
  "do not tell rows apart")
check("duplicates = aggregate collapses the group", {
  res <- diff_table(A_dup, B_dup, duplicates = "aggregate")
  nrow(res) == 2L && all(res$status == "same")
})
check("duplicates = aggregate sees a change in the group", {
  res <- diff_table(A_dup, copy(A_dup)[2, VAL := 12], duplicates = "aggregate")
  is_tally(res, same = 1L, changed = 1L)
})


section("summary")

cat(sprintf("\n%d checks, %d failed\n", nrow(RESULTS), RESULTS[ok == FALSE, .N]))
if (RESULTS[ok == FALSE, .N]) {
  print(RESULTS[ok == FALSE, .(label)])
}
