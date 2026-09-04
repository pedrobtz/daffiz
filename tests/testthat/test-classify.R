test_that("identical tables match", {
  fx <- make_fixture(30L)
  cmp <- compare_dt(fx, copy(fx))
  expect_true(is_matching(cmp))
  expect_true(all(all_cells(cmp)$.match_type == "equal"))
})

test_that("one changed cell is isolated", {
  x <- data.frame(id = 1:3, a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(id = 1:3, a = c(1, 2, 3), b = c(4, 5, 6.5))
  cells <- all_cells(compare_dt(x, y))
  diffs <- cells[cells$.match_type == "diff", ]
  expect_equal(nrow(diffs), 1L)
  expect_equal(diffs$.metric, "b")
  expect_equal(diffs$id, 3L)
})

test_that("several measures changing in one row are all reported", {
  x <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4), c = c(5, 6))
  y <- data.frame(id = 1:2, a = c(1, 9), b = c(3, 9), c = c(5, 9))
  cells <- all_cells(compare_dt(x, y))
  diffs <- cells[cells$.match_type == "diff", ]
  expect_equal(nrow(diffs), 3L)
  expect_true(all(diffs$id == 2L))
})

test_that("one measure changing across many rows is reported per row", {
  n <- 20L
  x <- data.frame(id = seq_len(n), a = rep(1, n), b = rep(2, n))
  y <- data.frame(id = seq_len(n), a = rep(1, n), b = rep(3, n))
  cells <- all_cells(compare_dt(x, y))
  diffs <- cells[cells$.match_type == "diff", ]
  expect_equal(nrow(diffs), n)
  expect_true(all(diffs$.metric == "b"))
})

test_that("side-only rows are classified and keep NA on the absent side", {
  x <- data.frame(id = c(1L, 2L), v = c(1, 2))
  y <- data.frame(id = c(2L, 3L), v = c(2, 3))
  cells <- all_cells(compare_dt(x, y))
  expect_equal(as.character(cells$.match_type[cells$id == 1L]), "x_only")
  expect_equal(as.character(cells$.match_type[cells$id == 3L]), "y_only")
  expect_true(is.na(cells$.row_y[cells$id == 1L]))
  expect_true(is.na(cells$.row_x[cells$id == 3L]))
})

test_that("an absent row is distinguished from a present NA value", {
  x <- data.frame(id = c(1L, 2L), v = c(NA_real_, 2))
  y <- data.frame(id = c(1L, 3L), v = c(5, 3))
  cells <- all_cells(compare_dt(x, y))
  # id 1 is present on both sides, with NA on one -> diff, not x_only.
  expect_equal(as.character(cells$.match_type[cells$id == 1L]), "diff")
  expect_equal(as.character(cells$.match_type[cells$id == 2L]), "x_only")
})

test_that("missing-value semantics follow section 6.2", {
  expect_equal(classify_one(NA_real_, NA_real_), "equal")
  expect_equal(classify_one(NaN, NaN), "equal")
  expect_equal(classify_one(NA_real_, NaN), "equal")
  expect_equal(classify_one(NaN, NA_real_), "equal")
  expect_equal(classify_one(NA_real_, 1), "diff")
  expect_equal(classify_one(1, NA_real_), "diff")
})

test_that("infinities follow section 6.2", {
  expect_equal(classify_one(Inf, Inf), "equal")
  expect_equal(classify_one(-Inf, -Inf), "equal")
  expect_equal(classify_one(Inf, -Inf), "diff")
  expect_equal(classify_one(Inf, 1), "diff")
  expect_equal(classify_one(1, -Inf), "diff")
})

test_that("infinite values do not match merely because tolerance is huge", {
  expect_equal(classify_one(Inf, 1, abs_tol = 1e300), "diff")
})

test_that("absolute tolerance boundary is inclusive", {
  expect_equal(classify_one(1, 1.5, abs_tol = 0.5), "equal")
  expect_equal(classify_one(1, 1.5, abs_tol = 0.4), "diff")
  expect_equal(classify_one(1, 1.5, abs_tol = 0.6), "equal")
})

test_that("relative tolerance uses the symmetric denominator", {
  # |100 - 110| = 10; max(|x|,|y|) = 110; 10/110 = 0.0909...
  expect_equal(classify_one(100, 110, rel_tol = 0.09), "diff")
  expect_equal(classify_one(100, 110, rel_tol = 0.10), "equal")
  # Symmetric: reversing the arguments gives the same verdict.
  expect_equal(classify_one(110, 100, rel_tol = 0.09), "diff")
  expect_equal(classify_one(110, 100, rel_tol = 0.10), "equal")
})

test_that("absolute and relative tolerance combine with max()", {
  # For (0, 0.5): abs_diff = 0.5 and max(|x|,|y|) = 0.5, so the relative
  # allowance is rel_tol * 0.5 and the effective tolerance is the larger of it
  # and abs_tol.
  expect_equal(classify_one(0, 0.5, abs_tol = 1.0, rel_tol = 0), "equal")
  expect_equal(classify_one(0, 0.5, abs_tol = 0, rel_tol = 1.0), "equal")
  expect_equal(classify_one(0, 0.5, abs_tol = 0, rel_tol = 0.5), "diff")

  # Whichever allowance is larger wins: abs alone fails, rel alone passes.
  expect_equal(classify_one(0, 0.5, abs_tol = 0.4, rel_tol = 0), "diff")
  expect_equal(classify_one(0, 0.5, abs_tol = 0.4, rel_tol = 1.0), "equal")
})

test_that(".rel_diff is 0 for two zeros and NA when undefined", {
  cells <- all_cells(compare_dt(simple_pair(0, 0)$x, simple_pair(0, 0)$y))
  expect_equal(cells$.rel_diff, 0)

  p <- simple_pair(NA_real_, 1)
  expect_true(is.na(all_cells(compare_dt(p$x, p$y))$.rel_diff))

  p <- simple_pair(Inf, Inf)
  expect_true(is.na(all_cells(compare_dt(p$x, p$y))$.rel_diff))
})

test_that(".match_type has stable factor levels", {
  cmp <- compare_dt(simple_pair(1, 1)$x, simple_pair(1, 1)$y)
  expect_equal(levels(all_cells(cmp)$.match_type),
               c("equal", "diff", "x_only", "y_only"))
})

test_that("reversing the inputs negates diffs without changing equality", {
  fx <- make_fixture(30L)
  fy <- copy(fx)
  fy[c(3L, 9L), amount := amount + 1]
  fy[5L, ratio := ratio + 0.5]

  a <- all_cells(compare_dt(fx, fy, by = "code", abs_tol = 0.1))
  b <- all_cells(compare_dt(fy, fx, by = "code", abs_tol = 0.1))
  setorderv(a, c("code", ".metric"))
  setorderv(b, c("code", ".metric"))

  expect_equal(as.character(a$.match_type), as.character(b$.match_type))
  finite <- is.finite(a$.diff) & is.finite(b$.diff)
  expect_equal(a$.diff[finite], -b$.diff[finite])
  expect_equal(a$.abs_diff, b$.abs_diff)
  expect_equal(a$.row_x, b$.row_y)
})

test_that("the cell table has the documented column order", {
  cmp <- compare_dt(simple_pair(1, 1)$x, simple_pair(1, 1)$y)
  expect_equal(names(all_cells(cmp)),
               c("id", ".row_x", ".row_y", ".metric", ".value_x", ".value_y",
                 ".diff", ".abs_diff", ".rel_diff", ".match_type"))
})

test_that("accessors return copies", {
  cmp <- compare_dt(simple_pair(1, 2)$x, simple_pair(1, 2)$y)
  cells <- all_cells(cmp)
  cells[, .value_x := 999]
  expect_false(any(all_cells(cmp)$.value_x == 999))
})
