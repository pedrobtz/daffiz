# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

d <- fake_dt(4, n_cols = c(character = 1, integer = 1, numeric = 2), seed = 1)

long <- melt_dt(d)

test_that("melt_dt", {
  check("melt_dt: rows x measured columns", nrow(long) == nrow(d) * 2L)
  check("melt_dt: only the doubles are measured by default", setequal(unique(long$metric), c("NUM_01", "NUM_02")))
  check("melt_dt: the measure names are character, not factor", is.character(long$metric))
  check("melt_dt: row_id comes first", names(long)[1L] == "row_id")
  check("melt_dt: row_id points back at the source row", all(long[metric == "NUM_01", value] == d$NUM_01[long[metric == "NUM_01", row_id]]))
  check("melt_dt: row_id is kept out of the key", !"row_id" %in% key(long))
  check("melt_dt: keyed by the real id columns", setequal(key(long), c("CHR_01", "INT_01")))
  check("melt_dt: the source table is not touched", !"row_id" %in% names(d))
})

both <- melt_dt(d, measures = "numeric+integer")

test_that("melt_dt", {
  check("melt_dt: numeric+integer measures the integers too", setequal(unique(both$metric), c("NUM_01", "NUM_02", "INT_01")))
  check("melt_dt: the value column widens to double", is.double(both$value))
  check("melt_dt: an integer measure leaves the key", !"INT_01" %in% key(both))
})

named <- melt_dt(d, variable.name = "which", value.name = "amount")

test_that("melt_dt", {
  check("melt_dt: the measure and value columns can be renamed", all(c("which", "amount") %in% names(named)))
})

test_that("melt_dt", {
  check("melt_dt: an upper-case ROW_ID is left alone as an ordinary id column", {
    upper <- melt_dt(copy(d)[, ROW_ID := 99L])
    all(upper$ROW_ID == 99L) && "ROW_ID" %in% key(upper) &&
      setequal(upper$row_id, seq_len(nrow(d)))
  })
  check("melt_dt: zero rows still melts", nrow(melt_dt(base[0])) == 0L)
  check("melt_dt: no numeric column leaves one row per row", nrow(melt_dt(data.table(a = c("x", "y")))) == 2L)
  check("melt_dt: an integer column is not numeric enough by default", !"metric" %in% names(melt_dt(data.table(a = "x", b = 1L))))
  check_error("melt_dt: an unknown measures option", melt_dt(d, measures = "chars"), "not a column: chars")
  check_error("melt_dt: refuses a non-table", melt_dt(1:3))
})

test_that("melt_dt melts integers without melt()'s coercion warning", {
  expect_no_warning(out <- melt_dt(d, measures = "numeric+integer"))
  expect_type(out$value, "double")
})

test_that("measures can be named", {
  out <- melt_dt(d, measures = c("NUM_02", "INT_01"))
  expect_setequal(unique(out$metric), c("NUM_02", "INT_01"))
  # The double that was not named is an id now.
  expect_true("NUM_01" %in% key(out))
})

test_that("a named measure must be numeric or integer, named once, and exist", {
  expect_error(select_measures(d, "CHR_01"), "not numeric or integer: CHR_01 \\(character\\)", class = "daffiz_error_measures")
  expect_error(select_measures(d, c("NUM_01", "NUM_01")), "named more than once: NUM_01")
  expect_error(select_measures(d, "NOPE"), "not a column: NOPE")
  expect_error(select_measures(d, character()), class = "daffiz_error_measures")
  expect_error(select_measures(d, NA_character_), class = "daffiz_error_measures")
})

test_that("index_dt numbers and keys rows on its own copy", {
  src <- data.table(ID = c("b", "a"), V = c(2L, 1L), OTHER = "o")
  wide <- index_dt(src, ids = "ID", measures = "V")
  expect_identical(names(wide), c("row_id", "ID", "V"))
  expect_identical(key(wide), "ID")
  expect_identical(wide$row_id, c(2L, 1L))
  expect_type(wide$V, "double")
  # Keying reordered rows; the caller's table keeps its order and types.
  expect_identical(src$ID, c("b", "a"))
  expect_identical(src$V, c(2L, 1L))
})

test_that("index_dt can map NaN to NA in double id columns", {
  src <- data.table(K = c(NaN, 1), V = c(1, 2))
  expect_true(any(is.nan(index_dt(src, "K", "V")$K)))
  wide <- index_dt(src, "K", "V", nan_to_na = TRUE)
  expect_false(any(is.nan(wide$K)))
  expect_true(is.nan(src$K[1L]))
})

test_that("an indexed table melts as it is, keeping key_seq in the key", {
  wide <- index_dt(data.table(ID = c("a", "a"), V = c(1, 2)), "ID", "V")
  disambiguate_by_key(wide, along = character())
  long <- melt_dt(wide, measures = "V")
  expect_identical(key(long), c("ID", "key_seq"))
  expect_identical(long$row_id, c(1L, 2L))
})
