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

keep <- copy(d)[, row_id := 99L]

test_that("melt_dt", {
  check("melt_dt: row_id is ours, so an existing one is overwritten", setequal(melt_dt(keep)$row_id, seq_len(nrow(d))))
  check("melt_dt: an upper-case ROW_ID is left alone as an ordinary id column", {
    upper <- melt_dt(copy(d)[, ROW_ID := 99L])
    all(upper$ROW_ID == 99L) && "ROW_ID" %in% key(upper) &&
      setequal(upper$row_id, seq_len(nrow(d)))
  })
  check("melt_dt: zero rows still melts", nrow(melt_dt(base[0])) == 0L)
  check_error("melt_dt: no numeric column to measure", melt_dt(data.table(a = "x")), "no columns of type")
  check_error("melt_dt: an integer column is not numeric enough by default", melt_dt(data.table(a = "x", b = 1L)), "no columns of type")
  check_error("melt_dt: an unknown measures option", melt_dt(d, measures = "chars"))
  check_error("melt_dt: refuses a non-table", melt_dt(1:3))
})
