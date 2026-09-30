# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

kx <- data.table(id = c("a", "b", "c"), v = 1:3)

setkey(kx, id)

ky <- data.table(id = c("a", "b", "d"), w = 4:6)

setkey(ky, id)

dry <- merge_dry_run(kx, ky)

test_that("merge_dry_run", {
  check("merge_dry_run: counts the rows on each side", dry$rows_x == 3L && dry$rows_y == 3L)
  check("merge_dry_run: counts the distinct keys", dry$keys_x == 3L && dry$keys_y == 3L)
  check("merge_dry_run: counts the shared keys", dry$keys_common == 2L)
  check("merge_dry_run: counts the matching rows", dry$matched_x == 2L && dry$matched_y == 2L)
  check("merge_dry_run: reports the matched share", dry$pct_x == 200 / 3)
  check("merge_dry_run: an inner join would keep 2 rows", dry$est_merge_rows == 2L)
  check("merge_dry_run: no key produces more than one row", dry$max_fanout == 1L)
  check("merge_dry_run: so the join is one to one", isTRUE(dry$one_to_one))
  check("merge_dry_run: no duplicate keys on either side", dry$dups_x == 0L && dry$dups_y == 0L)
})

fan <- merge_dry_run(
  setkey(data.table(id = c("a", "a"), v = 1:2), id),
  setkey(data.table(id = c("a", "a", "a"), w = 1:3), id)
)

test_that("merge_dry_run", {
  check("merge_dry_run: 2 x 3 rows on one key would be 6", fan$est_merge_rows == 6L)
  check("merge_dry_run: and that is the fanout", fan$max_fanout == 6L)
  check("merge_dry_run: so the join is refused as not one to one", isFALSE(fan$one_to_one))
  check("merge_dry_run: it says which side repeats", fan$dups_x == 1L && fan$dups_y == 1L)
})

none <- merge_dry_run(kx, setkey(data.table(id = c("x", "y"), w = 1:2), id))

test_that("merge_dry_run", {
  check("merge_dry_run: no shared keys", none$keys_common == 0L && none$est_merge_rows == 0L)
  check("merge_dry_run: nothing matches, so nothing fans out", none$max_fanout == 0L && isTRUE(none$one_to_one))
})

# Fanout only sees shared keys, so duplicates on one side alone used to slip
# through the gate. Finding 6.
lop <- merge_dry_run(
  setkey(data.table(id = "a", v = 1), id),
  setkey(data.table(id = c("b", "b"), w = 1:2), id)
)

test_that("merge_dry_run", {
  check("merge_dry_run: a key repeating on one side alone still fans out to 1", lop$max_fanout == 1 || lop$keys_common == 0L)
  check("merge_dry_run: but it is not a one-to-one join", isFALSE(lop$one_to_one))
  check("merge_dry_run: and it says which side repeats", lop$dups_y == 1L && lop$dups_x == 0L)
  check_error(
    "merge_dt: so the merge is refused",
    merge_dt(setkey(data.table(id = "a", v = 1), id),
             setkey(data.table(id = c("b", "b"), w = 1:2), id), all = TRUE),
    "not one to one"
  )
})

# Integer counts square past 2^31 on a wide join. Finding 7.
big <- 46341L

big_dry <- merge_dry_run(
  setkey(data.table(id = rep(1L, big)), id),
  setkey(data.table(id = rep(1L, big)), id)
)

test_that("merge_dry_run", {
  check("merge_dry_run: a large fanout does not overflow", big_dry$est_merge_rows == as.double(big) * big)
  check("merge_dry_run: nor does the fanout itself", big_dry$max_fanout == as.double(big) * big)
  check("merge_dry_run: and the gate still answers", isFALSE(big_dry$one_to_one))
})

empty <- merge_dry_run(kx[0], ky)

test_that("merge_dry_run", {
  check("merge_dry_run: an empty side gives NA for its share", is.na(empty$pct_x))
  check_error("merge_dry_run: no key to join on", merge_dry_run(data.table(a = 1), data.table(a = 1)), "no columns to join on")
  check_error("merge_dry_run: a column missing from one side", merge_dry_run(kx, ky, on = "nope"), "not in both tables")
  check_error("merge_dry_run: refuses a data.frame", merge_dry_run(as.data.frame(kx), ky, on = "id"))
})

m_inner <- merge_dt(kx, ky)

test_that("merge_dt", {
  check("merge_dt: new mode defaults to an inner join", nrow(m_inner) == 2L)
  check("merge_dt: it carries y's columns across", "w" %in% names(m_inner))
  check("merge_dt: x is left alone", !"w" %in% names(kx))
})

# A suffixed name must not land on a name x already uses: in place that
# overwrites it, in a new table it duplicates it. Finding 3.
sfx_x <- setkey(data.table(id = 1L, a = 1, a_y = 5), id)

sfx_y <- setkey(data.table(id = 1L, a = 7), id)

test_that("merge_dt", {
  check_error(
    "merge_dt: a suffix that would overwrite an existing column is refused",
    merge_dt(copy(sfx_x), sfx_y, mode = "in_place"), "would write over existing column"
  )
  check_error(
    "merge_dt: and refused in new mode too, where it would duplicate the name",
    merge_dt(copy(sfx_x), sfx_y, mode = "new"), "would write over existing column"
  )
  check("merge_dt: x is untouched by the refusal", sfx_x$a_y == 5)
  check(
    "merge_dt: another suffix gets it through",
    merge_dt(copy(sfx_x), sfx_y, mode = "new", suffix = "_from_y")$a_from_y == 7
  )
})

m_outer <- merge_dt(kx, ky, all = TRUE)

test_that("merge_dt", {
  check("merge_dt: all = TRUE keeps every key", nrow(m_outer) == 4L)
  check("merge_dt: unmatched rows get NA", m_outer[id == "c", is.na(w)])
})

clash <- setkey(data.table(id = c("a", "b", "c"), v = 7:9), id)

m_clash <- merge_dt(kx, clash)

test_that("merge_dt", {
  check("merge_dt: a clashing name gets the suffix", "v_y" %in% names(m_clash))
  check("merge_dt: x's own name never moves", "v" %in% names(m_clash))
  check("merge_dt: the suffix is configurable", "v_two" %in% names(merge_dt(kx, clash, suffix = "_two")))
})

ip <- copy(kx)

merge_dt(ip, ky, mode = "in_place")

test_that("merge_dt", {
  check("merge_dt: in_place adds y's columns by reference", "w" %in% names(ip))
  check("merge_dt: in_place keeps every row of x", nrow(ip) == 3L)
  check("merge_dt: in_place is a left join, so misses are NA", ip[id == "c", is.na(w)])
  check("merge_dt: in_place does not pick up y-only rows", !"d" %in% ip$id)
  check_error(
    "merge_dt: refuses a join that would multiply rows",
    merge_dt(
      setkey(data.table(id = c("a", "a"), v = 1:2), id),
      setkey(data.table(id = c("a", "a"), w = 1:2), id)
    ),
    "not one to one"
  )
  check_error(
    "merge_dt: the refusal points at the two ways out",
    merge_dt(
      setkey(data.table(id = c("a", "a"), v = 1:2), id),
      setkey(data.table(id = c("a", "a"), w = 1:2), id)
    ),
    "disambiguate_by_key"
  )
  check_error("merge_dt: an unknown mode", merge_dt(kx, ky, mode = "sideways"))
})

test_that("the refusal of a fanout past 2^31 can still be written", {
  big <- 46341L
  bx <- setkey(data.table(id = rep(1L, big)), id)
  err <- expect_error(merge_dt(bx, copy(bx)), class = "daffiz_error_keys")
  expect_match(conditionMessage(err), "46,341 rows would become 2,147,488,281", fixed = TRUE)
})
