library(data.table)

source("fake.R")
source("compare.R")

# A check is a label and something that must be TRUE. Every case below states
# what it expects, so running this file either prints all ok or names what
# broke -- nothing here relies on eyeballing printed output.

RESULTS <- data.table(label = character(), ok = logical())

check <- function(label, ok) {
  ok <- isTRUE(ok)
  RESULTS <<- rbind(RESULTS, data.table(label = label, ok = ok))
  cat(if (ok) "ok    " else "FAIL  ", label, "\n", sep = "")
  invisible(ok)
}

# `expr` stays unevaluated until it is forced inside tryCatch().
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

check_clean <- function(label, expr) {
  w <- NULL
  val <- withCallingHandlers(
    expr,
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  check(label, is.null(w) && isTRUE(val))
}

section <- function(x) cat("\n--- ", x, " ---\n", sep = "")

# The baseline table the original cases used.
d1 <- fake_dt(
  n = 3,
  n_cols = c(character = 2, date = 1, integer = 1, numeric = 2),
  seed = 42
)

# Hand-built tables where every row is identifiable, so the expectations below
# are about the code and not about what fake_dt() happened to draw.
base <- data.table(id = c("a", "b", "c"), v = c(1, 2, 3))


section("summarize_dt")

s1 <- summarize_dt(d1)

check("summarize_dt: one row per column", nrow(s1) == ncol(d1))
check("summarize_dt: reports names in order", identical(s1$colname, names(d1)))
check(
  "summarize_dt: reports classes",
  identical(s1$type, c("character", "character", "Date", "integer", "numeric", "numeric"))
)
check(
  "summarize_dt: counts distinct values",
  summarize_dt(data.table(x = c(1, 1, 2)))$n_unique == 2L
)
check(
  "summarize_dt: NA counts as a value",
  summarize_dt(data.table(x = c(1, NA)))$n_unique == 2L
)
check("summarize_dt: zero rows", all(summarize_dt(base[0]) $n_unique == 0L))
check_error("summarize_dt: refuses a non-table", summarize_dt(1:3))


section("compare_dt")

cmp_same <- compare_dt(d1, d1)

check("compare_dt: identical tables match on type", all(cmp_same$type_match))
check("compare_dt: identical tables overlap fully", all(cmp_same$over_pct == 100))
check("compare_dt: identical tables agree on counts", all(cmp_same$n_unique_x == cmp_same$n_unique_y))

y_extra <- copy(base)[, w := 10]
cmp_extra <- compare_dt(base, y_extra)

check("compare_dt: keeps x's column order, y-only columns last", identical(cmp_extra$colname, c("id", "v", "w")))
check("compare_dt: a y-only column has no x side", is.na(cmp_extra[colname == "w", type_x]))
check("compare_dt: a y-only column has no overlap", is.na(cmp_extra[colname == "w", over_pct]))

cmp_missing <- compare_dt(y_extra, base)
check("compare_dt: an x-only column has no y side", is.na(cmp_missing[colname == "w", type_y]))

cmp_disjoint <- compare_dt(base, data.table(id = c("x", "y", "z"), v = c(7, 8, 9)))
check("compare_dt: disjoint values give 0 overlap", all(cmp_disjoint$over_pct == 0))

cmp_half <- compare_dt(base, data.table(id = c("a", "b", "z"), v = c(1, 2, 3)))
check("compare_dt: partial overlap, 2 of 4 shared ids", cmp_half[colname == "id", n_common == 2L & n_union == 4L])
check("compare_dt: partial overlap gives 50 pct", cmp_half[colname == "id", over_pct == 50])

cmp_type <- compare_dt(base, data.table(id = c("a", "b", "c"), v = c("1", "2", "3")))
check("compare_dt: a changed type is flagged", isFALSE(cmp_type[colname == "v", type_match]))
# `%in%` coerces before matching, so measuring the overlap across a type change
# would report a full overlap for two columns that are not the same thing. It
# is left uncomputed instead.
check("compare_dt: a changed type leaves the overlap uncomputed", cmp_type[colname == "v", is.na(over_pct)])
check("compare_dt: and the counts behind it too", cmp_type[colname == "v", is.na(n_common) & is.na(n_union)])
check("compare_dt: the column that kept its type is still measured", cmp_type[colname == "id", over_pct == 100])
check(
  "compare_dt: the overlap is measured exactly where the types agree",
  cmp_type[!is.na(type_match), all(type_match == !is.na(n_union))]
)
check(
  "compare_dt: a POSIXct on both sides still counts as one type",
  !is.na(compare_dt(
    data.table(t = as.POSIXct("2024-01-01", tz = "UTC")),
    data.table(t = as.POSIXct("2024-01-01", tz = "UTC"))
  )[colname == "t", over_pct])
)
check(
  "compare_dt: a factor against its own labels is not measured",
  is.na(compare_dt(data.table(f = factor("a")), data.table(f = "a"))[colname == "f", over_pct])
)

check("compare_dt: two empty columns give NA overlap", is.na(compare_dt(base[0], base[0])[colname == "id", over_pct]))
check_error("compare_dt: refuses a non-table", compare_dt(base, 1:3))


section("col_type")

check("col_type: character", col_type("a") == "character")
check("col_type: logical", col_type(TRUE) == "logical")
check("col_type: integer", col_type(1L) == "integer")
check("col_type: numeric", col_type(1.5) == "numeric")
check("col_type: Date before its storage mode", col_type(Sys.Date()) == "Date")
check("col_type: POSIXct before its storage mode", col_type(Sys.time()) == "POSIXct")
check("col_type: factor is itself, not integer", col_type(factor("a")) == "factor")
check("col_type: anything else falls back to its class", col_type(complex(real = 1)) == "complex")


section("cast rules table")

check("CAST_RULES: covers every from x to pair but the identities", nrow(CAST_RULES) == length(CAST_FROM) * length(CAST_TYPES) - length(CAST_TYPES))
check("CAST_RULES: no self-conversions", all(CAST_RULES$from != CAST_RULES$to))
check("CAST_RULES: every rule carries a note", all(nzchar(CAST_RULES$note)))
check("cast_rule: finds a pair", isTRUE(cast_rule("integer", "numeric")$allowed))
check("cast_rule: finds a refusal", isFALSE(cast_rule("factor", "numeric")$allowed))
check("cast_rule: an unknown pair is NA, not an error", is.na(cast_rule("integer", "complex")$allowed))
check("cast_rule: an identity pair has no rule", is.na(cast_rule("integer", "integer")$allowed))
check("cast_rules(wide = FALSE): the long table", identical(dim(cast_rules(FALSE)), dim(CAST_RULES)))
check("cast_rules(FALSE): a copy, not the original", !identical(address(cast_rules(FALSE)), address(CAST_RULES)))

w <- cast_rules()
check("cast_rules(): one row per source type", nrow(w) == length(CAST_FROM))
check("cast_rules(): one column per target type, plus from", identical(names(w), c("from", CAST_TYPES)))
check("cast_rules(): the diagonal reads '='", w[from == "integer", integer] == "=")
check("cast_rules(): an allowed pair reads 'yes'", w[from == "integer", numeric] == "yes")
check("cast_rules(): a refused pair reads '-'", w[from == "integer", logical] == "-")


section("cast_value: every pair in the rules table")

# One representative value per source type, chosen so an allowed conversion
# has something it can actually convert.
cast_sample <- function(from, to) {
  switch(from,
    character = switch(to,
      logical = "TRUE",
      Date = "2024-03-05",
      integer = "3",
      numeric = "1.5",
      "abc"
    ),
    logical = TRUE,
    Date = as.Date("2024-03-05"),
    numeric = 3.5,
    integer = 3L,
    POSIXct = as.POSIXct("2024-03-05 23:30:00", tz = "UTC"),
    factor = factor("abc")
  )
}

allowed_ok <- CAST_RULES[allowed == TRUE][, .(
  ok = {
    v <- cast_sample(from, to)
    out <- tryCatch(cast_value(v, to, "col"), error = function(e) e)
    !inherits(out, "error") && col_type(out) == to && !is.na(out)
  }
), by = .(from, to)]

check(
  paste0("cast_value: all ", nrow(allowed_ok), " allowed pairs convert"),
  all(allowed_ok$ok)
)
if (!all(allowed_ok$ok)) print(allowed_ok[ok == FALSE])

refused_ok <- CAST_RULES[allowed == FALSE][, .(
  ok = inherits(
    tryCatch(cast_value(cast_sample(from, to), to, "col"), error = function(e) e),
    "error"
  )
), by = .(from, to)]

check(
  paste0("cast_value: all ", nrow(refused_ok), " refused pairs error"),
  all(refused_ok$ok)
)
if (!all(refused_ok$ok)) print(refused_ok[ok == FALSE])


section("cast_value: the details of each conversion")

check("cast_value: same type is returned untouched", identical(cast_value(3L, "integer", "col"), 3L))
check("cast_value: text to number", cast_value("1.5", "numeric", "col") == 1.5)
check("cast_value: text to integer truncates toward zero", identical(cast_value("3.7", "integer", "col"), 3L))
check("cast_value: negative truncation is toward zero too", identical(cast_value("-3.7", "integer", "col"), -3L))
check("cast_value: text to date", cast_value("2024-03-05", "Date", "col") == as.Date("2024-03-05"))
check("cast_value: text to logical, long form", identical(cast_value(c("TRUE", "FALSE"), "logical", "col"), c(TRUE, FALSE)))
check("cast_value: text to logical, short form", identical(cast_value(c("T", "F"), "logical", "col"), c(TRUE, FALSE)))
check("cast_value: text to logical, lower case", identical(cast_value(c("true", "false"), "logical", "col"), c(TRUE, FALSE)))
check("cast_value: logical to integer", identical(cast_value(c(TRUE, FALSE), "integer", "col"), c(1L, 0L)))
check("cast_value: date to integer is days since the epoch", cast_value(as.Date("1970-01-11"), "integer", "col") == 10L)
check("cast_value: number to date is days since the epoch", cast_value(10, "Date", "col") == as.Date("1970-01-11"))
check("cast_value: factor becomes its labels, never its codes", identical(cast_value(factor(c("7", "9")), "character", "col"), c("7", "9")))
check("cast_value: NA survives the trip", is.na(cast_value(NA_character_, "numeric", "col")))
check(
  "cast_value: POSIXct to Date uses the stamp's own zone",
  cast_value(as.POSIXct("2024-01-01 23:30:00", tz = "Europe/Lisbon"), "Date", "col") == as.Date("2024-01-01")
)
check(
  "cast_value: POSIXct to integer is seconds since the epoch",
  cast_value(as.POSIXct("1970-01-01 00:01:00", tz = "UTC"), "integer", "col") == 60L
)

check_error("cast_value: text that is not a number", cast_value(c("1", "abc"), "numeric", "col"), "not a number")
check_error("cast_value: text that is not a date", cast_value("05/03/2024", "Date", "col"), "not a date in format")

# as.Date() ignores whatever trails the format, so parsing alone is not enough:
# the parsed date has to format back to exactly the text it came from.
check_error(
  "cast_value: text with trailing junk is not a date",
  cast_value("2024-01-01junk", "Date", "col"), "not a canonical"
)
check_error(
  "cast_value: a two-digit year is not silently the year 24",
  cast_value("24-01-01", "Date", "col"), "not a canonical"
)
check_error(
  "cast_value: an unpadded date is not canonical",
  cast_value("2024-1-1", "Date", "col"), "not a canonical"
)
check_error(
  "cast_value: surrounding whitespace is not canonical",
  cast_value(" 2024-01-01", "Date", "col"), "not a canonical"
)
# as.character() gives 15 significant digits, so doubles that differ further
# out collapse onto one key. Finding 2, numeric half.
check_error(
  "cast_value: a double that loses precision as text is refused",
  cast_value(1 + .Machine$double.eps, "character", "col"), "loses precision as text"
)
check("cast_value: the message prints the value at full precision", {
  msg <- tryCatch(cast_value(1 + .Machine$double.eps, "character", "col"), error = conditionMessage)
  grepl("1.0000000000000002", msg, fixed = TRUE)
})
check("cast_value: an ordinary double still converts", cast_value(1.5, "character", "col") == "1.5")
check("cast_value: non-finite doubles round-trip as text", {
  all(cast_value(c(Inf, -Inf), "character", "col") == c("Inf", "-Inf"))
})
check_error(
  "diff_table: a double id that collides with a character id is refused",
  diff_table(data.table(ID = "1", V = 5), data.table(ID = 1 + .Machine$double.eps, V = 5)),
  "loses precision as text"
)

check("cast_value: a canonical date still converts", cast_value("2024-01-01", "Date", "col") == as.Date("2024-01-01"))
check("cast_value: NA stays NA through the round-trip check", is.na(cast_value(NA_character_, "Date", "col")))

# date_format drives both directions, so they stay inverse to one another.
check(
  "cast_value: date_format parses a non-ISO date",
  cast_value("05/03/2024", "Date", "col", date_format = "%d/%m/%Y") == as.Date("2024-03-05")
)
check_error(
  "cast_value: date_format still refuses trailing junk",
  cast_value("05/03/2024junk", "Date", "col", date_format = "%d/%m/%Y"), "not a canonical"
)
check_error(
  "cast_value: an ISO date is refused under a non-ISO date_format",
  cast_value("2024-03-05", "Date", "col", date_format = "%d/%m/%Y"), "not a date in format"
)
check(
  "cast_value: a Date goes back out through date_format",
  cast_value(as.Date("2024-03-05"), "character", "col", date_format = "%d/%m/%Y") == "05/03/2024"
)
check(
  "cast_value: text -> Date -> text round-trips under date_format",
  cast_value(cast_value("05/03/2024", "Date", "col", date_format = "%d/%m/%Y"),
             "character", "col", date_format = "%d/%m/%Y") == "05/03/2024"
)
check_error("cast_value: date_format must be one string", cast_value("2024-01-01", "Date", "col", date_format = c("%Y", "%m")), "single format string")

# The parameter has to survive the trip from diff_table() down to cast_value().
check_error(
  "diff_table: trailing junk in a date key is refused",
  diff_table(data.table(D = as.Date("2024-01-01"), V = 1),
             data.table(D = "2024-01-01junk", V = 1)),
  "not a canonical"
)
check(
  "diff_table: date_format reaches the cast",
  diff_table(data.table(D = as.Date("2024-03-05"), V = 1),
             data.table(D = "05/03/2024", V = 1),
             date_format = "%d/%m/%Y")$status == "same"
)
check_error("cast_value: text that is not a logical", cast_value(c("TRUE", "maybe"), "logical", "col"), "not a logical literal")
check_error("cast_value: 0/1 is a number, not a flag", cast_value(c(0, 1), "logical", "col"), "no sound conversion")
check_error("cast_value: a factor is not parsed implicitly", cast_value(factor("1"), "numeric", "col"), "as.character")
check_error("cast_value: out of integer range", cast_value("3000000000", "integer", "col"), "outside integer range")
check_error("cast_value: an unsupported source type", cast_value(complex(real = 1), "numeric", "col"), "unsupported source type")
check_error("cast_value: an unsupported target type", cast_value("2024-03-05", "POSIXct", "col"), "no rule for this pair")
check_error("cast_value: the message names the column", cast_value("abc", "numeric", "THE_COL"), "column THE_COL")
check_error("cast_value: the message names up to three offenders", cast_value(c("a", "b", "c", "d"), "numeric", "col"), "a, b, c$")
check_error(
  "cast_value: the message names the type the caller passed, not the hop",
  cast_value(factor(c("a", "b")), "numeric", "col"),
  "factor -> numeric"
)


section("cast_dt")

x_ref <- data.table(id = 1L, score = 1.5, day = as.Date("2024-01-01"), flag = TRUE)
y_cand <- data.table(id = "2", score = "3.5", day = as.POSIXct("2024-01-02 23:30:00", tz = "UTC"), flag = "FALSE")

cast_dt(x_ref, y_cand)
check("cast_dt: casts every shared column to x's type", identical(summarize_dt(y_cand)$type, summarize_dt(x_ref)$type))
check("cast_dt: converts the values, not just the types", y_cand$id == 2L && y_cand$score == 3.5)

y_partial <- data.table(id = "2", extra = "left alone")
cast_dt(x_ref, y_partial)
check("cast_dt: a y-only column is left alone", is.character(y_partial$extra))

y_cols <- data.table(id = "2", score = "3.5")
cast_dt(x_ref, y_cols, cols = "id")
check("cast_dt: `cols` limits what is cast", is.integer(y_cols$id) && is.character(y_cols$score))

y_atomic <- data.table(id = "2", score = "not a number")
check_error("cast_dt: refuses the whole table on one bad column", cast_dt(x_ref, y_atomic), "not a number")
check("cast_dt: a refusal leaves y exactly as it was", identical(y_atomic, data.table(id = "2", score = "not a number")))

check("cast_dt: returns y invisibly", identical(address(cast_dt(x_ref, y_partial)), address(y_partial)))
check_error("cast_dt: an unknown column", cast_dt(x_ref, copy(y_cand), cols = "nope"), "missing from x or y")
check_error("cast_dt: y must be a data.table, not a data.frame", cast_dt(x_ref, data.frame(id = "2")))
check_error(
  "cast_dt: x with a column of unsupported type",
  cast_dt(data.table(id = complex(real = 1)), data.table(id = "2")),
  "unsupported type"
)


section("normalize_dt")

check("NORMALIZE_TO: covers every source type that is not a target type", setequal(names(NORMALIZE_TO), setdiff(CAST_FROM, CAST_TYPES)))
check("NORMALIZE_TO: every target is a baseline type", all(NORMALIZE_TO %in% CAST_TYPES))

stamps <- data.table(t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"), v = c(1, 2))
b1 <- normalize_dt(stamps)
check("normalize_dt: POSIXct becomes a Date by default", col_type(b1$t) == "Date")
check("normalize_dt: on the day the stamp prints", b1$t[1L] == as.Date("2024-01-01"))
check("normalize_dt: a baseline column is untouched", identical(b1$v, stamps$v))
check("normalize_dt: the input is not modified", col_type(stamps$t) == "POSIXct")

check(
  "normalize_dt: the day is taken in the stamp's own zone, not UTC",
  normalize_dt(data.table(t = as.POSIXct("2024-01-01 23:30:00", tz = "Europe/Lisbon")))$t == as.Date("2024-01-01")
)
check(
  "normalize_dt: dropping the time can merge two stamps into one day",
  uniqueN(normalize_dt(data.table(t = as.POSIXct(c("2024-01-01 01:00", "2024-01-01 23:00"), tz = "UTC")))$t) == 1L
)
check(
  "normalize_dt: `to` overrides that, keeping the full stamp",
  col_type(normalize_dt(stamps, to = c(POSIXct = "character"))$t) == "character"
)
check("normalize_dt: a factor becomes its labels", identical(normalize_dt(data.table(f = factor(c("7", "9"))))$f, c("7", "9")))
check("normalize_dt: `cols` limits what is converted", col_type(normalize_dt(stamps, cols = "v")$t) == "POSIXct")

clean <- data.table(id = "a", v = 1)
check("normalize_dt: a table already in baseline types comes back as is", identical(normalize_dt(clean), clean))
check("normalize_dt: zero rows", nrow(normalize_dt(stamps[0])) == 0L)

check_error("normalize_dt: an unmapped type is refused, not guessed", normalize_dt(data.table(z = complex(real = 1))), "no conversion given")
check_error("normalize_dt: the refusal names the column and its type", normalize_dt(data.table(z = complex(real = 1))), "z \\(complex\\)")
check_error("normalize_dt: an empty `to` refuses everything unconvertible", normalize_dt(stamps, to = character()), "no conversion given")
check_error("normalize_dt: a target with no cast rule", normalize_dt(stamps, to = c(POSIXct = "logical")), "no sound conversion")
check_error("normalize_dt: an unknown column", normalize_dt(stamps, cols = "nope"), "unknown column")
check_error("normalize_dt: refuses a non-table", normalize_dt(1:3))


section("melt_dt")

d <- fake_dt(4, n_cols = c(character = 1, integer = 1, numeric = 2), seed = 1)
long <- melt_dt(d)

check("melt_dt: rows x measured columns", nrow(long) == nrow(d) * 2L)
check("melt_dt: only the doubles are measured by default", setequal(unique(long$metric), c("NUM_01", "NUM_02")))
check("melt_dt: the measure names are character, not factor", is.character(long$metric))
check("melt_dt: row_id comes first", names(long)[1L] == "row_id")
check("melt_dt: row_id points back at the source row", all(long[metric == "NUM_01", value] == d$NUM_01[long[metric == "NUM_01", row_id]]))
check("melt_dt: row_id is kept out of the key", !"row_id" %in% key(long))
check("melt_dt: keyed by the real id columns", setequal(key(long), c("CHR_01", "INT_01")))
check("melt_dt: the source table is not touched", !"row_id" %in% names(d))

both <- melt_dt(d, measures = "numeric+integer")
check("melt_dt: numeric+integer measures the integers too", setequal(unique(both$metric), c("NUM_01", "NUM_02", "INT_01")))
check("melt_dt: the value column widens to double", is.double(both$value))
check("melt_dt: an integer measure leaves the key", !"INT_01" %in% key(both))

named <- melt_dt(d, variable.name = "which", value.name = "amount")
check("melt_dt: the measure and value columns can be renamed", all(c("which", "amount") %in% names(named)))

keep <- copy(d)[, row_id := 99L]
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


section("unique_by_key")

check("unique_by_key: distinct ids", isTRUE(unique_by_key(melt_dt(base))))
check("unique_by_key: no duplicates to report", nrow(attr(unique_by_key(melt_dt(base)), "duplicates")) == 0L)

dup <- data.table(id = c("a", "a", "b"), v = c(1, 2, 3))
u_dup <- unique_by_key(melt_dt(dup))
check("unique_by_key: repeated ids", isFALSE(u_dup))
check("unique_by_key: names the offending group", attr(u_dup, "duplicates")$id == "a")
check("unique_by_key: counts the rows in it", attr(u_dup, "duplicates")$n_rows == 2L)
check(
  "unique_by_key: the measure is part of the identity, so one row per metric is fine",
  isTRUE(unique_by_key(melt_dt(data.table(id = "a", v = 1, w = 2))))
)
check_error("unique_by_key: an unkeyed table", unique_by_key(data.table(a = 1)), "no key")
check_error("unique_by_key: refuses a data.frame", unique_by_key(data.frame(a = 1)))


section("disambiguate_by_key")

lx <- melt_dt(dup)
disambiguate_by_key(lx, "rowid")
check("disambiguate_by_key: adds the sequence column", "KEY_SEQ" %in% names(lx))
check("disambiguate_by_key: numbers within the group", setequal(lx[id == "a", KEY_SEQ], c(1L, 2L)))
check("disambiguate_by_key: a single-row group gets 1", all(lx[id == "b", KEY_SEQ] == 1L))
check("disambiguate_by_key: the key now includes it", identical(key(lx), c("id", "KEY_SEQ")))
check("disambiguate_by_key: the table is now unique by key", isTRUE(unique_by_key(lx)))
check("disambiguate_by_key: modifies by reference", "KEY_SEQ" %in% names(lx))

rank_dt <- melt_dt(data.table(id = c("a", "a", "a"), v = c(30, 10, 20)))
disambiguate_by_key(rank_dt, "value_rank")
check("disambiguate_by_key: value_rank numbers by the value", identical(rank_dt[order(value), KEY_SEQ], c(1L, 2L, 3L)))

ties <- melt_dt(data.table(id = c("a", "a"), v = c(5, 5)))
disambiguate_by_key(ties, "value_rank")
check("disambiguate_by_key: value_rank breaks ties by arrival", setequal(ties$KEY_SEQ, c(1L, 2L)))

renamed <- melt_dt(dup)
disambiguate_by_key(renamed, "rowid", seq.name = "SEQ")
check("disambiguate_by_key: the sequence column can be renamed", "SEQ" %in% names(renamed))

check_error("disambiguate_by_key: an unkeyed table", disambiguate_by_key(data.table(a = 1)), "no key")
check_error("disambiguate_by_key: an unknown method", disambiguate_by_key(melt_dt(dup), "guess"))


section("aggregate_by_key")

agg <- aggregate_by_key(melt_dt(dup))
check("aggregate_by_key: one row per key group and measure", nrow(agg) == 2L)
check("aggregate_by_key: counts the rows in the group", agg[id == "a", n_rows] == 2L)
check("aggregate_by_key: sums the values", agg[id == "a", value_sum] == 3)
check("aggregate_by_key: sums the squares", agg[id == "a", value_sumsq] == 5)
check("aggregate_by_key: reports the range", agg[id == "a", value_min == 1 & value_max == 2])
check("aggregate_by_key: keyed by the group", identical(key(agg), "id"))

m13 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(1, 3))))
m22 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(2, 2))))
check("aggregate_by_key: the sum alone cannot separate {1,3} from {2,2}", m13$value_sum == m22$value_sum)
check("aggregate_by_key: the sum of squares can", m13$value_sumsq != m22$value_sumsq)

na_grp <- melt_dt(data.table(id = c("a", "a"), v = c(1, NA_real_)))
agg_na <- aggregate_by_key(na_grp)
check("aggregate_by_key: NAs are counted, not dropped", agg_na$n_na == 1L && agg_na$n_rows == 2L)
check("aggregate_by_key: the other values are still summarised", agg_na$value_sum == 1)

check_clean(
  "aggregate_by_key: an all-NA group reports NA, without warning",
  is.na(aggregate_by_key(melt_dt(data.table(id = "a", v = NA_real_)))$value_min)
)

ex <- aggregate_by_key(melt_dt(dup), stats = "exact")
check("aggregate_by_key: exact keeps a value key instead of moments", "value_key" %in% names(ex) && !"value_sum" %in% names(ex))

ex13 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(1, 3))), stats = "exact")
ex31 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(3, 1))), stats = "exact")
ex22 <- aggregate_by_key(melt_dt(data.table(id = c("a", "a"), v = c(2, 2))), stats = "exact")
check("aggregate_by_key: exact is order-free", ex13$value_key == ex31$value_key)
check("aggregate_by_key: exact separates different multisets", ex13$value_key != ex22$value_key)
check(
  "aggregate_by_key: exact round-trips the bits, so 0.1+0.2 differs from 0.3",
  aggregate_by_key(melt_dt(data.table(id = "a", v = 0.1 + 0.2)), stats = "exact")$value_key !=
    aggregate_by_key(melt_dt(data.table(id = "a", v = 0.3)), stats = "exact")$value_key
)

check_error("aggregate_by_key: an unkeyed table", aggregate_by_key(data.table(a = 1)), "no key")
check_error("aggregate_by_key: an unknown stats option", aggregate_by_key(melt_dt(dup), stats = "mean"))


section("merge_dry_run")

kx <- data.table(id = c("a", "b", "c"), v = 1:3)
setkey(kx, id)
ky <- data.table(id = c("a", "b", "d"), w = 4:6)
setkey(ky, id)

dry <- merge_dry_run(kx, ky)
check("merge_dry_run: counts the rows on each side", dry$rows_x == 3L && dry$rows_y == 3L)
check("merge_dry_run: counts the distinct keys", dry$keys_x == 3L && dry$keys_y == 3L)
check("merge_dry_run: counts the shared keys", dry$keys_common == 2L)
check("merge_dry_run: counts the matching rows", dry$matched_x == 2L && dry$matched_y == 2L)
check("merge_dry_run: reports the matched share", dry$pct_x == 200 / 3)
check("merge_dry_run: an inner join would keep 2 rows", dry$est_merge_rows == 2L)
check("merge_dry_run: no key produces more than one row", dry$max_fanout == 1L)
check("merge_dry_run: so the join is one to one", isTRUE(dry$one_to_one))
check("merge_dry_run: no duplicate keys on either side", dry$dups_x == 0L && dry$dups_y == 0L)

fan <- merge_dry_run(
  setkey(data.table(id = c("a", "a"), v = 1:2), id),
  setkey(data.table(id = c("a", "a", "a"), w = 1:3), id)
)
check("merge_dry_run: 2 x 3 rows on one key would be 6", fan$est_merge_rows == 6L)
check("merge_dry_run: and that is the fanout", fan$max_fanout == 6L)
check("merge_dry_run: so the join is refused as not one to one", isFALSE(fan$one_to_one))
check("merge_dry_run: it says which side repeats", fan$dups_x == 1L && fan$dups_y == 1L)

none <- merge_dry_run(kx, setkey(data.table(id = c("x", "y"), w = 1:2), id))
check("merge_dry_run: no shared keys", none$keys_common == 0L && none$est_merge_rows == 0L)
check("merge_dry_run: nothing matches, so nothing fans out", none$max_fanout == 0L && isTRUE(none$one_to_one))

# Fanout only sees shared keys, so duplicates on one side alone used to slip
# through the gate. Finding 6.
lop <- merge_dry_run(
  setkey(data.table(id = "a", v = 1), id),
  setkey(data.table(id = c("b", "b"), w = 1:2), id)
)
check("merge_dry_run: a key repeating on one side alone still fans out to 1", lop$max_fanout == 1 || lop$keys_common == 0L)
check("merge_dry_run: but it is not a one-to-one join", isFALSE(lop$one_to_one))
check("merge_dry_run: and it says which side repeats", lop$dups_y == 1L && lop$dups_x == 0L)
check_error(
  "merge_dt: so the merge is refused",
  merge_dt(setkey(data.table(id = "a", v = 1), id),
           setkey(data.table(id = c("b", "b"), w = 1:2), id), all = TRUE),
  "not one to one"
)

# Integer counts square past 2^31 on a wide join. Finding 7.
big <- 46341L
big_dry <- merge_dry_run(
  setkey(data.table(id = rep(1L, big)), id),
  setkey(data.table(id = rep(1L, big)), id)
)
check("merge_dry_run: a large fanout does not overflow", big_dry$est_merge_rows == as.double(big) * big)
check("merge_dry_run: nor does the fanout itself", big_dry$max_fanout == as.double(big) * big)
check("merge_dry_run: and the gate still answers", isFALSE(big_dry$one_to_one))

empty <- merge_dry_run(kx[0], ky)
check("merge_dry_run: an empty side gives NA for its share", is.na(empty$pct_x))

check_error("merge_dry_run: no key to join on", merge_dry_run(data.table(a = 1), data.table(a = 1)), "no columns to join on")
check_error("merge_dry_run: a column missing from one side", merge_dry_run(kx, ky, on = "nope"), "not in both tables")
check_error("merge_dry_run: refuses a data.frame", merge_dry_run(as.data.frame(kx), ky, on = "id"))


section("merge_dt")

m_inner <- merge_dt(kx, ky)
check("merge_dt: new mode defaults to an inner join", nrow(m_inner) == 2L)
check("merge_dt: it carries y's columns across", "w" %in% names(m_inner))
check("merge_dt: x is left alone", !"w" %in% names(kx))

# A suffixed name must not land on a name x already uses: in place that
# overwrites it, in a new table it duplicates it. Finding 3.
sfx_x <- setkey(data.table(id = 1L, a = 1, a_y = 5), id)
sfx_y <- setkey(data.table(id = 1L, a = 7), id)
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

m_outer <- merge_dt(kx, ky, all = TRUE)
check("merge_dt: all = TRUE keeps every key", nrow(m_outer) == 4L)
check("merge_dt: unmatched rows get NA", m_outer[id == "c", is.na(w)])

clash <- setkey(data.table(id = c("a", "b", "c"), v = 7:9), id)
m_clash <- merge_dt(kx, clash)
check("merge_dt: a clashing name gets the suffix", "v_y" %in% names(m_clash))
check("merge_dt: x's own name never moves", "v" %in% names(m_clash))
check("merge_dt: the suffix is configurable", "v_two" %in% names(merge_dt(kx, clash, suffix = "_two")))

ip <- copy(kx)
merge_dt(ip, ky, mode = "in_place")
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


section("diff_table")

# 1) all equal
t1 <- diff_table(d1, d1)
check("1) identical tables: every value is the same", all(t1$status == "same"))
check("1) identical tables: one row per row x measure", nrow(t1) == nrow(d1) * 2L)
check("1) identical tables: the difference is zero", all(t1$diff == 0))

# 2) same rows, some values changed
d2 <- copy(d1)
set(d2, i = 1L, j = "NUM_01", value = d1$NUM_01[1L] + 10)
t2 <- diff_table(d1, d2)
check("2) one changed value is found", sum(t2$status == "changed") == 1L)
check("2) everything else is unchanged", sum(t2$status == "same") == nrow(t2) - 1L)
check("2) the difference is reported", t2[status == "changed", abs(diff - 10) < 1e-9])

# 3) all four statuses at once
x3 <- data.table(id = c("a", "b", "c"), v = c(1, 2, 3))
y3 <- data.table(id = c("a", "b", "d"), v = c(1, 2.5, 4))
t3 <- diff_table(x3, y3)
check("3) an unchanged row is 'same'", t3[id == "a", status] == "same")
check("3) a changed row is 'changed'", t3[id == "b", status] == "changed")
check("3) a row only in x is 'only_x'", t3[id == "c", status] == "only_x")
check("3) a row only in y is 'only_y'", t3[id == "d", status] == "only_y")
check("3) only_x has no y value", t3[id == "c", is.na(value_y)])
check("3) only_y has no x value", t3[id == "d", is.na(value_x)])

# 4) the two modes agree
t4 <- diff_table(x3, y3, mode = "new")
check("4) mode = 'new' reaches the same verdicts", identical(t4[order(id), status], t3[order(id), status]))
check("4) mode = 'new' is a full outer join", nrow(t4) == 4L)

# 5) tolerance
x5 <- data.table(id = c("a", "b"), v = c(1, 2))
y5 <- data.table(id = c("a", "b"), v = c(1.005, 2))
check("5) without a tolerance a tiny drift is a change", diff_table(x5, y5)[id == "a", status] == "changed")
check("5) inside the tolerance it is the same", diff_table(x5, y5, tolerance = 0.01)[id == "a", status] == "same")
check("5) the tolerance is absolute, so it is not one-sided", diff_table(y5, x5, tolerance = 0.01)[id == "a", status] == "same")
check("5) exactly at the tolerance still counts as same", diff_table(x5, y5, tolerance = 0.005)[id == "a", status] == "same")

# Inf - Inf is NaN, so a tolerance test alone called two identical infinities a
# change. Finding 4.
inf_x <- data.table(id = c("a", "b"), v = c(Inf, -Inf))
check("5) identical infinities are not a change", all(diff_table(inf_x, copy(inf_x))$status == "same"))
check(
  "5) but opposite infinities are",
  diff_table(inf_x, data.table(id = c("a", "b"), v = c(-Inf, Inf)))[id == "a", status] == "changed"
)
check(
  "5) an infinity against a finite value is a change",
  diff_table(data.table(id = "a", v = Inf), data.table(id = "a", v = 1))$status == "changed"
)
check(
  "5) equality still wins with a tolerance set",
  diff_table(inf_x, copy(inf_x), tolerance = 0.5)[id == "a", status] == "same"
)
check(
  "5) NA on one side only is a change",
  diff_table(data.table(id = "a", v = NA_real_), data.table(id = "a", v = 1))$status == "changed"
)

# 6) casting the candidate first
x6 <- data.table(id = c("a", "b"), v = c(1, 2))
y6 <- data.table(id = c("a", "b"), v = c("1", "2"))
check("6) a text column is cast to x's type first", all(diff_table(x6, y6)$status == "same"))
check_error("6) with cast = FALSE the type mismatch is reported up front", diff_table(x6, y6, cast = FALSE), "disagree on the type of: v")
check_error("6) a value that cannot be cast is refused", diff_table(x6, data.table(id = c("a", "b"), v = c("1", "two"))), "not a number")

# 7) which columns count as measures
x7 <- data.table(id = c("a", "b"), n = c(1L, 2L), v = c(1, 2))
y7 <- data.table(id = c("a", "b"), n = c(1L, 9L), v = c(1, 2))
check("7) integers are ids by default, so a change to one moves the row", diff_table(x7, y7)[, any(status %in% c("only_x", "only_y"))])
t7 <- diff_table(x7, y7, measures = "numeric+integer")
check("7) numeric+integer measures them instead", t7[metric == "n" & id == "b", status] == "changed")
check("7) and the unchanged double is still same", t7[metric == "v" & id == "b", status] == "same")

# 8) ids that do not tell rows apart
x8 <- data.table(id = c("a", "a", "b"), v = c(1, 2, 3))
y8 <- data.table(id = c("a", "a", "b"), v = c(1, 2, 30))
t8 <- diff_table(x8, y8)
check("8) duplicates = 'disambiguate' pairs rows by arrival order", nrow(t8) == 3L)
check("8) and finds the change", sum(t8$status == "changed") == 1L)
check("8) the rows carry their sequence number", "KEY_SEQ" %in% names(t8))

t8a <- diff_table(x8, y8, duplicates = "aggregate")
check("8) duplicates = 'aggregate' answers per group instead", nrow(t8a) == 2L)
check("8) it compares value multisets, not values", "value_key_x" %in% names(t8a))
check("8) the untouched group is the same", t8a[id == "a", status] == "same")
check("8) the changed group is changed", t8a[id == "b", status] == "changed")
check(
  "8) aggregate is order-free: a reordered group is still the same",
  all(diff_table(x8, data.table(id = c("a", "a", "b"), v = c(2, 1, 3)), duplicates = "aggregate")$status == "same")
)
check(
  "8) while disambiguate pairs by position, so reordering shows up",
  any(diff_table(x8, data.table(id = c("a", "a", "b"), v = c(2, 1, 3)))$status == "changed")
)
check_error("8) duplicates = 'error' refuses", diff_table(x8, y8, duplicates = "error"), "do not tell rows apart")

# 9) NA values
x9 <- data.table(id = c("a", "b"), v = c(1, NA_real_))
y9 <- data.table(id = c("a", "b"), v = c(1, NA_real_))
# Membership comes from the row identity, not from the value: NA says nothing
# about whether the row was there. Two rows that both hold NA are unchanged,
# and a value turning into NA is a change to that row, not a deletion of it.
t9 <- diff_table(x9, y9)
check("9) an unchanged non-NA row is same", t9[id == "a", status] == "same")
check("9) NA on both sides is same", t9[id == "b", status] == "same")
check("9) a value that became NA is changed", diff_table(data.table(id = "a", v = 1), x9[1L][, v := NA_real_])[, status] == "changed")
check("9) a value that arrived is changed", diff_table(data.table(id = "a", v = NA_real_), data.table(id = "a", v = 1))[, status] == "changed")

# 10) the inputs are never modified
x10 <- data.table(id = c("a", "b"), v = c(1, 2))
y10 <- data.table(id = c("a", "b"), v = c("1", "2"))
before_x <- copy(x10)
before_y <- copy(y10)
invisible(diff_table(x10, y10))
check("10) x is untouched", identical(x10, before_x))
check("10) y is untouched, even though it was cast", identical(y10, before_y))

# 11) shapes and refusals
check("11) a plain data.frame works too", all(diff_table(as.data.frame(x3), as.data.frame(x3))$status == "same"))
check("11) the result is keyed by the ids and the measure", identical(key(t3), c("id", "metric")))
check("11) a table compared with itself has no changes", all(diff_table(d1, d1)$status == "same"))
check_error("11) refuses a non-table", diff_table(x3, 1:3))
check_error("11) an unknown mode", diff_table(x3, x3, mode = "sideways"))
check_error("11) an unknown duplicates option", diff_table(x3, x3, duplicates = "ignore"))
check_error("11) nothing to measure", diff_table(data.table(id = "a"), data.table(id = "a")), "no columns of type")

# 13) the baseline has to be in the baseline types
x13 <- data.table(t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"), v = c(1, 2))
check_error("13) a POSIXct baseline is refused", diff_table(x13, copy(x13)), "unsupported type")
check_error("13) the refusal names the column and points at the fix", diff_table(x13, copy(x13)), "t \\(POSIXct\\).*normalize_dt")
check_error("13) a factor baseline is refused too", diff_table(data.table(f = factor("a"), v = 1), data.table(f = factor("a"), v = 1)), "f \\(factor\\)")
check_error("13) the gate applies with cast = FALSE as well", diff_table(x13, copy(x13), cast = FALSE), "unsupported type")
# Every offending column is named in one message, not just the first one hit.
x13m <- data.table(
  ok = c("a", "b"),
  t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"),
  f = factor(c("p", "q")),
  z = c(complex(real = 1), complex(real = 2)),
  v = c(1, 2)
)
msg13 <- tryCatch(diff_table(x13m, copy(x13m)), error = function(e) conditionMessage(e))
check("13) all non-baseline columns are reported, not just the first", grepl("t (POSIXct), f (factor), z (complex)", msg13, fixed = TRUE))
check("13) the baseline columns stay out of the message", !grepl("ok ", msg13) && !grepl("v (", msg13, fixed = TRUE))

msg13b <- tryCatch(
  normalize_dt(data.table(z = complex(real = 1), w = complex(real = 2))),
  error = function(e) conditionMessage(e)
)
check("13) normalize_dt reports every column it cannot convert", grepl("z (complex), w (complex)", msg13b, fixed = TRUE))
check("13) and stays quiet about the ones it can", !grepl("POSIXct", tryCatch(normalize_dt(x13m), error = function(e) conditionMessage(e))))

check("13) converting the baseline first makes it work", all(diff_table(normalize_dt(x13), copy(x13))$status == "same"))
check(
  "13) a POSIXct candidate needs no conversion of its own",
  all(diff_table(normalize_dt(x13), data.table(t = as.POSIXct(c("2024-01-01 22:00", "2024-01-02 22:00"), tz = "UTC"), v = c(1, 2)))$status == "same")
)
check(
  "13) a Date baseline still converts a POSIXct candidate in its own zone",
  diff_table(
    data.table(d = as.Date("2024-01-01"), v = 1),
    data.table(d = as.POSIXct("2024-01-01 23:30:00", tz = "Europe/Lisbon"), v = 1)
  )$status == "same"
)

# 14) cast = FALSE checks the types up front
x14 <- data.table(id = c("1", "2"), v = c(1, 2))
y14 <- data.table(id = c(1, 2), v = c(1, 2))
check_error("14) a type mismatch is named, not left to the join", diff_table(x14, y14, cast = FALSE), "disagree on the type of: id")
check_error("14) it reports both types", diff_table(x14, y14, cast = FALSE), "character vs numeric")
check_error(
  "14) a mismatch on a non-id column is caught too",
  diff_table(data.table(id = "a", v = 1), data.table(id = "a", v = "1"), cast = FALSE),
  "v \\(numeric vs character\\)"
)
check("14) matching types pass the check", all(diff_table(x14, copy(x14), cast = FALSE)$status == "same"))

# 12) a realistic fuzzed table, end to end
d12 <- fake_dt(50, n_cols = c(character = 2, date = 1, integer = 1, numeric = 2), seed = 3)
num12 <- summarize_dt(d12)[type == "numeric", colname]
f12 <- fuzz_dt(d12, pct_replace = 0.2, cols = num12, seed = 5)
t12 <- diff_table(d12, f12)
check("12) a fuzzed table reports changes", any(t12$status == "changed"))
check("12) and leaves the rest alone", any(t12$status == "same"))
check("12) every row gets a status", !anyNA(t12$status))

f12b <- fuzz_dt(d12, pct_replace = 0, pct_new = 0.1, cols = num12, seed = 6)
check("12) appended rows show up as only_y", any(diff_table(d12, f12b)$status == "only_y"))
check("12) dropped rows show up as only_x", any(diff_table(d12, d12[1:40])$status == "only_x"))


section("summary")

cat(sprintf("\n%d checks, %d failed\n", nrow(RESULTS), RESULTS[ok == FALSE, .N]))
if (RESULTS[ok == FALSE, .N]) {
  print(RESULTS[ok == FALSE, .(label)])
}
