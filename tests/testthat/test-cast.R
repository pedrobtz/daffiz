# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

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

test_that("cast_value: every pair in the rules table", {
  check(
    paste0("cast_value: all ", nrow(allowed_ok), " allowed pairs convert"),
    all(allowed_ok$ok)
  )
})

if (!all(allowed_ok$ok)) print(allowed_ok[ok == FALSE])

refused_ok <- CAST_RULES[allowed == FALSE][, .(
  ok = inherits(
    tryCatch(cast_value(cast_sample(from, to), to, "col"), error = function(e) e),
    "error"
  )
), by = .(from, to)]

test_that("cast_value: every pair in the rules table", {
  check(
    paste0("cast_value: all ", nrow(refused_ok), " refused pairs error"),
    all(refused_ok$ok)
  )
})

if (!all(refused_ok$ok)) print(refused_ok[ok == FALSE])

test_that("cast_value: the details of each conversion", {
  check("cast_value: same type is returned untouched", identical(cast_value(3L, "integer", "col"), 3L))
  check("cast_value: text to number", cast_value("1.5", "numeric", "col") == 1.5)
  check("cast_value: text to integer truncates toward zero", cast_value("3.7", "integer", "col") == 3L)
  check("cast_value: negative truncation is toward zero too", cast_value("-3.7", "integer", "col") == -3L)
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
})

x_ref <- data.table(id = 1L, score = 1.5, day = as.Date("2024-01-01"), flag = TRUE)

y_cand <- data.table(id = "2", score = "3.5", day = as.POSIXct("2024-01-02 23:30:00", tz = "UTC"), flag = "FALSE")

suppressMessages(cast_dt(x_ref, y_cand))

test_that("cast_dt", {
  check("cast_dt: casts every shared column to x's type", identical(summarize_dt(y_cand)$type, summarize_dt(x_ref)$type))
  check("cast_dt: converts the values, not just the types", y_cand$id == 2L && y_cand$score == 3.5)
})

y_partial <- data.table(id = "2", extra = "left alone")

cast_dt(x_ref, y_partial)

test_that("cast_dt", {
  check("cast_dt: a y-only column is left alone", is.character(y_partial$extra))
})

y_cols <- data.table(id = "2", score = "3.5")

cast_dt(x_ref, y_cols, cols = "id")

test_that("cast_dt", {
  check("cast_dt: `cols` limits what is cast", is.integer(y_cols$id) && is.character(y_cols$score))
})

y_atomic <- data.table(id = "2", score = "not a number")

test_that("cast_dt", {
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
})

test_that("normalize_dt", {
  check("NORMALIZE_TO: only a factor has a default", identical(NORMALIZE_TO, c(factor = "character")))
  check("NORMALIZE_TO: every target is a baseline type", all(NORMALIZE_TO %in% CAST_TYPES))
})

stamps <- data.table(t = as.POSIXct(c("2024-01-01 10:00", "2024-01-02 10:00"), tz = "UTC"), v = c(1, 2))

b1 <- normalize_dt(stamps, to = c(POSIXct = "Date"))

test_that("normalize_dt", {
  check("normalize_dt: POSIXct becomes a Date when asked", col_type(b1$t) == "Date")
  check_error("normalize_dt: a POSIXct has no default target", normalize_dt(stamps), "t \\(POSIXct\\)")
  check("normalize_dt: on the day the stamp prints", b1$t[1L] == as.Date("2024-01-01"))
  check("normalize_dt: a baseline column is untouched", identical(b1$v, stamps$v))
  check("normalize_dt: the input is not modified", col_type(stamps$t) == "POSIXct")
  check(
    "normalize_dt: the day is taken in the stamp's own zone, not UTC",
    normalize_dt(data.table(t = as.POSIXct("2024-01-01 23:30:00", tz = "Europe/Lisbon")), to = c(POSIXct = "Date"))$t == as.Date("2024-01-01")
  )
  check(
    "normalize_dt: dropping the time can merge two stamps into one day",
    uniqueN(normalize_dt(data.table(t = as.POSIXct(c("2024-01-01 01:00", "2024-01-01 23:00"), tz = "UTC")), to = c(POSIXct = "Date"))$t) == 1L
  )
  check(
    "normalize_dt: `to` overrides that, keeping the full stamp",
    col_type(normalize_dt(stamps, to = c(POSIXct = "character"))$t) == "character"
  )
  check("normalize_dt: a factor becomes its labels", identical(normalize_dt(data.table(f = factor(c("7", "9"))))$f, c("7", "9")))
  check("normalize_dt: `cols` limits what is converted", col_type(normalize_dt(stamps, cols = "v")$t) == "POSIXct")
})

clean <- data.table(id = "a", v = 1)

test_that("normalize_dt", {
  check("normalize_dt: a table already in baseline types comes back as is", identical(normalize_dt(clean), clean))
  check("normalize_dt: zero rows", nrow(normalize_dt(stamps[0], to = c(POSIXct = "Date"))) == 0L)
  check("normalize_dt: keeps no truncation record", is.null(attr(b1$t, "truncated")))
  check_error("normalize_dt: an unmapped type is refused, not guessed", normalize_dt(data.table(z = complex(real = 1))), "no conversion given")
  check_error("normalize_dt: the refusal names the column and its type", normalize_dt(data.table(z = complex(real = 1))), "z \\(complex\\)")
  check_error("normalize_dt: an empty `to` refuses everything unconvertible", normalize_dt(stamps, to = character()), "no conversion given")
  check_error("normalize_dt: a target with no cast rule", normalize_dt(stamps, to = c(POSIXct = "logical")), "no sound conversion")
  check_error("normalize_dt: an unknown column", normalize_dt(stamps, cols = "nope"), "unknown column")
  check_error("normalize_dt: refuses a non-table", normalize_dt(1:3))
})

test_that("truncate = FALSE refuses every cast that drops a fraction", {
  expect_error(cast_value(1.7, "integer", "N", truncate = FALSE), "not a whole number", class = "daffiz_error_cast")
  expect_error(cast_value("3.7", "integer", "N", truncate = FALSE), "character -> integer, not a whole number")
  expect_error(cast_value(1.5, "Date", "D", truncate = FALSE), "not a whole day")
  expect_error(
    cast_value(as.POSIXct("2024-01-01 10:00", tz = "UTC"), "Date", "D", truncate = FALSE),
    "has a time of day"
  )
  expect_error(
    cast_value(as.POSIXct(90.5, tz = "UTC"), "integer", "S", truncate = FALSE),
    "not a whole number"
  )
  # Exact values pass either way.
  expect_identical(cast_value(c(1, 2), "integer", "N", truncate = FALSE), c(1L, 2L))
  expect_equal(
    cast_value(as.POSIXct("2024-01-01", tz = "UTC"), "Date", "D", truncate = FALSE),
    as.Date("2024-01-01")
  )
})

test_that("a truncating cast records what it dropped", {
  out <- cast_value(c(1.7, 2, -3.7, 1.7), "integer", "N")
  expect_identical(as.vector(out), c(1L, 2L, -3L, 1L))
  rec <- attr(out, "truncated")
  expect_identical(rec$n, 3L)
  # One example per distinct value.
  expect_identical(rec$examples, c("1.7 -> 1", "-3.7 -> -3"))
  expect_null(attr(cast_value(c(1, 2), "integer", "N"), "truncated"))
})

test_that("a number becomes the Date it prints as, never a fractional one", {
  d <- cast_value(c(1.5, -0.5), "Date", "D")
  expect_identical(as.numeric(d), c(1, -1))
  expect_identical(format(d), c("1970-01-02", "1969-12-31"))
})

test_that("a stamp with no zone is read as UTC, whatever the session's zone", {
  stamp <- as.POSIXct("2024-01-01 23:30:00", tz = "UTC")
  attr(stamp, "tzone") <- ""
  old_tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ = old_tz))
  Sys.setenv(TZ = "Asia/Tokyo")
  expect_equal(as.vector(cast_value(stamp, "Date", "D")), as.Date("2024-01-01"))
})

test_that("NaN becomes NA only when they are the same missing value", {
  expect_true(is.na(cast_value(NaN, "integer", "N")))
  expect_error(cast_value(NaN, "integer", "N", nan_is_na = FALSE), "NaN", class = "daffiz_error_cast")
  expect_error(cast_value(NaN, "Date", "D", nan_is_na = FALSE), "NaN")
  # Text keeps NaN as "NaN", so there is nothing to refuse.
  expect_identical(cast_value(NaN, "character", "C", nan_is_na = FALSE), "NaN")
})

test_that("cast_dt reports truncation once, as a message with a record", {
  x <- data.table(a = 1L, b = 1L, c = "k")
  y <- data.table(a = c(1.7, 2), b = c("3.2", "3.2"), c = "k")
  expect_message(cast_dt(x, y), class = "daffiz_message_truncation")
  rec <- attr(y, "truncated")
  expect_identical(rec$column, c("a", "b"))
  expect_identical(rec$n, c(1L, 2L))
  expect_identical(rec$examples, c("1.7 -> 1", "3.2 -> 3"))
})

test_that("cast_dt is quiet, with an empty record, when nothing was truncated", {
  y <- data.table(a = c(1, 2))
  expect_no_message(cast_dt(data.table(a = 1L), y))
  expect_identical(nrow(attr(y, "truncated")), 0L)
})

test_that("cast_dt with truncate = FALSE refuses before writing anything", {
  y <- data.table(a = c("1", "2"), b = c(1.5, 2))
  before <- copy(y)
  expect_error(cast_dt(data.table(a = 1L, b = 1L), y, truncate = FALSE), "column b")
  expect_identical(y, before)
})

test_that("cast_dt names columns by their labels in messages", {
  y <- data.table(AMOUNT = "x")
  expect_error(
    cast_dt(data.table(AMOUNT = 1), y, labels = c(AMOUNT = "\"amount\" (AMOUNT)")),
    "column \"amount\" \\(AMOUNT\\)"
  )
})

test_that("a short year is refused on every platform, not only where %Y pads", {
  # glibc formats year 24 as "24", so the text round trip alone would accept
  # "24-01-01" on Linux. The year is checked on the date itself.
  expect_error(cast_value("24-01-01", "Date", "col"), "not a canonical", class = "daffiz_error_cast")
  expect_error(cast_value("024-01-01", "Date", "col"), "not a canonical")
  expect_error(cast_value("0024-01-01", "Date", "col"), "not a canonical")
  expect_equal(cast_value("1024-01-01", "Date", "col"), as.Date("1024-01-01"))
})
