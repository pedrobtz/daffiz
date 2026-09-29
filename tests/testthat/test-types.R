# Ported from .agents/test.R. Setup between blocks stays at file level,
# in order, because later checks reuse what earlier ones built.

test_that("col_type", {
  check("col_type: character", col_type("a") == "character")
  check("col_type: logical", col_type(TRUE) == "logical")
  check("col_type: integer", col_type(1L) == "integer")
  check("col_type: numeric", col_type(1.5) == "numeric")
  check("col_type: Date before its storage mode", col_type(Sys.Date()) == "Date")
  check("col_type: POSIXct before its storage mode", col_type(Sys.time()) == "POSIXct")
  check("col_type: factor is itself, not integer", col_type(factor("a")) == "factor")
  check("col_type: anything else falls back to its class", col_type(complex(real = 1)) == "complex")
})

test_that("cast rules table", {
  check("CAST_RULES: covers every from x to pair but the identities", nrow(CAST_RULES) == length(CAST_FROM) * length(CAST_TYPES) - length(CAST_TYPES))
  check("CAST_RULES: no self-conversions", all(CAST_RULES$from != CAST_RULES$to))
  check("CAST_RULES: every rule carries a note", all(nzchar(CAST_RULES$note)))
  check("cast_rule: finds a pair", isTRUE(cast_rule("integer", "numeric")$allowed))
  check("cast_rule: finds a refusal", isFALSE(cast_rule("factor", "numeric")$allowed))
  check("cast_rule: an unknown pair is NA, not an error", is.na(cast_rule("integer", "complex")$allowed))
  check("cast_rule: an identity pair has no rule", is.na(cast_rule("integer", "integer")$allowed))
  check("cast_rules(wide = FALSE): the long table", identical(dim(cast_rules(FALSE)), dim(CAST_RULES)))
  check("cast_rules(FALSE): a copy, not the original", !identical(address(cast_rules(FALSE)), address(CAST_RULES)))
})

w <- cast_rules()

test_that("cast rules table", {
  check("cast_rules(): one row per source type", nrow(w) == length(CAST_FROM))
  check("cast_rules(): one column per target type, plus from", identical(names(w), c("from", CAST_TYPES)))
  check("cast_rules(): the diagonal reads '='", w[from == "integer", integer] == "=")
  check("cast_rules(): an allowed pair reads 'yes'", w[from == "integer", numeric] == "yes")
  check("cast_rules(): a refused pair reads '-'", w[from == "integer", logical] == "-")
})
