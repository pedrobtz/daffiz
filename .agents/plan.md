---
status: implemented (2026-09-29, branch redo-core)
---

# daffiz 0.1.0

A small package around one function, `diff_table()`, plus a few helpers.
Built from `.agents/compare.R`; fixes reference `core-review.md` (F1–F8,
D-a…D-h). Anything not listed here is out of scope for 0.1.0 (see
`roadmap.md`).

## API

```r
diff_table(x, y,
           mode = c("benchmark", "equal"),
           by = NULL,
           measures = "numeric",        # or "numeric+integer", or column names
           tolerance = 0,
           duplicates = c("disambiguate", "aggregate", "error"),
           cast = mode == "benchmark",
           truncate = TRUE,
           nan_is_na = TRUE,
           date_format = "%Y-%m-%d")

diff_summary(d)                    # counts per measure (+ magnitudes)
expect_table_equal(object, expected, ...)
compare_columns(x, y)              # the core's compare_dt(), renamed
normalize_dt(dt, to, cols)         # convert x to supported types
cast_rules(wide = TRUE)            # the conversion table
```

Everything else in `compare.R` stays internal.

## How `diff_table()` behaves

**Modes.**

- **`"benchmark"`** (default): `x` is the source of truth.
  - `y`'s extra columns are dropped, with a message.
  - A column missing from `y` is an error.
  - `y` is cast to `x`'s types.
- **`"equal"`**: the tables are peers.
  - Every column must be on both sides.
  - Types must already agree; nothing is cast.
  - The result is symmetric.

**Column names.** Both tables are normalized to `A-Z`, `0-9` and `_`:
upper-case, replace every other run of characters with `_`, and trim `_` from
both ends.

- A name that ends up empty is an error.
- Two names in one table that normalize to the same thing are an error the
  user must fix. In benchmark mode that only applies to `y` columns `x` uses.
- The core's own columns are lowercase, so they can never collide.

**Casting (benchmark only).** Numeric → integer and numeric → Date truncate
when `truncate = TRUE`, with one message listing the columns, counts and
examples. `truncate = FALSE` refuses them.

**Measures and identity.**

- `measures` picks the numeric columns to compare, by type keyword or by name,
  always read from `x`.
- The row identity is `by`, or every column that is not a measure.
- With `by` given, left-over columns are ignored with a message in
  `"benchmark"` and are an error in `"equal"`.
- With no measures at all, the question becomes "do they have the same rows?".

**Values.** A cell is `same` when both values are equal, or `|y - x| <=
tolerance`. With `nan_is_na = TRUE`, NaN counts as NA: in cells, in
`"aggregate"`, and in key columns.

**Duplicates.**

- `"disambiguate"` pairs rows by arrival order, with a warning. There is no
  warning when there are no measures, since identical rows are
  interchangeable.
- `"aggregate"` compares the multiset of values per key (`tolerance` must be
  0).
- `"error"` refuses.

**Result:** a keyed `data.table`, with `status` a factor (`same`, `changed`,
`only_x`, `only_y`). Its shape is in `attr(d, "shape")`:

| Shape | When | One row per |
|---|---|---|
| `cells` | measures exist | row × measure: `metric`, `row_id_x/y`, `value_x/y`, `diff` (`y - x`) |
| `groups` | `duplicates = "aggregate"` | key × measure: `n_rows_x/y`, `value_key_x/y` |
| `rows` | no measures | row: `row_id_x/y` |

**Errors, warnings and messages.**

- An error when the question cannot be answered: missing columns, type
  differences in `"equal"`, name collisions, no identity, no rows in common.
- A warning when the answer may be wrong: invented pairing.
- A message for expected side effects: truncation, dropped or ignored
  columns, no measures.
- Every condition has a `daffiz_*` class and names columns as the user wrote
  them.
- The caller's tables are never modified.

## Changes to the core

- **Types**
  - `col_type()` recognises `integer64` (refused).
  - `summarize_dt()` uses `col_type()`.
  - `NORMALIZE_TO` loses the POSIXct default.
- **Casting**
  - `cast_value()` and `cast_dt()` take `truncate`; `cast_dt()` records the
    truncations and raises one message (F1).
  - `cast_value()` also: catch NaN → NA, drop the dead branch, use a UTC
    fallback.
- **Names:** new `canonical_names()` (F2).
- **Columns:** `compare_dt()` → `compare_columns()` (D-f).
- **Melt**
  - `melt_dt()` takes measure names and promotes integers (D-b, D-g).
  - It stops calling `uniqueN()` and copying.
  - Its row-numbering half becomes `index_dt()`, so duplicates are checked
    before melting.
- **Keys**
  - `disambiguate_by_key()`: `rowid` only; `KEY_SEQ` → `key_seq` (D-d).
  - `aggregate_by_key()`: `nan_is_na`, signed zero, a count-only case (D-c).
- **Merge:** `merge_dt()` messages use `format()`, not `%d` (F7).
- **`diff_table()`:** the behaviour above, fixing F3–F6 and F8. It keeps the
  in-place merge only.
- **All files:** `stop()` → `daffiz_abort()` with classes.

## Steps

Each step is one commit, with tests passing and a clean
`R CMD check --as-cran`.

1. **Set up.**
   - Tag the old code `draft-0.1.0`.
   - Add `.agents` to `.Rbuildignore`.
   - Replace `R/` with `compare.R` split by section.
   - Port `.agents/test.R` and `.agents/test-cases.R` to testthat, unchanged.
2. **Fix the core helpers** (the "Changes to the core" list above, minus
   `diff_table()`). Each fix starts with its failing repro from
   `core-review.md`.
3. **Rebuild `diff_table()`** on them, with tests for:
   - every mode × `measures` option × `by`;
   - the symmetry of `"equal"`;
   - NA/NaN under both settings;
   - the `rows` shape under each duplicate policy;
   - the inputs being left unmodified.
4. **Add `diff_summary()` and `expect_table_equal()`.**
   `expect_table_equal()` calls `diff_table(expected, object, ...)` and
   defaults to `duplicates = "error"`.
5. **Release.** Write the README (benchmark, equal, and rows-only examples)
   and NEWS for 0.1.0, update CLAUDE.md, run `R CMD check --as-cran` on a
   fresh tarball, and release.
