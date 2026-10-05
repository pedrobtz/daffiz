# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`daffiz` is a small R package around one function, `diff_table()`, which
compares two tables value by value, plus a few helpers (`diff_summary()`,
`expect_table_equal()`, `compare_columns()`, `normalize_dt()`,
`cast_rules()`).

Version 0.1.0 was rebuilt around the core in `.agents/compare.R`. Its planning
documents live in `.agents/`:

- `plan.md`: what 0.1.0 is;
- `core-review.md`: the findings (F1–F8, D-a…D-h) the rebuild fixed;
- `roadmap.md`: the features of the earlier draft that were dropped, where
  their code lives, and how to re-add them.

The draft is tagged `draft-0.1.0`; read any of its files with
`git show draft-0.1.0:R/<file>`. `.agents/` is `.Rbuildignore`d.

Comments in `R/` carry reasoning that is not recoverable from the code. Read
them before rewriting a function.

Do not claim the package "checks cleanly" without re-running
`R CMD check --as-cran` on a fresh tarball; that claim was wrong once already.
Two NOTEs are expected: "New submission", and "unable to verify current time"
when the machine cannot reach a time server.

## Commands

Standard `devtools` workflow, run from the package root:

```r
devtools::load_all()      # load without installing
devtools::document()      # regenerate NAMESPACE + man/ from roxygen
devtools::test()          # run the testthat suite
devtools::check()         # full R CMD check
```

Run one test file:

```r
devtools::test(filter = "diff-table")         # tests/testthat/test-diff-table*.R
testthat::test_file("tests/testthat/test-cast.R")
```

From the shell:

```sh
R CMD build . && R CMD check --as-cran daffiz_*.tar.gz
```

## Architecture

`diff_table()` runs a 13-step pipeline, numbered in its body. Each helper it
calls lives in its own file and is tested on its own:

| File | Responsibility |
|---|---|
| `R/diff-table.R` | `diff_table()`: arguments, names, columns per mode, types, roles, index, duplicates, alignment, melt, merge, classification |
| `R/names.R` | `canonical_names()`, `name_map()`, `check_names()`, `shallow_dt()` |
| `R/types.R` | `col_type()`, the only type vocabulary; `CAST_RULES`; `cast_rules()` |
| `R/cast.R` | `cast_value()`, `cast_dt()` (truncation record and message), `normalize_dt()` |
| `R/columns.R` | `summarize_dt()`, `compare_columns()` |
| `R/melt.R` | `select_measures()`, `index_dt()` (row numbers and key), `melt_dt()` |
| `R/keys.R` | `unique_by_key()`, `disambiguate_by_key()`, `aggregate_by_key()` |
| `R/merge.R` | `merge_dry_run()` (index-only, exact), `merge_dt()` (one-to-one only) |
| `R/summary.R` | `diff_summary()`, `is_finite_pair()`, `scaled_rmse()` |
| `R/expect.R` | `expect_table_equal()` |
| `R/conditions.R` | `daffiz_abort()`, `daffiz_warn()`, `daffiz_inform()`, `fmt_names()` |

The tests in `test-types.R`, `test-cast.R`, `test-columns.R`, `test-melt.R`,
`test-keys.R`, `test-merge.R`, `test-diff-table.R` and
`test-diff-table-cases.R` were ported mechanically from `.agents/test.R` and
`.agents/test-cases.R`. They use the `check()` helpers in `helper-check.R`,
and the setup between their `test_that()` blocks runs at file level on
purpose, because later checks reuse it. New tests are ordinary testthat
(`test-diff-table-modes.R`, `test-names.R`, `test-summary.R`,
`test-expect.R`).

## Load-bearing details

Each is covered by a test.

- **User column names are normalized to `A-Z`, `0-9` and `_`; every internal
  column contains a lowercase letter.** That is what makes a collision
  between the two impossible (`test-names.R` asserts it for every internal
  name). A new internal column name must contain a lowercase letter. Names
  that normalize to nothing, or to the same thing within one table, are an
  error for the caller to fix, never suffixed or guessed.
- **The caller's tables are never modified.** `shallow_dt()` shares the
  caller's column vectors, so later steps may *replace* a column
  (`set(j = , value = )` swaps a pointer) but never write into one.
  `index_dt()` copies the columns it keeps, because `setkeyv()` reorders rows
  in place, writing into the vectors.
- **Benchmark mode drops `y`'s extra columns before anything else.** The
  core's final `setnames(..., "_x")` relabels every column carried from `y`,
  so a `y`-only column that reached the merge came out named as `x`'s.
- **Truncating casts are allowed by default, and announced by a message, not
  a warning.** The benchmark's types are the truth: Spark, ADLS and Delta turn
  a locally built double into an integer, and the comparison has to follow.
  A warning would fail `expect_no_warning()` pipelines on this expected path.
  `truncate = FALSE` refuses the cast instead.
- **Duplicates are checked and numbered on the wide table, before the melt.**
  The answer is the same as on the melted table, the work is smaller by the
  number of measures, and `key_seq` is the same for every measure of a row.
  The pairing warning fires only when there are measures: without them the
  paired rows are identical in every compared column. `expect_table_equal()`
  relies on exactly that, turning the warning into an error.
- **Pairing is by arrival order, never by value.** Sorting by value pairs rows
  so as to minimise differences and can pass tables that disagree; arrival
  order can only produce false failures.
- **A comparison that aligns nothing is refused before the melt.** Every row
  would be `only_x`/`only_y`, which almost always means a wrong `by` or a
  key-format skew, and it is the case that builds the largest table. Two
  empty tables are exempt.
- **Membership comes from `row_id` or `n_rows`, never from the value.** An NA
  value says nothing about whether the row was there.
- **Equality before tolerance.** `Inf - Inf` is NaN, so a tolerance test alone
  calls two identical infinities a change.
- **`nan_is_na` must act in three places**: value cells, the `%a` encoding of
  `"aggregate"`, and double key columns. data.table joins and groups NA and
  NaN as different keys (verified), so skipping the keys splits a row into
  `only_x` + `only_y`.
- **The `%a` multiset encoding sorts the encoded strings, in radix (C
  locale) order, after `+ 0`.** `sort()` leaves NA and NaN in arrival order
  among themselves, and `%a` writes `-0` and `0` differently.
- **Magnitude statistics filter on the source values, not on `diff`.** Two
  finite values can differ by more than the largest double; filtering the
  difference drops that value and reports the largest difference as 0.
  `scaled_rmse()` keeps squaring from overflowing.
- **Counts that can pass 2^31 are doubles, and are never formatted with
  `%d`.** `sprintf("%d", 3e9)` is an error, so the message for exactly the
  fanout worth refusing would fail to build.
- **Text dates before the year 1000 are refused, by reading the year off the
  parsed date.** The format-back round trip alone is platform-dependent:
  glibc writes year 24 as `"24"` under `%Y` (macOS writes `"0024"`), so
  `"24-01-01"` passed on Linux only. CI caught it; macOS cannot reproduce it.
- **`col_type()` is the only type vocabulary.** A second one (`class()[1]`)
  made IDate and integer64 one type to one function and another to the next.
- **A table is well-formed when it has at least one key column; it may have
  no measures.** With no key, the error offers `row_key`, which adds a
  lowercase `row_number` key: the position, or the rank after sorting by
  every compared column. `"sorted"` sorts in radix (C locale) order with NaN
  as NA, so both tables sort alike in any session. Compared exactly it can
  only pair badly, never pass different rows.
- **Measure keywords are lowercase; normalized names never are.** So
  `measures = "numeric"` can never mean a column.
- **`NORMALIZE_TO` has no POSIXct default.** Turning a stamp into a Date
  drops the time and can make two keys one; the caller chooses.

## Conventions

- `data.table` is the engine. Express comparisons as joins and grouped
  operations; avoid row-wise R loops.
- Conditions carry a `daffiz_*` class. An *error* means the question cannot
  be answered; a *warning* means the answer may be wrong; a *message* reports
  an expected consequence of the settings. Messages name columns in the
  caller's own spelling (`name_map()$label`).
- Keep the public surface small; new features go through `.agents/roadmap.md`
  first.
- `docs/`, `.claude/`, `.agents/`, `benchmarks/` and `inst/WORDLIST` are
  listed in `.Rbuildignore`. `benchmarks/` still measures the draft's
  pipeline.
- This file lives in `.claude/`, not at the top level: pkgdown renders every
  top-level `.md` file into the site, with no setting to exclude one.
- `docs/` is the local pkgdown build and is git-ignored; the site is built
  and deployed by `.github/workflows/pkgdown.yaml`.
