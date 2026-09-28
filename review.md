---
status: pending
---

# daffiz 0.1.0 review and proposed changes

## Outcome

The finite-number comparison kernel is well designed: the tolerance rule is
symmetric, overflow is handled deliberately, source-row provenance is retained,
and the summaries reconcile with the canonical cell table. The package should
nevertheless not be used as a correctness-critical regression oracle until the
P0 items below are fixed. In particular, the current implementation can report
a match for semantically different missing values and identity columns, and a
valid `data.frame` shape can escape preflight and fail with a low-level stack
error.

The changes below are ordered by implementation priority. Each item includes
the intended behavior, concrete source changes, tests, and acceptance criteria.

## Key files

- `R/compare-dt.R` — cell classification and comparison construction.
- `R/preflight.R` — input, identity-type, and tolerance validation.
- `R/duplicates.R` — duplicate-key policies and heuristic pairing.
- `R/comparison.R` — comparison object and cache.
- `R/accessors-rows.R` — snapshot-based original-row recovery.
- `R/plot.R` — plot-data ordering and difference-map rendering.
- `R/report.R` — summary API and report limits.
- `tests/testthat/` — regression and API tests.
- `README.md`, `vignettes/comparing-tables.Rmd`, `NEWS.md`, and `CLAUDE.md` —
  user-facing and maintainer-facing contracts.
- `.github/workflows/pkgdown.yaml` and `_pkgdown.yml` — website validation and
  deployment.

## P0 — correctness and robustness

### 1. Distinguish `NA` from `NaN`

**Problem:** `classify_cells()` currently uses `is.na(vx) & is.na(vy)`, which
classifies `NA_real_` versus `NaN` as equal. That allows a computational failure
represented by `NaN` to replace an expected missing value without failing
`is_matching()` or `expect_dt_equal()`.

**Required behavior:**

- `NA_real_` versus `NA_real_` is `equal`.
- `NaN` versus `NaN` is `equal`.
- `NA_real_` versus `NaN`, in either direction, is `diff`.
- A missing value versus a finite or infinite value remains `diff`.

**Change `R/compare-dt.R`:** compute the two matching missing-value cases
explicitly before the `fcase()` call.

```r
both_nan <- is.nan(vx) & is.nan(vy)
both_na <- is.na(vx) & !is.nan(vx) & is.na(vy) & !is.nan(vy)

match_type <- fcase(
  is.na(cells$.in_x),                            "y_only",
  is.na(cells$.in_y),                            "x_only",
  both_nan,                                      "equal",
  both_na,                                       "equal",
  is.na(vx) | is.na(vy),                         "diff",
  is.infinite(vx) & is.infinite(vy) & vx == vy,  "equal",
  is.infinite(vx) | is.infinite(vy),             "diff",
  abs_diff <= tol,                               "equal",
  default =                                      "diff"
)
```

**Change tests:** update `tests/testthat/test-classify.R` so the two mixed
`NA`/`NaN` expectations require `"diff"`. Add an integration test in
`tests/testthat/test-report-and-expectation.R` proving that
`expect_dt_equal()` fails when an expected `NA_real_` becomes `NaN`.

**Documentation:** add the four missing-value rules to the `compare_dt()`
details and the comparison vignette. Record the behavior change in `NEWS.md`.

**Acceptance criteria:** no combination of `NA`, `NaN`, finite values, and
infinities can silently pass except the explicitly matching pairs above.

### 2. Restrict identity columns to safe one-dimensional vectors

**Problem:** preflight currently treats the underlying type as sufficient proof
that a column is joinable. A matrix column such as `I(matrix(...))` has type
`double`, passes gate 6a, and later produces `CStackOverflowError`. Arbitrary
classed vectors are also accepted even though `column_signature()` checks only
`levels` and `units`; for example, two `ts` columns with different `tsp`
attributes can align and report as matching.

**Required behavior:** reject shaped atomic columns and unknown classes with a
structured `daffiz_error_types` before duplicate grouping or joining.

**Change `R/preflight.R`:** replace the storage-type-only allow-list with an
explicit predicate. Keep the supported set deliberately small and extend it
only with a test demonstrating data.table join semantics.

```r
is_supported_identity <- function(column) {
  if (!is.null(dim(column))) return(FALSE)
  if (!typeof(column) %in% daffiz_joinable_types) return(FALSE)
  if (!is.object(column)) return(TRUE)

  is.factor(column) ||
    inherits(column, "Date") ||
    inherits(column, "POSIXct") ||
    inherits(column, "difftime") ||
    inherits(column, "integer64") ||
    inherits(column, "units") ||
    inherits(column, "ITime")
}
```

Use this predicate in gate 6a. The error should name the column, show its
type/class, and state that identity columns must be one-dimensional supported
vectors. Do not claim that any class built on a joinable storage type works.

Keep the existing signature checks for the accepted classes:

- factor `levels` must be identical;
- `difftime`, `hms`, and `units` units must be identical;
- POSIXct `tzone` may differ because it does not change the represented instant.

If support for another class is needed later, add it to the predicate together
with tests for equality, differing semantic attributes, missing values,
duplicates, and side-only rows.

**Change tests:** add cases to `tests/testthat/test-regressions.R` asserting:

- an `I(matrix(...))` identity fails with `daffiz_error_types`, not a base or
  data.table error;
- a `ts` identity fails with `daffiz_error_types` rather than silently ignoring
  `tsp`;
- all currently documented supported classes still compare successfully;
- an unsupported identity can still be removed with `exclude=`.

**Acceptance criteria:** no valid `data.frame` column shape reaches data.table
grouping unless daffiz has explicitly established that it is a scalar-valued,
join-safe identity vector.

## P1 — API contract corrections

### 3. Make heuristic duplicate pairing opt-in

**Problem:** lexicographically sorting each side is deterministic, but it does
not minimize mismatch count or magnitude when multiple measures and tolerances
are involved. For example:

```text
x: (a, b) = (0.0,   0), (0.1, 100)
y: (a, b) = (0.0, 100), (0.1,   0)
abs_tol = c(a = 0.1, b = 0)
```

Cross-pairing yields two fully matching records, while the current
lexicographic pairing reports two differing cells.

**Change `R/compare-dt.R`:** change the formal default to:

```r
duplicate_keys = c("error", "pair", "report")
```

This makes ambiguous identity an error unless the caller explicitly chooses a
heuristic or incomplete result. Keep the existing warning when `"pair"` is
selected.

**Change `R/duplicates.R`:** describe `"pair"` only as deterministic
lexicographic pairing. Do not claim that it minimizes differences.

**Change tests:**

- update tests that rely on the old default to pass `duplicate_keys = "pair"`;
- assert that the new default raises `daffiz_error_duplicates`;
- add the counterexample above and assert the current deterministic pairing,
  without asserting optimality;
- retain permutation and original-row traceability tests.

**Documentation:** update `README.md`, `compare_dt()` documentation, the
vignette, `NEWS.md`, and `CLAUDE.md`. Remove every statement that the sort
“minimizes apparent differences.” Explain that `"pair"` is reproducible but is
not an assignment optimization and can produce tolerance-dependent false
negatives.

**Acceptance criteria:** no default call can return `TRUE` after resolving an
ambiguous identity through an undocumented assumption.

### 4. Prevent trace-column collisions from corrupting snapshots

**Problem:** reserved columns can currently be excluded from comparison, but an
input `.row_x` or `.row_y` is then overwritten when the source row number is
stamped. `original_rows()` no longer preserves that original column's values or
class.

**Change `R/preflight.R`:** reject `.row_x` and `.row_y` in either input before
processing `exclude=`. These two names cannot be made safe by exclusion because
they are also part of the original-row recovery API. Continue allowing other
reserved working names to be excluded.

Use a specific message such as:

```text
Input column(s) collide with daffiz source-row trace columns: .row_x.
Rename these columns before comparison; they cannot be resolved with `exclude=`.
```

**Change tests:** add regression tests for `.row_x` and `.row_y` on both sides,
including calls that try to exclude the collision. Verify that caller inputs
remain unchanged after the error.

**Documentation:** narrow the existing statement about excluding reserved
columns and document the exception for `.row_x`/`.row_y`.

**Acceptance criteria:** `original_rows(cmp, side, "all")` always contains the
original values plus an unambiguous, newly created trace column.

### 5. Preserve explicit plot measure order

**Problem:** `validate_measure_filter()` preserves `columns`, but `plot_data()`
immediately reorders it back to `cmp$settings$compare` order.

**Change `R/plot.R`:** delete this assignment:

```r
columns <- cmp$settings$compare[cmp$settings$compare %chin% columns]
```

No replacement is needed. `validate_measure_filter(NULL, ...)` already returns
input-column order, while an explicit character vector is returned in caller
order with duplicates removed.

**Change tests:** add unbinned and binned cases asserting that
`plot_data(cmp, columns = c("b", "a"))$column` and the ggplot x-axis use `b, a`.

**Documentation:** state that explicit `columns` order controls the plot axis.

**Acceptance criteria:** the order passed by the caller is the order returned by
`plot_data()` and drawn by `plot_diff()`.

### 6. Reject unused summary arguments

**Problem:** `summary(cmp, max_row = 0)` and `summary(cmp, 0)` are silently
accepted through `...`, but neither changes the report limits.

**Change `R/report.R`:** inspect `list(...)` at the start of
`summary.daffiz_comparison()` and raise `daffiz_error_report` when it is not
empty. Include supplied argument names, using `<unnamed>` where necessary.

**Change tests:** assert focused errors for a misspelled named limit and an
unnamed positional value. Retain successful named-limit calls through both
`summary()` and `print()`.

**Acceptance criteria:** a typo cannot silently produce a larger report than
the caller requested.

## P1 — documentation and release integrity

### 7. Rebuild the website from the current API and make it self-cleaning

**Problem:** checked-in pages still describe the removed `type`, `delta`, and
`max_columns` plotting API. `pkgdown::check_pkgdown()` fails because there is no
root `_pkgdown.yml`, and deployment uses `clean: false`, so obsolete pages and
assets can survive indefinitely.

**Add `_pkgdown.yml`:** define the site URL and enumerate the exported API so a
newly exported function cannot disappear from the reference index unnoticed.

```yaml
url: https://pedrobtz.github.io/daffiz/

template:
  bootstrap: 5

reference:
  - title: Compare tables
    contents:
      - compare_dt
      - daffiz_row_number
      - is_matching
      - expect_dt_equal
  - title: Inspect differences
    contents:
      - all_cells
      - diff_cells
      - column_summary
      - diff_columns
      - row_summary
      - diff_rows
      - diff_indices
      - original_rows
      - x_only
      - y_only
      - duplicate_info
      - key_profile
  - title: Plot differences
    contents:
      - plot_data
      - plot_diff
      - plot.daffiz_comparison
  - title: Comparison objects
    contents:
      - summary.daffiz_comparison
      - all.equal.daffiz_comparison
```

**Change `.github/workflows/pkgdown.yaml`:** run
`pkgdown::check_pkgdown()` before building and change the deployment option to
`clean: true`.

**Generated files:** choose one source of truth. The recommended approach is to
stop tracking `docs/` on `main`, add `/docs/` to `.gitignore`, and let the
workflow deploy freshly generated output to `gh-pages`. If generated docs must
remain tracked, rebuild all of them in the same change and add a CI assertion
that they match current sources.

Remove obsolete overview images and pages. Do not publish `docs/CLAUDE.html`.

**Change `CLAUDE.md`:** remove brittle hard-coded test and expectation counts;
state the verification command and most recent check result instead.

**Acceptance criteria:**

- `pkgdown::check_pkgdown()` succeeds;
- all examples on the published site use the current function signatures;
- a removed page or asset disappears on the next deployment;
- `CLAUDE.md` is not rendered into the public site.

## P2 — hardening and clarity

### 8. Clarify default role inference

The current inference treats only shared unclassed doubles as measures. Integer
measures and changed context columns can therefore become identity fields,
causing a role error or unmatched rows instead of numeric cell differences.
This is documented, so it is not a direct implementation defect, but it is a
sharp edge for an API described as comparing numeric measures.

Keep the rule for 0.1.x only if the README's first example is followed by a
prominent recommendation to specify `by=` and `compare=` in automated tests.
For a future API, consider requiring explicit `by=` in `expect_dt_equal()` and
then treating all remaining unclassed integer/double columns as measures.

### 9. Define comparison-object mutability as unsupported

The S3 object exposes mutable data.tables and a shared environment cache.
Directly changing `cmp$cells` can make `is_matching(cmp)` disagree with a
previously cached `column_summary(cmp)`.

Document `daffiz_comparison` as opaque and direct users to accessors. For a
future breaking release, store mutable internals behind a private environment
or remove the cache unless profiling demonstrates that it is still necessary.
Add a structural validation test if direct field access remains part of the
supported API.

### 10. Correct the plot accessibility claim

The redundant tile symbols are omitted for more than 400 tiles and for binned
plots, so the statement that classification never depends on colour is too
broad. Either provide another scalable encoding or state precisely when symbols
are available and direct users to `plot_data()` for a non-visual representation.

## Test additions summary

- [ ] Mixed `NA`/`NaN` cells fail matching and `expect_dt_equal()`.
- [ ] Matrix/array identity columns fail during daffiz preflight.
- [ ] Unknown semantic classes such as `ts` fail during daffiz preflight.
- [ ] Supported identity classes retain their documented behavior.
- [ ] Duplicate keys error by default and pair only when explicitly requested.
- [ ] The tolerance-aware duplicate counterexample is documented by a test.
- [ ] Trace-column collisions cannot be bypassed with `exclude=`.
- [ ] Explicit plot measure order survives binned and unbinned paths.
- [ ] Unknown or positional summary arguments fail clearly.
- [ ] `pkgdown::check_pkgdown()` succeeds in CI.

## Verification commands

Run all of the following after implementation:

```sh
air format .
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(reporter = "summary")'
Rscript -e 'covr::package_coverage()'
Rscript -e 'pkgdown::check_pkgdown()'
```

Build and check a fresh tarball outside the repository:

```sh
review_dir=$(mktemp -d)
cd "$review_dir"
R CMD build /absolute/path/to/daffiz
R CMD check --as-cran daffiz_*.tar.gz
```

Required final state:

- 0 test failures;
- no material coverage regression;
- `pkgdown::check_pkgdown()` succeeds;
- 0 `R CMD check --as-cran` errors or warnings;
- only understood, environment- or submission-specific check notes;
- clean git worktree except for the intended implementation and generated
  documentation changes.

## Decision log

### 2026-09-15 — Prefer false failures over false passes

**Decision:** distinguish `NA` from `NaN`, reject unknown identity classes, and
make duplicate pairing opt-in.

**Rationale:** daffiz is intended for regression detection. A false failure is
visible and diagnosable; a false pass can silently approve incorrect output.
