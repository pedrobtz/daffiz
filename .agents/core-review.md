---
status: pending
---

# Review of the new core (`.agents/compare.R`)

Reviewed on 2026-09-28 against `.agents/compare.R` as committed in `5259b00`.
Both scratch suites pass (`test.R`: 296 checks, `test-cases.R`: 32 checks), so
everything below is something those suites do not cover. Every **Confirmed**
finding was reproduced with a probe script; the repro is inlined.

## What to keep as-is

These are the core's strongest ideas. The rebuild should not dilute them.

- **`CAST_RULES` as the single authority.** `cast_value()` consults the table
  before doing any work, and `cast_rules()` prints it. The docs cannot drift
  from the code.
- **Atomic `cast_dt()`.** Every column is validated before any is written.
- **Round-trip checks.** A Date must re-format to the exact input text, and a
  double must re-read from its text. These catch `"2024-01-01junk"` and
  15-digit collisions that R's coercion lets through.
- **`merge_dry_run()`.** Index-only, exact, and the fanout product is done in
  doubles. It is the right gate for a join that must be one-to-one.
- **Membership from `row_id`, never from the value.** An NA value says nothing
  about whether the row existed.
- **Equality before tolerance.** `Inf == Inf` is tested first, so `Inf - Inf =
  NaN` never reaches the tolerance comparison.
- **`aggregate_by_key(stats = "exact")` via `%a`.** Exact and order-free, and
  it invents no identity.
- **Baseline asymmetry is explicit.** `x` fixes the types and `y` is cast to
  them. That is a clearer contract than the old symmetric inference. It is
  kept as `mode = "benchmark"`, the default, next to a symmetric
  `mode = "equal"` (F3).

## P0 — wrong answers

### F1. Truncating casts: allowed on purpose, but not optional or visible — Confirmed, revised

Numeric → integer is `as.integer()`, which truncates, and `cast_dt()` casts
**every shared column**:

```r
x <- data.table(id = c("a","b"), n = c(1L, 2L))
y <- data.table(id = c("a","b"), n = c(1.7, 2))
diff_table(x, y, measures = "numeric+integer")$status   # "same" "same"
# character: "3.7" cast to integer -> 3L
```

**Truncation is intended** (decided 2026-09-29). The baseline `x` holds the
true types, and `y` is forced into them. That covers the common case of a
table built locally with doubles and stored in a system that turns those
columns into integers (Spark, Azure ADLS, Delta). Without truncation, a
comparison against the stored table would fail on the storage conversion
rather than on the data.

What is still missing:

- **It cannot be turned off.** Nothing lets a caller ask for exact casts only.
- **It is invisible.** Nothing reports that truncation happened, or where. If
  `n` is an identity column (the default `measures = "numeric"`), a truncated
  value can land on an existing key, and two rows then become a duplicate
  group or align with the wrong row. The caller should be able to see that
  this is why.
- **The contract text is wrong.** `cast_value()`'s title ("refusing anything
  lossy") says the opposite of what the code does.

**Fix:**

- Add `truncate = TRUE` to `cast_value()`, `cast_dt()` and `diff_table()`.
  With `FALSE`, numeric → integer refuses `num != trunc(num)` ("not a whole
  number"); the range check applies either way.
- Apply the same switch to numeric → Date, where `1.5` becomes a fractional
  day that prints like `1970-01-02` but does not equal it. It should either
  truncate to a whole day (`TRUE`) or be refused (`FALSE`), never be kept
  fractional.
- Record what was truncated: per column, the number of values changed, and a
  few examples. `cast_dt()` returns it as an attribute, and `diff_table()`
  carries it on its result.
- **Alert the user with a message when truncation happens** (decided
  2026-09-29). Raise one `message()` per call, not one per column, as a
  subclassed condition (`daffiz_message_truncation`, carrying the record
  above). It names each column, how many values were truncated, and up to
  three examples (`1.7 -> 1`). No message when nothing was truncated.
  - **Why a message rather than a warning:** under the storage workflow,
    truncation is expected, not a fault. A warning would fail
    `expect_no_warning()` and `options(warn = 2)` pipelines on the normal
    path.
  - **Silencing it:** `suppressMessages()` works, or
    `withCallingHandlers(daffiz_message_truncation = ...)` for callers who
    want to act on it.
  - **Where it is raised:** in `cast_dt()`. `diff_table()` and `expect_*()`
    pass it through.
- Reword `cast_value()`'s title and the `CAST_RULES` notes. They should say
  "lossy only where `truncate` allows it".

### F2. The core's own column names overwrite or break on user columns — Confirmed

The comment in `melt_dt()` assumes "callers' columns are upper case". That is
true of `fake_dt()` and of little else. The results:

| User column | What happens |
|---|---|
| `row_id` (as a key) | Overwritten. No identity is left, and the call fails with "`dt` has no key to check". |
| `metric` | `melt()` renames it with `make.names`, and the key becomes `<metric, metric>`. |
| `status` | Silently overwritten by the classification; the user's values are lost. |
| `value`, `diff`, `KEY_SEQ`, `n_rows`, `value_key`, `*_x`, `*_y` | Each collides with a column the core creates. |

**Fix (decided 2026-09-29): normalize user column names, and keep the core's
names lowercase.** Before any other work, rename every column of both tables
with `canonical_names()`, so that a normalized name uses only `A-Z`, `0-9`
and `_` (digits kept, decided 2026-09-29).
The core's own columns always contain a lowercase letter, so the two can
never collide. It also makes the assumption in `melt_dt()`'s comment true by
construction.

The rule, in order:

1. Upper-case with `toupper()`.
2. Replace every run of characters outside `A-Z` and `0-9` with a single `_`:
   spaces, punctuation and non-ASCII letters alike. Use
   `gsub("[^A-Z0-9]+", "_", x, perl = TRUE)`, which does not depend on the
   locale, so the result is the same in every session.
3. Trim leading and trailing `_` (decided 2026-09-29).
4. A name that ends up empty (`"%"`, `" "`, `"_"`) is a
   `daffiz_error_columns` error: the user must rename it.

| Original | Normalized |
|---|---|
| `amount` | `AMOUNT` |
| `Unit Price` | `UNIT_PRICE` |
| `unit.price`, `unit-price`, `unit  price` | `UNIT_PRICE` (a collision if two are in one table) |
| `amount ` (trailing space, common in CSV headers) | `AMOUNT` |
| `prix_unité` | `PRIX_UNIT` |
| `Q1`, `Q2` | `Q1`, `Q2` |
| `2024` | `2024` |
| `%` | error: empty after normalization |

**Why trim:** without step 3, `amount ` in one table and `amount` in the
other would become `AMOUNT_` and `AMOUNT` and fail to pair, over an invisible
trailing space.

**Why digits are kept:** without them, `Q1`/`Q2` and `sales_2023`/
`sales_2024` would all collide. Digits cannot clash with the core's names
either, since those always contain a lowercase letter.

Conditions for this to hold:

- **Every internal name must contain a lowercase letter.** `KEY_SEQ` breaks
  this and becomes `key_seq`. The suffixes are already lowercase (`ID_x` can
  never equal a user column `ID_X`), and so are `row_id`, `metric`, `value`,
  `diff`, `status`, `n_rows`, `n_na` and `value_key`. Add a unit test that
  asserts this for the whole internal name list, so a future name cannot
  quietly break the guarantee.
- **Collisions after normalization are an error (decided 2026-09-29).**
  `amount` and `Amount`, or `unit price` and `UNIT_PRICE`, in the same table
  would become one column. The user must fix it by renaming or dropping one
  of them.
  - **No workaround:** no suffixing, no "keep the first", and no argument to
    bypass the check.
  - **Class:** `daffiz_error_columns`, carrying the colliding groups.
  - **Message:** names the table (`x` or `y`) and lists each group as its
    normalized name with every original that maps to it, for example
    `AMOUNT <- "amount", "Amount"`. It ends with "rename or drop these
    columns before comparing".
  - **When:** the check runs on each table separately, before anything else.
    `amount` in `x` and `Amount` in `y` are not a collision; they are the
    intended match.
  - **Which collisions count:** in `mode = "equal"`, every collision in
    either table. In `mode = "benchmark"`, every collision in `x`, and in `y`
    only a collision on a name `x` uses; the others are dropped anyway (see
    F3).
- **Keep the mapping.** Store `original → normalized` for each side as an
  attribute of the result. Reports can then show the user's names, and 3.3/3.4
  can translate back. The `metric` values are normalized names too.
- **It changes matching across the two tables:** `amount` in `x` and `AMOUNT`
  in `y` now pair up. That is a benefit here, since Spark/Delta are
  case-insensitive about column names and often lowercase them, which is the
  same round trip as F1. Document it as intended.
- **Broad normalization merges more names.** Replacing every other
  character means `unit.price` and `unit_price` become one name. Within a
  table that is a collision error, so it is never silent. Across the two
  tables it is a deliberate match, which also covers storage that rewrites
  punctuation in column names.
- **Collisions with internal names are impossible by construction.** Every
  normalized name is non-empty and made of `A-Z`, `0-9` and `_`, while every internal
  name contains a lowercase letter.

A reserved-name gate is no longer needed. See "Column names" in `plan.md`.

### F3. Columns present on one side only break `diff_table()` — Confirmed

The core's `compare_dt()` says `y` "may carry extra columns", but
`diff_table()` does not handle them:

- **y-only identity column:** it appears in the output as `note_x`, but the
  values are `y`'s. The final `setnames(out, carried, paste0(carried, "_x"))`
  renames every `ly` column, including ones `x` never had.
- **x-only identity column:** the call fails with "not in both tables: note",
  which does not name the real problem.
- **x-only measure:** every cell is `only_x`, although each row exists on both
  sides. A missing column gets the same label as a missing row.

**Fix (decided 2026-09-29): two modes, chosen with
`mode = c("benchmark", "equal")`.** Each mode answers a different question.
Both run right after name normalization (F2) and before any casting or
melting.

**`mode = "benchmark"` (the default).** Question: *does the tested table match
the benchmark?* `x` is the master/benchmark and the source of truth; `y` is
the table under test.

- **Columns only in `y` are dropped.** They are extra output of the table
  under test and play no part in the comparison. This fixes the first case:
  `note` never reaches the join, so nothing can be mislabelled `note_x`.
- **Columns only in `x` are an error** (recommended; please confirm). The
  benchmark defines the contract, so a column it has and the tested table
  lacks is a regression. Silently comparing the rest would hide it. The error
  (`daffiz_error_columns`) names the missing columns in their original `x`
  spelling. This fixes the second and third cases. There is no confusing "not
  in both tables" message, and no `only_x` cells for a column that is missing
  rather than a row.
- **Announce what was dropped** (recommended). One subclassed message,
  `daffiz_message_dropped_columns`, lists `y`'s dropped columns in their
  original spelling, like the truncation message. Record them in the same
  result attribute as the name mapping. That way a typo in `y` (for example
  `amout`) is visible instead of silently vanishing, and the error then also
  reports `amount` as missing.
- **Types:** `y` is cast to `x`'s types, truncating where `truncate` allows
  (F1).
- **Name collisions (F2):** on `y`, only collisions that land on a name `x`
  uses are errors. `y` having `amount` and `Amount` when `x` has `AMOUNT` is
  ambiguous; `foo` and `Foo` that `x` does not use are dropped anyway. On `x`,
  every collision is an error.

**`mode = "equal"`.** Question: *are these two tables equal?* The tables are
peers; neither is the source of truth.

- **All columns must match.** A column on either side only is an error
  (`daffiz_error_columns`) that lists both directions: "only in `x`: …" and
  "only in `y`: …", each in its original spelling. Nothing is dropped. This is
  the draft's gate 4.
- **Types must already agree; nothing is cast** (recommended). With no source
  of truth there is no target type to cast to, and casting either side would
  make one of them the benchmark after all. A column whose `col_type()`
  differs is an error listing each column as `name (integer vs numeric)`. So
  `truncate` does not apply in this mode. A lossless integer ↔ double
  widening is a possible later addition (recorded in the roadmap).
- **Name collisions (F2):** every collision in either table is an error.
- **The result is symmetric.** `diff_table(x, y, mode = "equal")` and
  `diff_table(y, x, mode = "equal")` give the same statuses, with `only_x`
  and `only_y` swapped and `diff` negated. Add that as a test.

**In both modes**, the measures are chosen once, from `x`, and passed to both
melts (D-b). After the column step both tables have the same columns and
types, so `x` and `y` would give the same list.

`compare_columns()` reports every column on both sides, whatever the mode. It
is the schema view; `diff_table()` is the comparison.

### F4. `NA` vs `NaN` is `same` — Confirmed

`is.na(NaN)` is `TRUE`, so the rule "NA on both sides is not a change" also
passes a computation that now returns `NaN` where it used to return `NA`. The
old `review.md` raised the same problem (P0 #1).

**Fix (decided 2026-09-29): make it configurable, with `nan_is_na = TRUE` as
the default.** `TRUE` treats NaN as NA, so NA vs NaN is `same`; `FALSE` keeps
them apart. The option must act everywhere a missing value is compared, or
the answer depends on where the value sits:

- **Value cells:** the classification above.
- **`duplicates = "aggregate"`:** `%a` writes `NA` and `NaN` differently, so
  map NaN to NA before encoding when `TRUE`.
- **Double identity columns:** data.table joins and groups NA and NaN as
  different keys (checked 2026-09-29), so map NaN to NA before duplicate
  detection and the join when `TRUE`.

See "Values" in `plan.md`.

## P1 — errors that name the wrong thing, or none

### F5. A table with no identity columns fails with a message about keys — Confirmed

`diff_table(data.table(v = 1:2 + 0), data.table(v = c(1, 3)))` raises a
data.table warning about a zero-length `cols`, then "`dt` has no key to check".
**Fix:** raise a daffiz error that says no identity columns were found and
suggests passing `by=` or adding a key. Positional alignment is in the roadmap.

### F6. `tolerance` is not validated — Confirmed

- `-1` behaves like `0`.
- `NA` makes every non-identical pair `changed`.
- `c(1, 0)` fails inside `fcase()` with "Argument #5 has length 2".

**Fix:** require a single finite number `>= 0`.

### F7. The fanout error fails while being built — Confirmed

`merge_dt()` formats `est_merge_rows` and `max_fanout` (doubles) with `%d`.
`sprintf("%d", 3e9)` is an error, so the message for exactly the huge fanout
it guards against becomes "invalid format '%d'". This is the same class of bug
as the old size projection (CLAUDE.md, "must be double"). **Fix:** use
`format(x, big.mark = ",")`.

### F8. Duplicate keys are disambiguated without any signal — Confirmed

With the default `duplicates = "disambiguate"`, the only sign that identity
was invented is a `KEY_SEQ` column. Pairing by arrival order can only produce
false *failures*, which is why it is a safer default than the old sorted
pairing. It should still be announced. **Fix:** emit a subclassed warning
(`daffiz_warning_duplicates`) carrying the `unique_by_key()` groups.

## P2 — design

### D-a. Two type vocabularies — Confirmed

`summarize_dt()` uses `class(x)[1]`, while everything else uses `col_type()`.
They disagree:

- **IDate vs Date:** `compare_dt()` reports a type mismatch (`Date` vs
  `IDate`), but `diff_table()` treats the columns as the same type. `fread()`
  returns IDate.
- **integer64:** `col_type()` says `numeric`, so it passes the `x` check and
  the cast rules treat it as a double. `summarize_dt()` says `integer64`, so
  `melt_dt()` does not melt it and it becomes an identity column. `fread()`
  returns integer64 for large ids.

Because `melt_dt()` picks measures through `summarize_dt()`, the vocabulary
affects results, not just the report.

**Fix:** `col_type()` is the only vocabulary. Add an explicit `integer64`
branch before `is.numeric()` and refuse it, pointing at `normalize_dt()`.

### D-b. Measures are chosen separately for each side

`melt_dt(xx)` and `melt_dt(yy)` each pick their own measures, so the two
tables need not melt the same columns. Once F3 fixes the column set, choose
measures once from `x` and pass the same names to both melts.

### D-c. `duplicates = "aggregate"` changes the output and ignores `tolerance` — Confirmed

That mode returns `n_rows_*`/`value_key_*` with no `diff` or `row_id`, and it
compares exactly whatever `tolerance` is set to. `%a` also separates `0` from
`-0`, which the value path calls equal: `q` returns `changed` under aggregate
and `same` otherwise.

**Fix:**

- Error when `tolerance > 0` is combined with `"aggregate"`.
- Normalise signed zero with `v + 0` before `sprintf("%a")`.
- Document the second output schema in `@return`.

### D-d. `value_rank` pairs different rows for each measure — Confirmed

`frank()` runs within `(key, metric)`, so `KEY_SEQ = 1` can be source row 1
for measure `p` and source row 2 for measure `q`. The "row" stops being one
row. `diff_table()` never uses it. **Suggest:** drop it and keep `rowid` only.
It is recorded in the roadmap.

### D-e. Two merge modes produce the same output

`mode = "in_place"` never costs more than `"new"`. `rbind()` copies only when
`y` has extra keys, while `"new"` always builds a new table. Keeping both
doubles the paths to test ("modes agree") for no difference in output.
**Suggest:** keep `in_place` as the only path inside `diff_table()` and drop
the `mode` argument. `merge_dt()` can keep both modes internally. (The name `mode` is then reused for the benchmark/equal choice in F3.)

### D-f. `compare_dt()` name clash

In the core, `compare_dt()` is the column-schema comparison. In the draft it is
the package's entry point. **Suggest:** rename the core's version to
`compare_columns()`.

### D-g. `numeric+integer` warns on every call — Confirmed

`melt()` warns that the integer measures are coerced to double.
**Fix:** promote integer measures with `set(..., as.double())` before the melt,
as the draft did.

### D-h. Smaller design points

- **`status`:** make it a factor with fixed levels (`same`, `changed`,
  `only_x`, `only_y`), so `table()` shows zero counts and the levels are a
  documented contract.
- **Errors:** all use plain `stop()`. Use `daffiz_abort()` with subclasses
  (keep `R/conditions.R`), so tests and callers can catch them by class.
- **`NORMALIZE_TO` POSIXct → Date:** it is lossy: two stamps on the same day
  become one key, which then shows up as a "duplicate". Consider having no
  default for POSIXct, so the caller must choose.

## P3 — performance

- **`melt_dt()` runs `summarize_dt()`:** that calls `uniqueN()` on every column
  just to find the measure names. Use `col_type()` alone.
- **Copies:**
  - `diff_table()` runs `copy(as.data.table(y))` and `melt_dt()` copies again;
    for a `data.frame`, `as.data.table()` has already copied once, so `y` is
    duplicated up to three times.
  - `melt()` never modifies its input, so a table that only carries the
    row-number column is enough:
    `setDT(c(list(.row = seq_len(nrow(dt))), as.list(dt)))` shares the column
    vectors.
- **Duplicate work on the long table:** `unique_by_key()` and
  `disambiguate_by_key()` run on the long table (`n × m` rows). Each wide row
  gives exactly one long row per metric, so both can run on the wide table
  (`n` rows) before melting. That is m-fold cheaper, and it makes the sequence
  number the same for every measure of a row by construction.

## P3 — minor

- **Dead code:** the `logical` branch for integer/numeric sources in
  `cast_value()`. `CAST_RULES` refuses those pairs before the branch is
  reached.
- **Silent NaN → NA:** `NaN` → integer becomes `NA`, and the "nothing may turn
  into NA" backstop cannot see it, because `is.na(NaN)` is already `TRUE`.
- **Permissive number parsing:** character → numeric accepts `" 0x10 "` as
  `16`. That is looser than the Date path's canonical check. It is acceptable,
  but should be documented.
- **Session-dependent Date:** POSIXct → Date with an empty `tzone` uses
  `Sys.timezone()`, so the result depends on the session. Use `"UTC"`, or
  document it.
- **Top-level `library(data.table)`:** it must go. The package already uses
  `import(data.table)`.
- **Repeated validation:** `date_format` is checked on every per-column call.
  Validate it once in `cast_dt()`.
