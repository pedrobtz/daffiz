# State of the Art in Table Comparison

Research notes for `daffiz`. Reviewed 2026-08-29.

The goal of this document is to decide what `daffiz` should be. Four reference
tools were studied, each representing a different *model* of what "comparing two
tables" means. They are not competitors of each other so much as answers to
different questions.

Research snapshot: daff
[`ad5a02e`](https://github.com/paulfitz/daff/commit/ad5a02efd58ea7a2a70bb888f65ab610d136db28),
DataComPy
[`fd91514`](https://github.com/capitalone/datacompy/commit/fd91514fe5452d25e41796ac7bc97db4be530ac0),
diffly
[`3bbdce9`](https://github.com/Quantco/diffly/commit/3bbdce9a0c77608be15d1b6f8cc9c7ce7e7a69c1),
and anofox_tabular
[`40ab2d3`](https://github.com/DataZooDE/anofox-tabular/commit/40ab2d3ce2760e77f0b64824852ac6adefad291b).

| Tool | Model | Core question it answers |
|---|---|---|
| [daff](https://github.com/paulfitz/daff) | diff / patch / merge | *What edits turn table A into table B?* |
| [DataComPy](https://github.com/capitalone/datacompy) | reconciliation | *Do these two extracts agree, within tolerance?* |
| [diffly](https://github.com/Quantco/diffly) | interactive exploration | *Where and how did my pipeline output change?* |
| [anofox_tabular](https://github.com/DataZooDE/anofox-tabular) | SQL-native set diff | *Which rows were added / removed / changed?* |

---

## 1. daff — the diff/patch/merge model

daff treats a table the way `git` treats a text file. It is optimised for
comparing multiple versions of the *same* table, and its output is itself a
table that can be stored, edited, transmitted, and re-applied.

### Data model

The output is a **highlighter diff**: a normal table with one extra leading
*action column* (named `__hilite_diff__` when column order is not meaningful).
Every row carries a tag:

| Tag | Meaning |
|---|---|
| `@@` | header row (column names) |
| `!` | schema row — present only when the column structure differs |
| `+++` | row inserted (in REMOTE only) |
| `---` | row deleted (in LOCAL only) |
| `->` | row with modified cells (repeatable as `-->`, `--->`, … to avoid collisions with data) |
| `:` | row reordered |
| `+` | row with added cells |
| *(blank)* | context row, identical in both |
| `...` | elided block of unchanged rows |

The schema row uses the same vocabulary at the column level: `+++` inserted
column, `---` deleted column, `(old_name)` renamed column, `:` reordered, `...`
elided columns.

Changed cells are encoded inline as `LOCAL->REMOTE` using the same separator as
the row tag. NULL is escaped by an underscore convention (`NULL` = actual null,
`_NULL` = the literal string `"NULL"`, `__NULL` = `"_NULL"`, …) so a diff
survives a CSV round-trip through unaware tools.

### Capabilities

- **Patch.** A diff is a first-class object: `patch_data(x, diff)` reapplies it.
  It can be edited before applying — you can accept a subset of changes.
- **3-way merge.** `compareTables3()` / `merge_data(parent, a, b)` computes a
  merge from a common ancestor and emits conflict markers; `which_conflicts()`
  locates them.
- **Ordered vs unordered.** CSV inputs default to *ordered* (row position is
  meaningful, so reorders are reported); JSON defaults to *unordered*.
- **Key inference.** Without `--id`, daff aligns rows by content similarity.
  With explicit ids, alignment is key-driven and far more stable.
- **Rendering.** CSV, TSV, JSON, HTML, and ANSI-coloured terminal output.
- **Context control.** `unchanged_context`, `show_unchanged`,
  `show_unchanged_columns`, `never_show_order`, `padding_strategy` — the same
  knobs as `diff -U`.

The R binding (`daff` on CRAN, wrapping daff.js through V8) exposes
`diff_data`, `patch_data`, `merge_data`, `render_diff`, `write_diff`,
`read_diff`, `which_conflicts`.

### What to take

- **The diff as data.** The single most valuable idea: comparison output is a
  table, not a printed report. Everything downstream (render, filter, patch,
  serialise) is a function over that table.
- **Schema changes as named operations** are useful for a later structural-diff
  layer. V1 deliberately keeps the draft's stricter regression contract and
  fails immediately when column sets differ.
- **Column renames and reorders as named operations**, not as
  "one column vanished and another appeared".
- **A tag vocabulary** — a small closed set of change types is worth designing
  once and reusing throughout the API.

### What to leave

- The `LOCAL->REMOTE` in-cell string encoding is a *rendering* choice driven by
  CSV round-tripping. In R we have lists and typed columns; keeping old and new
  values in separate typed columns is strictly better, with the string form
  produced only at render time.
- Content-similarity row alignment without a key is expensive and its results
  are hard to explain. `daffiz` instead uses a transparent column-role default
  for its narrow numeric workflow, offers explicit `by=`, and always reports
  the resolved identity.

---

## 2. DataComPy — tolerance and comparator semantics

DataComPy (Capital One) is the reconciliation tool: two extracts of what should
be the same data, and the question is whether the difference matters.

### API shape

```python
Compare(df1, df2,
        join_columns='acct_id',   # or on_index=True
        abs_tol={'default': 0, 'amount': 0.01}, rel_tol=0,
        df1_name='original', df2_name='new',
        ignore_spaces=False, ignore_case=False,
        cast_column_names_lower=True,
        custom_comparators=[...])
```

Comparison is **key-based by construction** — you must supply `join_columns`
or `on_index`. There is no alignment heuristic.

### Semantics worth stealing

- **Two tolerances, applied together.** Current Pandas and Polars comparators
  delegate to NumPy `isclose`, whose effective rule is
  `|a - b| <= abs_tol + rel_tol * |b|`. Absolute tolerance handles values near
  zero; relative tolerance handles large magnitudes. The rule is asymmetric.
  The `compare_dt` draft instead implements a symmetric `math.isclose`-style
  rule, `|a-b| <= max(abs_tol, rel_tol * max(|a|, |b|))`; `daffiz` should keep
  the symmetric behavior and document the deliberate incompatibility.
- **Per-column tolerances and per-type comparators.** Current DataComPy accepts
  scalar or named tolerances with a `default` entry. It also has an ordered
  comparator framework: caller-supplied comparators run before its numeric,
  string/date, array and boolean defaults. Strings get `ignore_spaces` /
  `ignore_case`; column *names* get `cast_column_names_lower`. Equality is a
  dispatch problem, not one global rule.
- **Duplicate keys are handled, not rejected.** DataComPy generates a
  sequential id within each duplicate key group, joins on
  `(key…, group_seq)`, and drops the helper afterwards. This is the same
  mechanism the `compare_dt` draft uses with `.row_key`. DataComPy documents
  the essential caveat: matching within a duplicate group is naive and callers
  may need more join columns. If a regression-testing package pairs duplicate
  identities by default, it must report the heuristic and retain original row
  indices so every pairing is auditable.
- **The report is the product.** `report()` emits a structured text document:
  frame summary, column summary, row summary, per-column match statistics,
  sample mismatches, and unique rows. Current DataComPy also exposes typed
  report data before template rendering and supports masking sensitive columns.
  Users read the report first and interrogate the objects second.

### Accessor surface

A useful checklist of what a comparison object should expose:

`matches()`, `report()`, `intersect_rows`, `df1_unq_rows`, `df2_unq_rows`,
`df1_unq_columns()`, `df2_unq_columns()`, `intersect_columns()`,
`all_mismatch()`, `sample_mismatch(col)`, `count_matching_rows()`,
`all_columns_match()`, `subset()`.

### Known weaknesses

- Nulls in join columns are stringified and filled with the sentinel
  `'DATACOMPY_NULL'` — real data containing that string breaks the comparison.
  **Do not use sentinel values.** `data.table` joins are already NULL-safe if
  written that way; anofox's `IS NOT DISTINCT FROM` is the correct semantics.
- Both frames must fit in memory.
- Multiple backends (Pandas, Polars, Spark, Snowpark) with a shared core; the
  Fugue cross-backend layer was dropped in v1.0.

---

## 3. diffly — the modern interactive API

diffly (QuantCo, Polars-based) is the newest of the four and the one whose
*ergonomics* are closest to what `daffiz` should aim for. Its stated use case is
"compare the output of your data pipeline before and after a code change".

### Design

```python
cmp = compare_frames(left, right, primary_key="id")
cmp.equal()
cmp.summary(top_k_column_changes=..., show_sample_primary_key_per_change=...)
cmp.left_only()
cmp.right_only()
cmp.fraction_same(column="amount")
cmp.change_counts(column="status")
```

The comparison object is **lazy and cached**: it is constructed cheaply, each
accessor computes on demand, and results are memoised. Under Polars lazy frames
the whole thing can defer to the query engine.

A supplied `primary_key` must actually be unique on both sides; diffly rejects
duplicates rather than inventing an alignment. Without a primary key it can
still compare whole frames for unordered equality by sorting them, but join-
based exploratory accessors such as `joined()`, `fraction_same()` and
`change_counts()` raise a `PrimaryKeyError`.

### The ideas that matter

- **One object, many questions.** `compare_frames` returns a handle you
  *interrogate*, rather than a fixed list of pre-computed data frames. The
  `compare_dt` draft currently computes and returns everything eagerly —
  `dt1_only`, `dt2_only`, `diff`, `all`, plus two profile tables — whether or
  not the caller wants them.
- **Summary and exploration are separate modes.** A printed summary for the
  first look; typed accessors returning real data frames for the follow-up.
  Every accessor returns something you can pipe into further code.
- **`change_counts()` is the standout feature.** For a given column, it returns
  the distinct `(old_value, new_value)` transitions with their frequencies and
  example primary keys. This turns "3,412 rows differ" into "3,400 of them are
  `pending -> active`, and 12 are something else" — which is almost always the
  actual question. Nothing else in this survey has it.
- **`fraction_same(column=)`** — a per-column scalar agreement rate makes it
  trivial to rank columns by how badly they diverged.
- **Rich tolerance model:** absolute, relative, **per-column** overrides, and a
  separate **temporal tolerance** for date/datetime columns (a duration, not a
  number). Dates are not floats and should not share their tolerance parameter.
- **Full type support** including nested types (lists, structs, enums) — their
  explicit differentiator over datacompy.
- **Testing utilities:** assertion helpers so a comparison can back a unit test,
  not only an interactive session.
- **Metrics and safe display:** recent versions accept user-defined per-frame
  and change metrics in summaries and can hide sensitive columns and keys.

---

## 4. anofox_tabular — SQL-native row diffing

A DuckDB C++ extension (DataZooDE). Diffing is one module among a broader set of
validation, PII, profiling, and anomaly-detection tools. The current diff
functions are bind-time SQL rewrites: they generate a full outer join and leave
planning and vectorised execution to DuckDB.

```sql
LOAD anofox_tabular;
SELECT * FROM diff_joindiff(
  'source_table',
  'target_table',
  ['id', 'region'],
  ['amount', 'status'],  -- default: all shared non-key columns
  false                 -- include unchanged rows?
);
```

Output schema: a `diff_type` column taking `'added' | 'removed' | 'changed' |
`'unchanged'`, the primary key columns, and the remaining **target-side**
columns. This is a wide, row-level result, not a long cell-level diff. Removed
rows have NULL target values, so the output alone does not preserve the source
values needed for a patch.

`diff_hashdiff` currently generates the same full outer join as
`diff_joindiff`. Hash/bisection tuning arguments are explicitly rejected because
that algorithm has not been implemented.

### What to take

- **`diff_type` as a row-level enum.** Rather than returning disjoint added,
  removed, changed, and unchanged frames, expose one derived row summary with a
  discriminator and make the splits accessors. In `daffiz`, that row view is an
  aggregation over the canonical melted numeric cell table.
- **`include_all` / `compare_columns`.** Two small parameters that cover most
  real usage: restrict *which* columns are compared, and choose whether
  unchanged rows come back.
- **NULL-safe key matching** (`IS NOT DISTINCT FROM`). Nulls in a key must
  match nulls, and must never be replaced by a sentinel.
- **Push work to the engine.** The whole design is "the database already knows
  how to join — express the diff as a join". For `daffiz` the equivalent is:
  express everything as `data.table` joins and grouped operations, never as
  row-wise R loops.

### Limitations relevant to daffiz

- Key uniqueness is not validated, so duplicate keys can create a many-to-many
  join fan-out.
- Added/removed columns are not schema events; value comparison is limited to
  columns shared by both inputs.
- Changed and removed rows do not retain paired source/target values in the
  output.
- The current `hashdiff` name does not imply a different execution plan.

---

## 5. The R landscape

Worth knowing what already exists, since `daffiz` must justify itself against
it:

- **`daff`** — the R binding to daff.js via V8. Unique in offering patch and
  3-way merge. Costs a JS runtime dependency and marshals data through it.
- **`diffdf`** — a light-weight `PROC COMPARE` for pharma QC workflows. Keyed,
  tolerance-aware, prints a report. Closest in spirit to DataComPy.
- **`arsenal::comparedf`** — the fullest `PROC COMPARE` reimplementation, with a
  configurable "control" object for comparison rules.
- **`compareDF::compare_df`** — grouping-variable based, strong HTML/XLSX
  rendered output.
- **`waldo::compare`** — general-purpose R object comparison; excellent
  narration, but not table-shaped and not scalable.
- **`dataCompare`** — a Shiny front-end.

**The gap for this package.** The immediate opportunity is narrower: a fast,
numeric, melted `data.table` comparison that is easy to inspect by column and
by original source row. The reference tools supply useful reporting and API
ideas, but none defines this exact R-native workflow.

---

## 6. Conclusions for `daffiz`

### The synthesis

Keep the draft's **melted numeric `data.table`**, add **DataComPy's tolerance and
report semantics**, **diffly's interrogable object**, **anofox's presence-safe
full join**, and later use **daff's visualization/edit vocabulary** where it
fits. General patch/merge machinery must not displace the numeric regression
use case.

### Design decisions this research supports

1. **Fail fast in stages.** After resolving `exclude`, unequal column sets,
   invalid requested columns, incompatible types, and invalid tolerances stop
   before any melt or join. Different rows remain valid diff results.

2. **Retain the long numeric cell table.** Double measures keep `value_x`,
   `value_y`, signed/absolute differences, and a closed match classification.
   Keeping equal cells makes rates and overview plots direct aggregations.

3. **Keep the convenient role defaults.** Context columns identify rows and
   shared unclassed doubles are measures. Add explicit `by=` and `compare=` for
   exceptions, exclude classed doubles such as dates from inference, and print
   the resolved roles.

4. **Preserve original row identity.** Stamp both inputs before sorting or
   melting, carry `.row_x` and `.row_y` through the join as values, and provide
   direct index and original-row accessors.

5. **Use symmetric numeric tolerance.** Apply
   `|x-y| <= max(abs_tol, rel_tol * max(|x|,|y|))`, with scalar and per-column
   settings. Remove the draft's lossy double-to-integer coercion.

6. **NULL/NA-safe joins, no sentinel values.** `NA` keys must join to `NA` keys.
   The draft's `.in_1`/`.in_2` presence columns correctly distinguish an absent
   row from a present `NA` value.

7. **One object, several derived questions.** `compare_dt()` returns a stable
   `daffiz_comparison` holding the melted table and input snapshots. Cell,
   column, row, original-row, report, and plot views are accessors over it.

8. **Columns and rows are first-class diagnostics.** Rank numeric measures by
   mismatch count/rate/magnitude; group by source indices to show affected
   rows, changed column names, and direct original-row recovery.

9. **Report first, data underneath.** A concise report leads with affected
   columns and original row indices. Every displayed count and sample is backed
   by an accessor.

10. **Add a global visual perspective.** Column bars, row-impact distributions,
    and a row/column incidence matrix should derive from testable `plot_data()`
    aggregates and label any binning or truncation.

11. **Testing helpers use the same engine.** `expect_dt_equal()` should print
    bounded column, row, and cell diagnostics without changing comparison
    semantics.

### Deliberately staged after the numeric comparison core

- **Patch and 3-way merge.** Do not delay the numeric regression comparator for
  them.
- **Content-based row alignment without keys.** Expensive and hard to explain.
- **General heterogeneous comparison.** Character, date, nested and custom
  comparators are optional extensions, not V1 requirements.
- **HTML/Shiny rendering.** This remains separate from comparison semantics.

---

## Sources

- daff — [repository](https://github.com/paulfitz/daff), [tabular diff specification](https://paulfitz.github.io/daff-doc/spec.html), [R binding](https://cran.r-project.org/web/packages/daff/), [project page](https://paulfitz.github.io/daff/)
- DataComPy — [repository](https://github.com/capitalone/datacompy), [Pandas usage docs](https://capitalone.github.io/datacompy/pandas_usage.html), [Capital One announcement](https://www.capitalone.com/tech/open-source/datacompy-open-source-dataframe-comparisons/)
- diffly — [repository](https://github.com/Quantco/diffly), [documentation](https://diffly.readthedocs.io/stable/), [QuantCo engineering blog](https://tech.quantco.com/blog/diffly)
- anofox_tabular — [repository](https://github.com/DataZooDE/anofox-tabular), [diff implementation](https://github.com/DataZooDE/anofox-tabular/blob/main/src/anofox_diff.cpp), [Python wrapper](https://github.com/DataZooDE/anofox-tabular/blob/main/python/src/anofox/diff.py), [DuckDB community extensions](https://duckdb.org/community_extensions/extensions/anofox_tabular)
- R landscape — [diffdf](https://gowerc.github.io/diffdf/latest-tag/), [arsenal::comparedf](https://mayoverse.github.io/arsenal/reference/comparedf.html), [compareDF](https://github.com/alexsanjoseph/compareDF)
