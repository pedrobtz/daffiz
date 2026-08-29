# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`daffiz` is an early-stage R package building a more complete and flexible
data.frame/data.table comparison tool. Only a first draft exists; nothing is
API-stable and everything in `R/` is open to being rewritten.

**Read [docs/design-plan.md](docs/design-plan.md) before changing the comparison
engine.** It is the authoritative architecture, result contract, semantic
policy, test strategy, and phased roadmap. [docs/state-of-the-art.md](docs/state-of-the-art.md)
contains the primary-tool review, and [docs/features.md](docs/features.md) is a
historical extraction checklist; neither overrides the design plan.

## Current state — known gaps

The package skeleton is an unmodified `usethis` template. Before it can be
built or checked, these need to be dealt with:

- `DESCRIPTION` still contains placeholder Title/Description/Authors/License.
- `NAMESPACE` is empty and there are no roxygen blocks on any function, so
  nothing is exported.
- **No dependencies are declared.** `compare_dt` uses `data.table`
  (`copy`, `set`, `melt`, `fcase`, `setorderv`, `uniqueN`, `.N`, `:=`) and
  `create_fake_dt` uses `charlatan` (`ch_name`, `ch_job`, `ch_company`,
  `ch_phone_number`, `ch_color_name`, `ch_hex_color`) — neither is in `Imports`
  and neither is imported in `NAMESPACE`. Nothing currently runs without
  `library(data.table)` already attached.
- The two files in `R/` are named `compare_dt 2.R` and `create_fake_dt 2.R`.
  The ` 2` is a macOS duplicate-file artifact, and the space makes them awkward
  to reference. Rename to `compare_dt.R` / `create_fake_dt.R` when touching them.
- There are no tests (`tests/` does not exist) and no `man/`.

## Commands

Standard `devtools` workflow, run from the package root:

```r
devtools::load_all()      # load without installing
devtools::document()      # regenerate NAMESPACE + man/ from roxygen
devtools::test()          # run testthat suite (once tests/ exists)
devtools::check()         # full R CMD check
```

Run a single test file or a single test:

```r
devtools::test(filter = "compare")            # runs tests/testthat/test-compare*.R
testthat::test_file("tests/testthat/test-compare.R")
```

From the shell:

```sh
R -q -e 'devtools::check()'
R CMD build . && R CMD check daffiz_*.tar.gz
```

## Architecture of the current draft

`compare_dt(dt1, dt2, exclude, abs_tol, rel_tol)` is a single function running a
fixed pipeline. Understanding it matters because the design keeps this pipeline
and refactors it into validated, testable stages:

1. Drop `exclude`d columns, warning about names found in neither table.
2. Require identical column *sets* (order-insensitive) — `stop()`s otherwise.
3. Reconcile types: integer/double mismatches are coerced toward integer;
   anything else `stop()`s.
4. **Column roles by type**: the draft makes every non-`double` column an id var
   and every `double` column a measure. The design retains the intent for
   near-identical numeric tables, narrows the default measure rule to unclassed
   doubles (`Date` and `POSIXct` are also double underneath), and adds explicit
   `by=` and `compare=` overrides.
5. Duplicate keys are detected, warned about, then resolved by sorting on all
   columns and stamping a within-group `.row_key` that joins the key.
6. `melt` both tables to long form, tag each with `.in_1` / `.in_2` presence
   columns (so a missing *row* is distinguishable from an `NA` *value* after the
   outer join), full-join on key + `metric`, and classify each cell via `fcase`
   into `match_type` ∈ {`dt1_only`, `dt2_only`, `equal`, `diff`}.
   Tolerance rule: `abs(diff) <= pmax(rel_tol * pmax(abs(v1), abs(v2)), abs_tol)`.
7. Return an eager list of six frames: `dt1_only`, `dt2_only`, `diff`, `all`,
   plus `dt1_only_profile` / `dt2_only_profile` — key columns of the unmatched
   rows ranked by ascending distinct-value count, i.e. "which key column best
   characterises the rows that failed to match".

The design preserves this melted numeric comparison pipeline. It also keeps the
`.in_1`/`.in_2` presence-column technique and key-profile ranking, while adding
original input row indices before sorting/melting so every difference can be
traced back to the source rows.

`create_fake_dt(n, seed)` generates a wide fixture — 10 character, 1 date, 3
integer, 1 logical and 5 double columns — for exercising the type-dispatch paths.

## Conventions

- `data.table` is the engine. Express comparisons as joins and grouped
  operations; avoid row-wise R loops. Copy inputs (`copy()`) rather than
  modifying a caller's table by reference.
- Preserve the canonical melted numeric cell table; build column, row,
  original-row, report, and visualization accessors from it.
- Duplicate identities may use deterministic occurrence pairing by default,
  but the result must report the heuristic and retain both original row indices.
- Invalid calls (missing requested columns, incompatible join types, invalid
  tolerances) should raise clearly — see the design plan.
- `docs/`, `CLAUDE.md` and `.claude/` are listed in `.Rbuildignore`.
