# Cell-warning threshold validation

Date: 2026-09-01

## Decision

Lower the default warning threshold from 50 million to **10 million projected
cells**. The threshold remains advisory and configurable through
`options(daffiz.max_cells = ...)`.

The prior 50-million value correctly approximated the retained cell table at
64 bytes per cell, but did not account for construction intermediates. On the
benchmark machine, a 50-million-cell comparison projects to:

- 3.20 GB for the retained canonical cell table;
- about 15.6 GB of R-managed construction heap without batching;
- about 9.7 GB with `batch = 5`.

That makes 50 million an unsafe point at which to *start* warning on a 16 GB
machine. At 10 million cells the corresponding projections are 640 MB retained,
about 3.1 GB unbatched, and about 1.9 GB with `batch = 5`. A warning at this
point is early enough to recommend column narrowing or batching while remaining
non-blocking.

## Environment

- macOS 26.5.2, arm64
- Apple M1, 8 logical CPUs, 16 GB physical memory
- R 4.6.1
- data.table 1.18.4 using 4 threads

## Method

`benchmarks/run-benchmarks.R` ran each case in a fresh R process. The fixtures
exercise `compare_dt()`, `column_summary()`, and `row_summary()` using the real
long-table pipeline. The recorded high-water value estimates memory managed by
R's Ncell and Vcell heaps; it is not operating-system resident set size.

The bounded validation profile covered 5,000 to 5,000,000 projected cells,
0% to 20% changed cells, unique and duplicated identities, and batched and
unbatched construction. The raw results are in
`benchmark-2026-09-01.csv`.

## Observations

- At one million cells and above, unique-key retained tables used 64.001 to
  64.005 bytes per cell. This validates the 64-byte retained-size estimate in
  the warning.
- Duplicated pairing added an occurrence column and used 68.005 bytes per cell
  in the measured one-million-cell case, a 6.25% increase.
- A 5-million-cell comparison completed in 2.47 seconds unbatched and 2.40
  seconds with `batch = 5` on this machine.
- At five million cells, batching reduced measured construction high-water heap
  from 1.56 GB to 0.97 GB (37.8%) without changing the 320 MB retained result.
- Change rate did not materially affect construction: one-million-cell cases at
  0% and 20% differences took 0.616 and 0.619 seconds and had effectively the
  same high-water heap.
- At one million cells, duplicated identities increased construction time from
  0.62 to 0.75 seconds and high-water heap from 364 MB to 377 MB.

## Interpretation limits

The 10-million threshold is a warning, not a memory guarantee. Peak memory
depends on R, `data.table`, input column types, duplicated identities, allocator
behavior, and the chosen batch size. The full benchmark matrix includes cases
up to 100 million cells but is safety-capped by default; it should only be run
on a deliberately sized machine.
