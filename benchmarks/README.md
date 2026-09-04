# Performance benchmarks

These benchmarks exercise the package's actual canonical long-table workload
in fresh R processes. Each completed case records comparison construction time,
column- and row-summary time, in-memory object sizes, and the high-water mark
of R-managed heap cells.

Run the bounded validation profile from the repository root:

```sh
Rscript benchmarks/run-benchmarks.R .
```

It is capped at five million materialized cells by default. The full design
matrix includes 1,000, 100,000, and 1,000,000 rows; 5, 20, and 100 measures;
0%, 0.1%, 1%, and 20% changed cells; and unique and duplicated identities. It
can include 100-million-cell cases and must be enabled deliberately:

```sh
DAFFIZ_BENCH_PROFILE=full \
DAFFIZ_BENCH_MAX_CELLS=100000000 \
Rscript benchmarks/run-benchmarks.R .
```

`construction_peak_r_heap_bytes` is an R allocator high-water estimate, not
operating-system resident set size. It includes the two input fixtures and the
comparison object. `cells_bytes` is the measured size of the retained canonical
cell table. Both make extrapolations and their limitations explicit.

The checked-in validation report records the hardware, package revision state,
raw results, and the decision about the default warning threshold.
