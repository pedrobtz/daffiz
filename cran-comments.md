## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The check was run on the source tarball with `R CMD check --as-cran` using
R 4.6.1 on macOS arm64.

## Method references

There are no published references describing the methods in this package. It
implements tolerance-aware numeric comparison of two tabular datasets: rows are
aligned on identity columns, and each numeric cell is classified as equal,
different, or present on only one side.

## Before submitting

These must be true at submission time or the URL check will report 404s:

- [ ] `https://github.com/pedrobtz/daffiz` is **public** (it is private during
      development, which makes every URL below fail for CRAN's checker)
- [ ] `benchmarks/` is committed and pushed to `main`, so the README's
      benchmark-validation link resolves
- [ ] the pkgdown workflow has deployed, so
      `https://pedrobtz.github.io/daffiz/` and its article link resolve

Re-run `urlchecker::url_check()` once those are done.
