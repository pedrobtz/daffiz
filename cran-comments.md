## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local: macOS arm64, R 4.6.1, `R CMD check --as-cran` on the source tarball.
* GitHub Actions (via the reusable `r-cmd-check.yml` workflow of
  pedrobtz/r-actions), all with `--as-cran`:
  * macOS (latest), Windows (latest) and Ubuntu (latest): R release;
  * Ubuntu (latest): R oldrel-1;
  * R-hub containers `clang23`, `ubuntu-clang` and `ubuntu-gcc16` (R-devel,
    CRAN's compilers);
  * R-hub `nosuggests` container: R-devel with only the hard dependencies
    installed.

## Method references

There are no published references describing the methods in this package. It
compares two tabular datasets value by value: rows are aligned on identity
columns, and each numeric value is classified as the same (within a
tolerance), changed, or present on only one side.
