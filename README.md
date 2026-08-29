# daffiz

`daffiz` is an early-stage R package for explainable numeric regression
comparisons between near-identical data frames. It uses a melted `data.table`
of numeric cells as its comparison core, with tolerance-aware differences,
column and row diagnostics, original-row recovery, optional overview plots,
and testthat assertions.

The current code is a proof of concept and is not API-stable. Start with the
[design and implementation plan](docs/design-plan.md); the supporting tool
review is in [state of the art](docs/state-of-the-art.md).
