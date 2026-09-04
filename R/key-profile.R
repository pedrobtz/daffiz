# Unmatched-row identity profiles --------------------------------------------

build_key_profile <- function(cmp, side) {
  match_type <- paste0(side, "_only")
  by <- cmp$settings$by
  cells <- cmp$cells[.match_type == match_type]
  if (!nrow(cells)) {
    return(data.table(
      column = character(),
      n_distinct = integer(),
      values = vector("list", 0L)
    ))
  }

  rows <- unique(cells[, by, with = FALSE])
  out <- data.table(
    column = by,
    n_distinct = vapply(by, function(nm) uniqueN(rows[[nm]]), integer(1L)),
    # `na.last = TRUE` keeps missing keys: `sort()` drops them by default,
    # which made `length(values)` disagree with `n_distinct` and hid an NA
    # identity -- one of the commonest reasons rows fail to align at all.
    values = lapply(by, function(nm) sort(unique(rows[[nm]]), na.last = TRUE))
  )
  out[, .identity_order := seq_len(.N)]
  setorderv(out, c("n_distinct", ".identity_order"))
  out[, .identity_order := NULL]
  out[]
}

#' Profile identity columns among unmatched rows
#'
#' Ranks identity columns by their number of distinct values among side-only
#' records. Low-cardinality columns can help explain concentrations of rows
#' that failed to align.
#'
#' @param cmp A `daffiz_comparison` from [compare_dt()].
#' @param side Either `"x"` or `"y"`.
#'
#' @return A `data.table` with `column`, `n_distinct`, and a list column of
#'   observed `values`, sorted by distinct count.
#' @export
#' @examples
#' x <- data.frame(group = c("a", "b"), id = 1:2, amount = c(10, 20))
#' y <- data.frame(group = c("a", "c"), id = 1:2, amount = c(10, 20))
#' cmp <- compare_dt(x, y)
#' key_profile(cmp, "x")
key_profile <- function(cmp, side = c("x", "y")) {
  validate_comparison(cmp)
  side <- validate_side(side)
  out <- cached_comparison_value(
    cmp,
    paste0("key_profile_", side),
    function() build_key_profile(cmp, side)
  )
  copy(out)
}
