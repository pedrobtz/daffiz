# Duplicate identities --------------------------------------------------------
#
# Design plan section 4.4. Duplicate groups are always reported; what happens
# next is governed by `duplicate_keys`.

# Counts per identity group on both sides, including unambiguous groups. The
# full table also gives the exact melted-record count for the size projection.
identity_counts <- function(wx, wy, by) {
  cx <- wx[, list(n_x = .N), by = by]
  cy <- wy[, list(n_y = .N), by = by]
  both <- merge(cx, cy, by = by, all = TRUE)
  both[is.na(n_x), n_x := 0L]
  both[is.na(n_y), n_y := 0L]
  both
}

apply_duplicate_policy <- function(wx, wy, by, compare, policy,
                                   x_name, y_name, counts) {
  info <- counts[n_x > 1L | n_y > 1L]

  if (!nrow(info)) {
    return(list(x = wx, y = wy, info = info, occurrence = FALSE,
                n_records = nrow(counts)))
  }

  n_groups <- nrow(info)
  affected <- sum(info$n_x) + sum(info$n_y)
  headline <- sprintf(
    "%d identity group(s) are duplicated (%d row(s) across `%s` and `%s`).",
    n_groups, affected, x_name, y_name
  )

  if (policy == "error") {
    daffiz_abort("daffiz_error_duplicates",
      paste0(headline,
             "\n  Identity: ", fmt_names(by),
             "\n  Add columns to `by=`, or set duplicate_keys = \"pair\"."),
      groups = info, by = by)
  }

  if (policy == "report") {
    keys <- unique(info[, by, with = FALSE])
    wx <- wx[!keys, on = by]
    wy <- wy[!keys, on = by]
    daffiz_warn("daffiz_warning_duplicates",
      paste0(headline,
             "\n  duplicate_keys = \"report\": these rows are reported but ",
             "excluded from cell comparison."),
      groups = info, by = by)
    # Every surviving group has at most one row per side.
    return(list(x = wx, y = wy, info = info, occurrence = FALSE,
                n_records = nrow(counts) - nrow(info)))
  }

  # policy == "pair". Sort on the comparison data, using the original row
  # position only as the final tie-breaker -- the row-id column must never
  # become a leading sort key (design plan section 4.4).
  #
  # Measures are sorted by name in the C locale first, so the pairing does not
  # depend on the order the caller happened to list `compare=`, and does not
  # shift with the session locale. Both sides must be deep copies by now:
  # `setorderv()` permutes column vectors in place.
  measure_order <- sort(compare, method = "radix")
  wx <- copy(wx)
  wy <- copy(wy)
  setorderv(wx, c(by, measure_order, ".row_x"))
  setorderv(wy, c(by, measure_order, ".row_y"))
  wx[, .occurrence := seq_len(.N), by = by]
  wy[, .occurrence := seq_len(.N), by = by]

  daffiz_warn("daffiz_warning_duplicates",
    paste0(headline,
           "\n  duplicate_keys = \"pair\": rows were paired by sorted ",
           "position within each group. This pairing is heuristic; ",
           "`.row_x`/`.row_y` record which source rows were paired."),
    groups = info, by = by)

  list(x = wx, y = wy, info = info, occurrence = TRUE,
       n_records = sum(pmax(counts$n_x, counts$n_y)))
}
