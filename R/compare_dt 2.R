compare_dt <- function(dt1, dt2, exclude = NULL, abs_tol = 0, rel_tol = 0) {
  # drop excluded columns upfront before any checks
  if (length(exclude)) {
    unknown <- setdiff(exclude, union(names(dt1), names(dt2)))
    if (length(unknown))
      warning("Excluded columns not found in either table: ", paste(unknown, collapse = ", "))
    dt1 <- dt1[, setdiff(names(dt1), exclude), with = FALSE]
    dt2 <- dt2[, setdiff(names(dt2), exclude), with = FALSE]
  }

  # Step 1: same columns, order-insensitive
  only_1 <- setdiff(names(dt1), names(dt2))
  only_2 <- setdiff(names(dt2), names(dt1))
  if (length(only_1) || length(only_2)) {
    msg <- "Column mismatch:"
    if (length(only_1)) msg <- paste0(msg, "\n  only in dt1: ", paste(only_1, collapse = ", "))
    if (length(only_2)) msg <- paste0(msg, "\n  only in dt2: ", paste(only_2, collapse = ", "))
    stop(msg)
  }

  # align column order and work on copies
  dt1 <- copy(dt1)
  dt2 <- copy(dt2)[, names(dt1), with = FALSE]

  # Step 2: type check; coerce numeric -> integer when the other side is integer
  for (col in names(dt1)) {
    t1 <- typeof(dt1[[col]])
    t2 <- typeof(dt2[[col]])
    if (t1 == t2) next

    int_dbl <- c("integer", "double")
    if (t1 %in% int_dbl && t2 %in% int_dbl) {
      # integer wins: coerce the double side
      if (t1 == "double") set(dt1, j = col, value = as.integer(dt1[[col]]))
      else                set(dt2, j = col, value = as.integer(dt2[[col]]))
    } else {
      stop(sprintf(
        "Column '%s': incompatible types '%s' (dt1) vs '%s' (dt2)",
        col, t1, t2
      ))
    }
  }

  # Step 3: melt on all non-double columns as id vars (integers are merge keys)
  id_vars      <- names(dt1)[!vapply(dt1, is.double, logical(1))]
  measure_vars <- names(dt1)[ vapply(dt1, is.double, logical(1))]

  if (!length(measure_vars)) stop("No numeric columns to compare.")

  # duplicate key check: warn and resolve by sorting all columns + adding a within-group row key
  warn_dupes <- function(dt, label) {
    dupes <- dt[, .N, by = id_vars][N > 1]
    if (!nrow(dupes)) return(FALSE)
    warning(sprintf(
      "%s has %d duplicate key combination(s), e.g.:\n%s",
      label, nrow(dupes), paste(capture.output(print(head(dupes, 5))), collapse = "\n")
    ))
    TRUE
  }
  has_dupes <- warn_dupes(dt1, "dt1") | warn_dupes(dt2, "dt2")

  if (has_dupes) {
    # sort by all columns for deterministic within-group ordering, then stamp a row key
    setorderv(dt1, names(dt1))
    setorderv(dt2, names(dt2))
    dt1[, .row_key := seq_len(.N), by = id_vars]
    dt2[, .row_key := seq_len(.N), by = id_vars]
    id_vars <- c(id_vars, ".row_key")
  }

  m1 <- melt(dt1, id.vars = id_vars, measure.vars = measure_vars,
              variable.name = "metric", value.name = "value_1",
              variable.factor = FALSE)
  m2 <- melt(dt2, id.vars = id_vars, measure.vars = measure_vars,
              variable.name = "metric", value.name = "value_2",
              variable.factor = FALSE)

  # sentinel columns to distinguish missing rows from NA values after outer join
  m1[, .in_1 := TRUE]
  m2[, .in_2 := TRUE]

  # Step 4: merge and compute difference
  result <- merge(m1, m2, by = c(id_vars, "metric"), all = TRUE)
  result[, diff := value_1 - value_2]
  result[, match_type := fcase(
    is.na(.in_1),                             "dt2_only",
    is.na(.in_2),                             "dt1_only",
    is.na(value_1) & is.na(value_2),          "equal",    # both NA in source
    is.na(value_1) | is.na(value_2),          "diff",     # one NA, one not
    abs(diff) <= pmax(rel_tol * pmax(abs(value_1), abs(value_2)), abs_tol), "equal",
    default =                                 "diff"
  )]
  result[, c(".in_1", ".in_2") := NULL]

  # profile id columns of unmatched rows by n_distinct (ascending = most characterizing)
  key_profile <- function(rows) {
    if (!nrow(rows)) return(data.table(column = character(), n_distinct = integer(), values = list()))
    uniq <- unique(rows[, id_vars, with = FALSE])
    data.table(
      column    = id_vars,
      n_distinct = vapply(id_vars, function(col) uniqueN(uniq[[col]]), integer(1)),
      values    = lapply(id_vars, function(col) sort(unique(uniq[[col]])))
    )[order(n_distinct)]
  }

  dt1_only_rows <- result[match_type == "dt1_only"]
  dt2_only_rows <- result[match_type == "dt2_only"]

  list(
    dt1_only         = dt1_only_rows[, !c("match_type")],
    dt1_only_profile = key_profile(dt1_only_rows),
    dt2_only         = dt2_only_rows[, !c("match_type")],
    dt2_only_profile = key_profile(dt2_only_rows),
    diff             = result[match_type == "diff", !c("match_type")],
    all              = result
  )
}
