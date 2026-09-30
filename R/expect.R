# The testthat expectation ------------------------------------------------------

#' Expect a table to match another
#'
#' A testthat expectation built on [diff_table()]: it passes when every
#' compared value is `same`. `expected` is the benchmark, as in testthat's
#' `expect_equal(object, expected)`, so the comparison is
#' `diff_table(expected, object, ...)`: `object` is cast to `expected`'s types
#' and its extra columns are dropped. Pass `mode = "equal"` to compare the two
#' as peers instead.
#'
#' Duplicate keys fail the expectation rather than being paired: pairing rows
#' by arrival order is an assumption, and an assertion should not rest on one.
#' The exception is a comparison with no measures, where the repeated rows are
#' identical in every compared column and pairing them assumes nothing. Pass
#' `duplicates = "disambiguate"` or `"aggregate"` to accept duplicates.
#'
#' @param object The table under test.
#' @param expected The benchmark table.
#' @param ... Passed to [diff_table()]: `mode`, `by`, `measures`,
#'   `tolerance`, `duplicates` and so on.
#' @param max_rows The most records that differ shown in a failure.
#' @return The [diff_table()] result, invisibly. On failure it is also
#'   attached to the failed expectation, as its `"diff"` attribute.
#' @export
#' @examplesIf requireNamespace("testthat", quietly = TRUE)
#' expected <- data.frame(id = 1:3, amount = c(10, 20, 30))
#' actual <- data.frame(id = 1:3, amount = c(10, 20, 30 + 1e-9))
#' expect_table_equal(actual, expected, tolerance = 1e-6)
expect_table_equal <- function(object, expected, ..., max_rows = 10L) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    daffiz_abort(
      "daffiz_error_dependency",
      "expect_table_equal() requires the suggested package 'testthat'."
    )
  }
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) || max_rows < 0) {
    daffiz_abort("daffiz_error_input", "`max_rows` must be a single number >= 0")
  }
  object_label <- deparse1(substitute(object))
  expected_label <- deparse1(substitute(expected))

  dots <- list(...)
  # diff_table() warns exactly when it pairs duplicate rows of measures, so
  # that warning is the failure: an assertion must not decide identity by
  # arrival order. Without measures it does not warn, and pairing goes ahead.
  refuse_pairing <- is.null(dots$duplicates)
  d <- withCallingHandlers(
    do.call(diff_table, c(list(expected, object), dots)),
    daffiz_warning_duplicates = function(w) {
      if (refuse_pairing) {
        daffiz_abort(
          "daffiz_error_duplicates",
          paste0(
            conditionMessage(w),
            "\nexpect_table_equal() does not pair duplicate keys; pass ",
            "duplicates = \"disambiguate\" or \"aggregate\" to accept them."
          ),
          duplicates = w$duplicates
        )
      }
    }
  )

  if (all(d$status == "same")) {
    testthat::succeed()
    return(invisible(d))
  }

  off <- d[status != "same"]
  shown <- utils::head(off, max_rows)
  msg <- paste0(
    "`", object_label, "` does not match `", expected_label, "`: ",
    format(nrow(off), big.mark = ","), " of ", format(nrow(d), big.mark = ","),
    " records differ.\n\n",
    paste(utils::capture.output(print(diff_summary(d))), collapse = "\n"),
    if (nrow(shown)) {
      paste0(
        "\n\n",
        if (nrow(off) > nrow(shown)) sprintf("First %d records that differ:\n", nrow(shown)),
        paste(utils::capture.output(print(shown)), collapse = "\n")
      )
    }
  )
  testthat::expectation("failure", msg, diff = d)
  invisible(d)
}
