# Structured conditions -------------------------------------------------------
#
# Every preflight gate raises a subclassed condition carrying structured fields
# as well as readable text, so tests can verify which gate failed without
# parsing an error message (design plan section 4.1).

daffiz_abort <- function(subclass, message, ...) {
  cond <- structure(
    class = c(subclass, "daffiz_error", "error", "condition"),
    list(message = message, call = NULL, ...)
  )
  stop(cond)
}

daffiz_warn <- function(subclass, message, ...) {
  cond <- structure(
    class = c(subclass, "daffiz_warning", "warning", "condition"),
    list(message = message, call = NULL, ...)
  )
  warning(cond)
}

# Formats a character vector for inclusion in a message, bounded so that a
# comparison of a very wide table still produces a readable error.
fmt_names <- function(x, max_n = 10L) {
  if (!length(x)) return("<none>")
  shown <- utils::head(x, max_n)
  out <- paste(shown, collapse = ", ")
  if (length(x) > max_n) {
    out <- paste0(out, ", ... (", length(x) - max_n, " more)")
  }
  out
}
