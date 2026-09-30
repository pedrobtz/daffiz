# Structured conditions -------------------------------------------------------
#
# Every condition daffiz raises carries a subclass as well as readable text, so
# callers and tests can tell which check fired without parsing a message.
#
# The three kinds mean different things:
# - an error: the question cannot be answered as asked;
# - a warning: the answer may be wrong (identity was invented);
# - a message: an expected consequence of the settings the caller chose
#   (values truncated by a cast, columns dropped or ignored).

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

# message() prints conditionMessage() as is, so the newline is ours to add.
daffiz_inform <- function(subclass, message, ...) {
  cond <- structure(
    class = c(subclass, "daffiz_message", "message", "condition"),
    list(message = paste0(message, "\n"), call = NULL, ...)
  )
  message(cond)
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
