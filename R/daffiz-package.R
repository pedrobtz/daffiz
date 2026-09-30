#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom utils head
NULL

.datatable.aware <- TRUE

# Columns referenced inside data.table's `[`, where R CMD check cannot see
# that they resolve to columns rather than to globals.
utils::globalVariables(c(
  ".", "colname", "column", "from", "metric", "n_rows", "n_x", "n_y", "name", "status", "type",
  "type_match", "type_x", "type_y", "value_key_x", "value_key_y",
  "value_x", "value_y"
))
