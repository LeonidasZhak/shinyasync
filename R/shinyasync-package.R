#' @keywords internal
"_PACKAGE"

`%||%` <- function(x, y) if (is.null(x)) y else x
