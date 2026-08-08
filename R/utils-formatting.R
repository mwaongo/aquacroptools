#' Format String with sprintf and Justification
#'
#' @description
#' Internal helper function that formats numeric or character values using
#' sprintf format specifications and then applies fixed-width alignment.
#' This is the unified function that replaces `.format_string2`, `.format_string3`,
#' and `.format_string4`.
#'
#' @param string Numeric or character value to format
#' @param fmt Character sprintf format specification (e.g., "%.1f", "%.2f", "%.0f", "%s")
#' @param width Numeric specifying the total character width for formatted value
#' @param justify Character string specifying alignment:
#'   \itemize{
#'     \item \code{"centre"} or \code{"center"}: Center-align (default)
#'     \item \code{"left"}: Left-align
#'     \item \code{"right"}: Right-align
#'   }
#'
#' @return Character string formatted with sprintf and aligned to specified width
#'
#' @examples
#' # Center alignment (default)
#' .format_string_fmt(3.14159, "%.2f", 10)
#' # Returns: "   3.14   "
#'
#' # Left alignment
#' .format_string_fmt(3.14159, "%.2f", 10, "left")
#' # Returns: "3.14      "
#'
#' # Right alignment
#' .format_string_fmt(3.14159, "%.2f", 10, "right")
#' # Returns: "      3.14"
#'
#' # String formatting
#' .format_string_fmt("hello", "%s", 10, "centre")
#' # Returns: "  hello   "
#'
#' @seealso
#' \code{\link{write_fwf}} for writing fixed-width files
#'
#' @keywords internal
#' @noRd
.format_string_fmt <- function(string, fmt, width, justify = "centre") {
  x <- sprintf(fmt, string)
  base::format(x, width = width, justify = justify)
}


# Justification-specific shorthands, used throughout the write_*() functions
# (write_cro, write_man, write_irr, write_prm, utils-headers, ...).
# .format_string_fmt() is the underlying implementation; these are the
# primary call sites, not aliases pending removal.

#' @describeIn .format_string_fmt Center-justified formatting
#' @keywords internal
#' @noRd
.format_string2 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "centre")
}

#' @describeIn .format_string_fmt Left-justified formatting
#' @keywords internal
#' @noRd
.format_string3 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "left")
}

#' @describeIn .format_string_fmt Right-justified formatting
#' @keywords internal
#' @noRd
.format_string4 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "right")
}


#' Extract sprintf Format from String
#'
#' @description
#' Internal helper function that extracts and reformats sprintf-style format
#' specifications from parameter metadata.
#'
#' @param string Character vector containing sprintf-style format specifications
#'   (e.g., "%.1f", "%.2f", "%.0f")
#'
#' @return Character vector containing the extracted format specifications
#' @keywords internal
#' @noRd
.get_sprintf_format <- function(string) {
  s <- string %>%
    stringr::str_extract_all(pattern = ".", simplify = TRUE)
  fmt <- ""
  for (i in 2:5) {
    fmt <- paste0(fmt, c(s[, i]))
  }
  return(fmt)
}


# Note: .add_trailing_slash() has been moved to utils-paths.R
# for better organization of path-related utilities
