#' Write Fixed-Width Format File
#'
#' @description
#' Write data to a fixed-width format (FWF) file where each column has a specified
#' character width. This function creates properly aligned text files commonly used
#' for data exchange with legacy systems and specific file format requirements like
#' AquaCrop climate files.
#'
#' @param x Data frame to write to file. All columns will be converted to character format.
#'   Factor columns are automatically converted to character before writing.
#' @param file Character string specifying the output file path. Can be a relative or
#'   absolute path.
#' @param width Numeric value or vector specifying the character width for each column:
#'   \itemize{
#'     \item Single value: All columns use the same width (e.g., \code{width = 10})
#'     \item Vector: Each column uses its corresponding width (e.g., \code{width = c(10, 15, 8)})
#'   }
#'   If a single value is provided for a multi-column data frame, it is recycled for all columns.
#' @param justify Character string specifying text alignment within each column width:
#'   \itemize{
#'     \item \code{"l"}: Left-align all columns
#'     \item \code{"r"}: Right-align all columns
#'     \item Multi-character string: Align each column individually (e.g., \code{"lrl"} for
#'       left, right, left alignment of three columns)
#'   }
#'   Default = \code{"l"}. If a single character is provided for a multi-column data frame,
#'   it is recycled for all columns.
#' @param replace_na Character string to use in place of NA values. Default = \code{"NA"}
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#'   \itemize{
#'     \item \code{"windows"}: Windows-style line endings (\\r\\n)
#'     \item \code{"unix"}, \code{"linux"}, \code{"macOS"}: Unix-style line endings (\\n)
#'   }
#'   Default = \code{"windows"}
#' @param append Logical. If \code{TRUE}, append to existing file. If \code{FALSE}, overwrite
#'   existing file. Default = \code{TRUE}
#'
#' @details
#' This function provides a lightweight solution for writing fixed-width format files without
#' additional dependencies beyond base R and readr. It is particularly useful for:
#' \itemize{
#'   \item Creating AquaCrop climate data files (.PLU, .ETo, .Tnx)
#'   \item Generating data files for legacy systems
#'   \item Producing human-readable aligned text output
#' }
#'
#' ## Column Alignment:
#' Text is aligned within the specified width using \code{sprintf} formatting:
#' \itemize{
#'   \item Left-aligned (\code{"l"}): Text starts at the left edge, padded on the right
#'   \item Right-aligned (\code{"r"}): Text ends at the right edge, padded on the left
#' }
#'
#' ## Data Conversion:
#' \itemize{
#'   \item Factor columns are automatically converted to character
#'   \item NA values are replaced with the string specified in \code{replace_na}
#'   \item All data is formatted as character strings with specified widths
#' }
#'
#' ## File Writing:
#' \itemize{
#'   \item By default, data is appended to existing files (\code{append = TRUE})
#'   \item Use \code{append = FALSE} to overwrite existing files
#'   \item Column names are not included in the output
#' }
#'
#' @return
#' Invisibly returns \code{NULL}. The function is called for its side effect of
#' writing data to a file.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   year = c(2020, 2021, 2022),
#'   rainfall = c(850.5, 920.3, 780.1),
#'   temp = c(25.2, 26.1, 24.8)
#' )
#'
#' # Write with uniform width, left-aligned
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "l",
#'   append = FALSE
#' )
#'
#' # Write with different widths per column, right-aligned
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = c(6, 10, 10),
#'   justify = "r",
#'   append = FALSE
#' )
#'
#' # Write with mixed alignment (left, right, right)
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = c(6, 10, 10),
#'   justify = "lrr",
#'   append = FALSE
#' )
#'
#' # Append to existing file
#' write_fwf(
#'   x = df[1:2, ],
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   append = TRUE
#' )
#'
#' # Handle NA values with custom replacement
#' df_na <- data.frame(
#'   year = c(2020, 2021, NA),
#'   value = c(100, NA, 150)
#' )
#'
#' write_fwf(
#'   x = df_na,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   replace_na = "-9999",
#'   append = FALSE
#' )
#'
#' # Unix-style line endings
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   eol = "unix",
#'   append = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}} for
#' functions that use \code{write_fwf} to create AquaCrop climate files
#'
#' @export
write_fwf <- function(x, file, width,
                      justify = "l",
                      replace_na = "NA",
                      eol = NULL, append = TRUE) {
  tbl_content <- .fwf_format(x, width = width, justify = justify,
                             replace_na = replace_na)

  sep <- .get_eol(eol = eol)

  # Write to file. readr::write_lines() carries enough per-call overhead to
  # dominate batch runs, so use a single base connection instead.
  con <- base::file(file, open = if (isTRUE(append)) "ab" else "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(tbl_content), con, sep = sep, useBytes = TRUE)

  invisible(NULL)
}


#' Format a Data Frame as Fixed-Width Lines
#'
#' @description
#' Formatting half of [write_fwf()], split out so that callers which already
#' hold an open connection (see `.write_climate_file()`) can write a header and
#' the data rows without reopening the file.
#'
#' @inheritParams write_fwf
#' @return A character vector, one element per row of `x`, with no line endings.
#' @keywords internal
#' @noRd
.fwf_format <- function(x, width, justify = "l", replace_na = "NA") {
  # Work on a plain list of columns: tibbles reject the character NA
  # replacement below, and sprintf() only ever sees the columns anyway.
  cols <- as.list(x)
  n_col <- length(cols)

  # Convert factors to character and replace NA values, column by column so
  # that the result is independent of x's data frame class.
  cols <- lapply(cols, function(col) {
    if (is.factor(col)) col <- as.character(col)
    if (anyNA(col)) col <- replace(as.character(col), is.na(col), replace_na)
    col
  })

  # Process justify parameter
  justify <- unlist(strsplit(justify, ""))
  justify <- as.character(factor(justify, c("l", "r"), c("-", "")))

  # Recycle width and justify if needed
  if (n_col != 1) {
    if (length(width) == 1) width <- rep(width, n_col)
    if (length(justify) == 1) justify <- rep(justify, n_col)
  }

  # Build sprintf format string
  sptf_fmt <- paste0(
    paste0("%", justify, width, "s"),
    collapse = ""
  )

  tbl_content <- do.call(sprintf, c(list(fmt = sptf_fmt), cols))

  # A value wider than its column silently runs into the next one, producing a
  # file with no separator between the two fields. Flag it rather than emit it.
  too_wide <- which(nchar(tbl_content) > sum(width))
  if (length(too_wide) > 0) {
    warning(
      "Value(s) too wide for the requested column width(s) in ",
      length(too_wide), " row(s) (first: row ", too_wide[1], ").\n",
      "Adjacent columns will run together. Round the data or increase `width`.",
      call. = FALSE
    )
  }

  tbl_content
}
