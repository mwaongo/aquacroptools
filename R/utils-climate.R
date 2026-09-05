#' Generic Climate File Writer
#'
#' @description
#' Internal helper function that writes AquaCrop climate files (.PLU, .ETo, .Tnx).
#' This consolidates the common logic shared by `write_plu()`, `write_eto()`, and
#' `write_tnx()`, eliminating code duplication while maintaining consistent behavior.
#'
#' @param path Directory path where the output file will be written.
#' @param site_name Station name or identifier for the weather station.
#'   If `NULL`, extracted from data if available, otherwise defaults to "station".
#' @param data Data frame containing the climate data with columns:
#'   year, month, day, and the variable(s) specified by `var_cols`.
#' @param var_cols Character vector of column names to include in output.
#'   For single-variable files (rain, et0): `c("rain")` or `c("et0")`.
#'   For temperature files: `c("tmin", "tmax")`.
#' @param header_var_name Name used in header generation (e.g., "rain", "et0", "temperature").
#' @param file_ext File extension including the dot (e.g., ".PLU", ".ETo", ".Tnx").
#' @param syear Start year of the data period. If `NULL`, extracted from data.
#' @param eyear End year of the data period. If `NULL`, extracted from data.
#' @param eol End-of-line character style. Options: "windows", "linux", or "macos".
#'   If `NULL`, auto-detected based on OS.
#' @param record_type Type of temporal aggregation:
#'   - 1 = daily (default)
#'   - 2 = 10-daily
#'   - 3 = monthly
#' @param first_day First day of record. For 10-daily: 1, 11, or 21; For monthly: 1.
#' @param first_month First month of record (1-12).
#' @param col_widths Numeric vector of column widths for output. If single value,
#'   applied to all columns. Default: 10.
#'
#' @return Invisibly returns the full path to the created file.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates the input data frame has required columns
#' 2. Extracts station name and year range if not provided
#' 3. Creates the output directory if it doesn't exist
#' 4. Generates the appropriate AquaCrop header
#' 5. Writes the header and data to a fixed-width format file
#'
#' @seealso
#' \code{\link{write_plu}} for rainfall files,
#' \code{\link{write_eto}} for evapotranspiration files,
#' \code{\link{write_tnx}} for temperature files
#'
#' @keywords internal
#' @noRd
.write_climate_file <- function(
    path,
    site_name,
    data,
    var_cols,
    header_var_name,
    file_ext,
    syear = NULL,
    eyear = NULL,
    eol = NULL,
    record_type = 1,
    first_day = 1,
    first_month = 1,
    col_widths = 10) {

  # Validate data is a data frame
  if (is.null(data) || !is.data.frame(data)) {
    stop(
      "data must be a data frame with columns: year, month, day, ",
      paste(var_cols, collapse = ", "),
      "\nReceived: ", class(data)[1],
      call. = FALSE
    )
  }

  # Validate required columns (year, month, day + var_cols)
  required_cols <- c("year", "month", "day", var_cols)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nRequired columns are: ", paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Extract station name from data if not provided
  site_name <- .extract_station(site_name, data)

  # Extract year range from data if not provided
  years <- .extract_years(data, syear, eyear)
  syear <- years$syear
  eyear <- years$eyear

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist. Checked with base::dir.exists()
  # rather than the fs equivalents: both fs::dir_create() and fs::dir_exists()
  # re-derive the platform on every call, which dominates batch runs that write
  # files for hundreds of stations.
  if (!dir.exists(path)) fs::dir_create(path, recurse = TRUE)

  # Use station name as-is for filename

  # Get header
  header <- .get_header(
    var_name = header_var_name,
    site_name = site_name,
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month
  )

  # Prepare data for output (select only the variable columns)
  out_data <- data[var_cols]

  # Format the data rows before opening the file, so a formatting failure
  # leaves no half-written file behind.
  body <- .fwf_format(out_data, width = col_widths, justify = "r")
  sep <- .get_eol(eol = eol)

  # Write header and data through a single connection. The header already
  # carries its own line endings, so it is written verbatim.
  output_file <- paste0(path, site_name, file_ext)

  con <- base::file(output_file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(as.character(header)), con, sep = "", useBytes = TRUE)
  writeLines(enc2utf8(body), con, sep = sep, useBytes = TRUE)

  # Return invisibly with file path
  invisible(output_file)
}
