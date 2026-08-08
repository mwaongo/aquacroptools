#' Parse AquaCrop Climate File Header
#'
#' Extracts metadata from AquaCrop climate file headers.
#'
#' @param lines Character vector. Lines read from the file.
#' @param sep_idx Integer. Index of the separator line (====).
#'
#' @return A list with components: station, record_type, first_day, first_month, first_year.
#'
#' @keywords internal
#' @noRd
.parse_climate_header <- function(lines, sep_idx) {
  header_lines <- lines[1:(sep_idx - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  list(
    station     = trimws(parts[[1]][1]),
    record_type = as.integer(trimws(parts[[2]][1])),
    first_day   = as.integer(trimws(parts[[3]][1])),
    first_month = as.integer(trimws(parts[[4]][1])),
    first_year  = as.integer(trimws(parts[[5]][1]))
  )
}


#' Generate Date Sequence for Climate Data
#'
#' Creates date components (year, month, day) based on record type and start date.
#'
#' @param header List. Parsed header from .parse_climate_header().
#' @param n_rows Integer. Number of data rows.
#'
#' @return A list with components: year, month, day (all integer vectors).
#'
#' @keywords internal
#' @noRd
.generate_dates <- function(header, n_rows) {
  by_type <- c("day", "10 days", "month")
  start_date <- as.Date(sprintf(
    "%d-%02d-%02d",
    header$first_year, header$first_month, header$first_day
  ))
  dates <- seq.Date(
    from = start_date,
    by = by_type[header$record_type],
    length.out = n_rows
  )

  list(
    year  = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    day   = as.integer(format(dates, "%d"))
  )
}


#' Read AquaCrop Climate File (Generic)
#'
#' Generic reader for AquaCrop climate files (Tnx, ETo, PLU).
#'
#' @param file Character. Path to the AquaCrop climate file.
#' @param expected_ext Character. Expected file extension.
#' @param col_names Character vector. Column names for the data values.
#' @param file_type Character. Human-readable file type for error messages.
#'
#' @return A tibble with station, year, month, day, and value columns.
#'
#' @keywords internal
#' @noRd
.read_climate_file <- function(file, expected_ext, col_names, file_type) {
  # Validate file
  if (!.is_climate_file(file, expected_ext)) {
    stop("File is not a valid AquaCrop ", file_type, " file: ", file)
  }

  # Read file header portion
  lines <- readLines(file, n = 100, warn = FALSE)

  # Locate separator (====)
  sep_idx <- grep("^=+", lines)[1]
  if (is.na(sep_idx)) {
    stop("Cannot find separator line (====) in file: ", file)
  }

  # Parse header
  header <- .parse_climate_header(lines, sep_idx)

  # Check business rules for dates
  .check_dates_rule(header$record_type, header$first_day)

  # Read data values
  if (length(col_names) == 1) {
    # Single column (ETo, PLU)
    values <- as.numeric(readr::read_fwf(file, skip = sep_idx, show_col_types = FALSE)[[1]])
    values <- replace(values, values == -9, NA_real_)
    n_rows <- length(values)
  } else {
    # Multiple columns (Tnx)
    data <- readr::read_table(
      file,
      skip = sep_idx,
      col_names = col_names,
      show_col_types = FALSE
    )
    # Convert -9 to NA for all columns
    for (col in col_names) {
      data[[col]] <- replace(data[[col]], data[[col]] == -9, NA_real_)
    }
    n_rows <- nrow(data)
  }

  # Generate dates
  date_cols <- .generate_dates(header, n_rows)

  # Build result tibble
  if (length(col_names) == 1) {
    result <- tibble::tibble(
      station = header$station,
      year    = date_cols$year,
      month   = date_cols$month,
      day     = date_cols$day
    )
    result[[col_names]] <- values
  } else {
    result <- tibble::tibble(
      station = header$station,
      year    = date_cols$year,
      month   = date_cols$month,
      day     = date_cols$day
    )
    for (col in col_names) {
      result[[col]] <- data[[col]]
    }
  }

  result
}


#' Resolve File Path Relative to Base Directory
#'
#' Helper function to resolve file paths that may be absolute, relative, or
#' just filenames.
#'
#' @param file_path Character. Path from CLI file (may be filename, relative, or absolute).
#' @param base_dir Character. Base directory (directory containing CLI file).
#'
#' @return Character. Resolved absolute path.
#'
#' @keywords internal
#' @noRd
.resolve_path <- function(file_path, base_dir) {
  # If absolute path, return as is
  if (fs::is_absolute_path(file_path)) {
    return(file_path)
  }

  # Otherwise, treat as relative to base_dir
  return(fs::path(base_dir, file_path))
}


#' Check Consistency of first_day for record_type
#'
#' Validates that the first_day value is consistent with the record_type
#' according to AquaCrop PLU file specifications.
#'
#' @param record_type Integer. Record type: 1 = daily, 2 = 10-day, 3 = monthly.
#' @param first_day Integer. First day of the record.
#'
#' @return Logical. Returns TRUE if consistent, otherwise stops with an error.
#'
#' @details
#' Business rules:
#' \itemize{
#'   \item Monthly (record_type = 3): first_day must be 1
#'   \item 10-day (record_type = 2): first_day must be 1, 11, or 21
#'   \item Daily (record_type = 1): first_day can be any valid day (1-31)
#' }
#'
#' @keywords internal
#' @noRd
.check_dates_rule <- function(record_type, first_day) {
  if (record_type == 3 && first_day != 1) {
    stop("Monthly record_type requires first_day = 1")
  }
  if (record_type == 2 && !(first_day %in% c(1, 11, 21))) {
    stop("10-day record_type requires first_day = 1, 11, or 21")
  }
  TRUE
}
