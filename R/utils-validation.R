#' Validate that Required Files Exist
#'
#' @description
#' Internal helper function that checks if a set of required files exist
#' and provides a clear error message listing all missing files.
#' This consolidates repetitive file existence checks used in functions
#' like `.write_prm()`.
#'
#' @param files Named character vector where names describe the file type
#'   and values are the file paths to check.
#'   Example: `c(CLI = "/path/to/file.CLI", SOL = "/path/to/file.SOL")`
#' @param stop_on_missing Logical. If `TRUE` (default), stops with an error
#'   when files are missing. If `FALSE`, returns the missing file names.
#'
#' @return If `stop_on_missing = TRUE`: invisibly returns `TRUE` if all files exist,
#'   otherwise throws an error. If `stop_on_missing = FALSE`: returns a character
#'   vector of missing file type names (empty if all exist).
#'
#' @examples
#' # Check multiple files at once
#' .validate_files_exist(c(
#'   CLI = "/path/to/station.CLI",
#'   Tnx = "/path/to/station.Tnx",
#'   ETo = "/path/to/station.ETo",
#'   PLU = "/path/to/station.PLU"
#' ))
#'
#' # Check without stopping (useful for conditional logic)
#' missing <- .validate_files_exist(
#'   c(SOL = "/path/to/file.SOL"),
#'   stop_on_missing = FALSE
#' )
#' if (length(missing) > 0) {
#'   message("Missing: ", paste(missing, collapse = ", "))
#' }
#'
#' @keywords internal
#' @noRd
.validate_files_exist <- function(files, stop_on_missing = TRUE) {
  # Find missing files
  exists_check <- file.exists(files)
  missing_names <- names(files)[!exists_check]
  missing_paths <- files[!exists_check]

  if (length(missing_names) > 0) {
    if (stop_on_missing) {
      # Build informative error message
      details <- paste0("  - ", missing_names, ": ", missing_paths, collapse = "\n")
      stop(
        "Required file(s) not found:\n",
        details,
        call. = FALSE
      )
    } else {
      return(missing_names)
    }
  }

  if (stop_on_missing) {
    invisible(TRUE)
  } else {
    character(0)
  }
}


#' Extract Station Name from Data
#'
#' @description
#' Internal helper function that extracts station name from data frame
#' or returns a default value. Used by climate writers to handle the
#' optional station parameter consistently.
#'
#' @param stn Provided station name (can be NULL)
#' @param data Data frame that may contain a "station" column
#' @param default Default station name if not found. Default: "station"
#'
#' @return Character string with the station name
#'
#' @examples
#' # Extract from data if stn is NULL
#' stn <- .extract_station(NULL, weather_df)
#'
#' # Use provided value
#' stn <- .extract_station("MyStation", weather_df)
#'
#' @keywords internal
#' @noRd
.extract_station <- function(stn, data, default = "station") {
  if (is.null(stn) && "station" %in% names(data)) {
    stn <- utils::head(data, 1) |>
      dplyr::pull("station")
  }

  if (is.null(stn)) {
    stn <- default
  }

  stn
}


#' Extract Year Range from Data
#'
#' @description
#' Internal helper function that extracts start and end years from a
#' climate data frame. Used by climate writers to determine the data
#' period when years are not explicitly provided.
#'
#' @param data Data frame containing a "year" column
#' @param syear Provided start year (can be NULL)
#' @param eyear Provided end year (can be NULL)
#'
#' @return Named list with `syear` and `eyear` values
#'
#' @examples
#' years <- .extract_years(weather_df, NULL, NULL)
#' # Returns: list(syear = 1991, eyear = 2020)
#'
#' @keywords internal
#' @noRd
.extract_years <- function(data, syear = NULL, eyear = NULL) {
  if (is.null(syear)) {
    syear <- utils::head(data, 1) |>
      dplyr::pull("year")
  }

  if (is.null(eyear)) {
    eyear <- utils::tail(data, 1) |>
      dplyr::pull("year")
  }

  list(syear = syear, eyear = eyear)
}
