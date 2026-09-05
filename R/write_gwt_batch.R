#' Write AquaCrop Groundwater Table (.GWT) Files for Multiple Stations
#'
#' Generate AquaCrop groundwater table files for multiple stations.
#'
#' @param site_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param code Integer. Groundwater table mode applied to all stations:
#'   0 = no groundwater table,
#'   1 = constant depth and salinity,
#'   2 = variable depth and salinity.
#' @param gwt_data For code 0: ignored, can be NULL.
#'   For code 1 and 2: either a single data.frame applied to all stations,
#'   or a list of data.frames (one per station). Each data.frame must have
#'   columns day, depth, and ecw. For code 1, exactly one row is required.
#' @param path Output directory path for GWT files. Default: "SOIL/".
#' @param climate_path Path to climate files directory, used for station
#'   discovery. Default: "CLIMATE/".
#' @param start_day Integer. First day of observations (code 2 only).
#'   Default: 1.
#' @param start_month Integer. First month of observations (code 2 only).
#'   Default: 1.
#' @param start_year Integer. First year of observations (code 2 only).
#'   Use 1901 if not linked to a specific year. Default: 1901.
#' @param description Character. Description written on the first line.
#'   Default: auto-generated based on code and data.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .GWT files from path
#'   before writing new files. Default: FALSE.
#'
#' @details
#' The function validates that all specified stations have corresponding
#' climate files. A single data.frame for gwt_data will be automatically
#' applied to all stations. For code 0, gwt_data is ignored entirely.
#'
#' @family batch operations
#' @return Invisibly returns NULL. The main effect is writing GWT files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' stations <- c("grid_001", "grid_002")
#'
#' # Code 0: no groundwater table for all stations
#' write_gwt_batch(
#'   site_name = stations,
#'   code         = 0
#' )
#'
#' # Code 1: same constant groundwater for all stations
#' write_gwt_batch(
#'   site_name = stations,
#'   code         = 1,
#'   gwt_data     = data.frame(day = 1, depth = 1.50, ecw = 1.5)
#' )
#'
#' # Code 2: different variable groundwater per station
#' write_gwt_batch(
#'   site_name = stations,
#'   code         = 2,
#'   gwt_data     = list(
#'     data.frame(day = c(50, 100), depth = c(1.0, 2.0), ecw = c(1.0, 2.0)),
#'     data.frame(day = c(50, 100), depth = c(1.5, 2.5), ecw = c(0.5, 1.5))
#'   ),
#'   start_year = 2000
#' )
#' }
#'
#' @seealso \code{\link{write_gwt}} for single station GWT file generation.
#'
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_gwt_batch <- function(
  site_name = NULL,
  code,
  gwt_data = NULL,
  path = "SOIL/",
  climate_path = "CLIMATE/",
  start_day = 1,
  start_month = 1,
  start_year = 1901,
  description = NULL,
  version = 7.1,
  eol = NULL,
  base_path = getwd(),
  verbose = TRUE,
  clean = FALSE
) {
  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.GWT$", verbose)
  }

  # Discover or validate stations from climate files
  site_name <- .discover_or_validate_items(
    item_names = site_name,
    climate_path = climate_path,
    base_path = base_path,
    item_type = "site",
    verbose = verbose
  )

  n <- length(site_name)
  .warn_single_item(n, "write_gwt_batch", "write_gwt", verbose)

  # Normalize gwt_data: code 0 needs no data
  if (code == 0) {
    gwt_data <- rep(list(NULL), n)
  } else {
    if (is.data.frame(gwt_data)) {
      if (verbose) {
        message("Applying the same groundwater data to all ", n, " site(s)")
      }
      gwt_data <- rep(list(gwt_data), n)
    }

    if (!is.list(gwt_data) || length(gwt_data) != n) {
      stop(
        "gwt_data must be either:\n",
        "  - A single data.frame (applied to all stations), or\n",
        "  - A list of data.frames with length matching site_name\n",
        "Expected length: ",
        n,
        ", got: ",
        length(gwt_data),
        call. = FALSE
      )
    }
  }

  if (verbose) {
    message("Writing GWT files for ", n, " site(s)...")
  }

  .batch_with_progress(
    items = site_name,
    params = gwt_data,
    verbose = verbose,
    item_type = "site",
    fn = function(
      item,
      params,
      code,
      path,
      start_day,
      start_month,
      start_year,
      description,
      version,
      eol
    ) {
      write_gwt(
        site_name = item,
        code = code,
        gwt_data = params,
        path = path,
        start_day = start_day,
        start_month = start_month,
        start_year = start_year,
        description = description,
        version = version,
        eol = eol
      )
    },
    code = code,
    path = path,
    start_day = start_day,
    start_month = start_month,
    start_year = start_year,
    description = description,
    version = version,
    eol = eol
  )

  if (verbose) {
    message("Successfully created GWT files for ", n, " site(s)")
  }

  invisible(NULL)
}
