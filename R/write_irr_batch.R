#' Write AquaCrop Irrigation (.IRR) Files for Multiple Stations
#'
#' Generate AquaCrop irrigation files for multiple stations.
#'
#' @param site_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param method Integer. Irrigation method applied to all stations. One of:
#'   1 = Sprinkler, 2 = Basin, 3 = Border, 4 = Furrow, 5 = Drip.
#' @param wet_surface Numeric. Percentage of soil surface wetted (0-100),
#'   applied to all stations.
#' @param mode Integer. Irrigation mode applied to all stations. One of:
#'   1 = Specified events, 2 = Generated schedule, 3 = Net requirement.
#' @param irr_data Either a single data.frame applied to all stations, or a
#'   list of data.frames (one per station). Structure depends on mode:
#'   mode 1: columns day and depth; mode 2: schedule rule columns;
#'   mode 3: net requirement columns.
#' @param path Output directory path for IRR files. Default: "MANAGEMENT/".
#' @param climate_path Path to climate files directory, used for station
#'   discovery. Default: "CLIMATE/".
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param crop_length Integer. Length of crop cycle in days. If provided,
#'   events or rules beyond crop_length will trigger a warning. Default: NULL.
#' @param eol End-of-line character style. One of "windows", "linux", "macos".
#'   If NULL (default), eol is auto-detected.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .IRR files from path
#'   before writing new files. Default: FALSE.
#'
#' @details
#' The function validates that all specified stations have corresponding
#' climate files. A single data.frame for irr_data will be automatically
#' applied to all stations.
#'
#' The irrigation file for each station is named after the station
#' (e.g., "grid_001.IRR").
#'
#' @family batch operations
#' @return Invisibly returns NULL. The main effect is writing IRR files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' # Example 1: multiple stations, same irrigation events (mode 1)
#' irr <- data.frame(day = c(20, 40, 60), depth = c(30, 30, 25))
#' stations <- c("grid_001", "grid_002")
#' write_irr_batch(
#'   site_name = stations,
#'   method       = 1,
#'   wet_surface  = 100,
#'   mode         = 1,
#'   irr_data     = irr
#' )
#'
#' # Example 2: multiple stations, different irrigation data
#' irr_list <- list(
#'   data.frame(day = c(20, 40), depth = c(30, 25)),
#'   data.frame(day = c(25, 45), depth = c(35, 30))
#' )
#' write_irr_batch(
#'   site_name = stations,
#'   method       = 1,
#'   wet_surface  = 100,
#'   mode         = 1,
#'   irr_data     = irr_list,
#'   crop_length  = 90
#' )
#'
#' # Example 3: auto-discover all stations, silent mode
#' write_irr_batch(
#'   site_name = NULL,
#'   method       = 4,
#'   wet_surface  = 60,
#'   mode         = 2,
#'   irr_data     = irr,
#'   base_path    = "/my/project/path",
#'   verbose      = FALSE
#' )
#' }
#'
#' @seealso \code{\link{write_irr}} for single station IRR file generation.
#'
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_irr_batch <- function(
    site_name = NULL,
    method,
    wet_surface,
    mode,
    irr_data,
    path         = "MANAGEMENT/",
    climate_path = "CLIMATE/",
    version      = 7.1,
    crop_length  = NULL,
    eol          = NULL,
    base_path    = getwd(),
    verbose      = TRUE,
    clean        = FALSE
) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.IRR$", verbose)
  }

  # Discover or validate stations from climate files
  site_name <- .discover_or_validate_items(
    item_names   = site_name,
    climate_path = climate_path,
    base_path    = base_path,
    item_type    = "site",
    verbose      = verbose
  )

  n <- length(site_name)
  .warn_single_item(n, "write_irr_batch", "write_irr", verbose)

  # Normalize irr_data: single data.frame applied to all stations
  if (is.data.frame(irr_data)) {
    if (verbose) {
      message("Applying the same irrigation data to all ", n, " site(s)")
    }
    irr_data <- rep(list(irr_data), n)
  }

  if (!is.list(irr_data) || length(irr_data) != n) {
    stop(
      "irr_data must be either:\n",
      "  - A single data.frame (applied to all stations), or\n",
      "  - A list of data.frames with length matching site_name\n",
      "Expected length: ", n, ", got: ", length(irr_data),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Writing IRR files for ", n, " site(s)...")
  }

  .batch_with_progress(
    items     = site_name,
    params    = irr_data,
    verbose   = verbose,
    item_type = "site",
    fn = function(item, params, method, wet_surface, mode, path,
                  version, crop_length, eol) {
      write_irr(
        site_name = item,
        method          = method,
        wet_surface     = wet_surface,
        mode            = mode,
        irr_data        = params,
        path            = path,
        version         = version,
        crop_length     = crop_length,
        eol             = eol
      )
    },
    method      = method,
    wet_surface = wet_surface,
    mode        = mode,
    path        = path,
    version     = version,
    crop_length = crop_length,
    eol         = eol
  )

  if (verbose) {
    message("Successfully created IRR files for ", n, " site(s)")
  }

  invisible(NULL)
}
