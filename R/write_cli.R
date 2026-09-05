#' Write AquaCrop Climate File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) climate (.CLI) file that references the
#' temperature, ETo, and rainfall files, along with the appropriate CO2 concentration file
#' for the simulation scenario.
#'
#' @param path Directory path where climate files are located and where .CLI will be written. Default = "CLIMATE/"
#' @param site_name Station name or identifier. The .Tnx, .ETo, and .PLU files with this
#'   station name must exist in the path. Default = "station"
#' @param eol End-of-line character style for the output file.
#'   Options: "windows", "linux", or "macos". If `NULL` (default), eol is
#'   auto-detected from the host operating system, matching the other
#'   AquaCrop file writers.
#' @param scenario CO2 scenario to use. Options: "hist" (historical/Mauna Loa),
#'   "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585".
#'   Default = "hist"
#' @param check_files Logical. If TRUE, checks that all three required climate files (.Tnx, .ETo, .PLU)
#'   exist before writing .CLI file. Default = TRUE
#'
#' @details
#' The .CLI file is the main climate file that references:
#' \itemize{
#'   \item Temperature file (.Tnx)
#'   \item Reference evapotranspiration file (.ETo)
#'   \item Rainfall file (.PLU)
#'   \item CO2 concentration file (.CO2)
#' }
#'
#' **Important**: By default, the function validates that all three climate files (.Tnx, .ETo, .PLU)
#' exist before creating the .CLI file. Set `check_files = FALSE` to skip this validation
#' (not recommended unless you're sure the files will be created later).
#'
#' ## CO2 Scenarios:
#' - **hist**: Historical data from Mauna Loa Observatory (MaunaLoa.CO2)
#' - **RCP scenarios**: Representative Concentration Pathways
#'   - rcp26: RCP 2.6 (low emissions)
#'   - rcp45: RCP 4.5 (intermediate emissions)
#'   - rcp60: RCP 6.0 (intermediate-high emissions)
#'   - rcp85: RCP 8.5 (high emissions)
#' - **SSP scenarios**: Shared Socioeconomic Pathways
#'   - ssp119: SSP1-1.9 (very low emissions)
#'   - ssp126: SSP1-2.6 (low emissions)
#'   - ssp245: SSP2-4.5 (intermediate emissions)
#'   - ssp370: SSP3-7.0 (high emissions)
#'   - ssp585: SSP5-8.5 (very high emissions)
#' @family AquaCrop file writers
#' @return
#' Invisibly returns the full path to the created .CLI file. The function also copies
#' the appropriate CO2 file to the specified path if it doesn't already exist there.
#'
#' @examples
#' \dontrun{
#' # First, create the required climate files
#' data("weather")
#' write_plu(data = weather, site_name = "Wakanda", path = "weather/")
#' write_eto(data = weather, site_name = "Wakanda", path = "weather/")
#' write_tnx(data = weather, site_name = "Wakanda", path = "weather/")
#'
#' # Then write CLI file with historical CO2
#' write_cli(
#'   path = "weather/",
#'   site_name = "Wakanda",
#'   scenario = "hist"
#' )
#'
#' # Write CLI file with RCP 4.5 scenario
#' write_cli(
#'   path = "weather/",
#'   site_name = "Wakanda",
#'   scenario = "rcp45"
#' )
#'
#' # Skip validation (not recommended)
#' write_cli(
#'   path = "weather/",
#'   site_name = "Wakanda",
#'   scenario = "hist",
#'   check_files = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{write_plu}} for writing rainfall files,
#' \code{\link{write_eto}} for writing ETo files,
#' \code{\link{write_tnx}} for writing temperature files
#'
#' @export
write_cli <- function(
    path = "CLIMATE/",
    site_name = "station",
    eol = NULL,
    scenario = "hist",
    check_files = TRUE) {
  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  if (!dir.exists(path)) fs::dir_create(path, recurse = TRUE)

  # Use station name as-is for filenames

  # Check if required climate files exist
  if (check_files) {
    required_files <- paste0(path, site_name, c(".Tnx", ".ETo", ".PLU"))
    missing_files <- required_files[!file.exists(required_files)]

    if (length(missing_files) > 0) {
      stop(
        "Cannot create .CLI file: Missing required climate files for station '", site_name, "':\n",
        paste("  - ", basename(missing_files), collapse = "\n"),
        "\n\nPlease create these files first using:\n",
        "  write_tnx(data = your_data, site_name = '", site_name, "', path = '", path, "')\n",
        "  write_eto(data = your_data, site_name = '", site_name, "', path = '", path, "')\n",
        "  write_plu(data = your_data, site_name = '", site_name, "', path = '", path, "')"
      )
    }
  }

  # Validate and select CO2 file
  valid_scenarios <- c(
    "hist", "rcp26", "rcp45", "rcp60", "rcp85",
    "ssp119", "ssp126", "ssp245", "ssp370", "ssp585"
  )

  if (!scenario %in% valid_scenarios) {
    stop(
      "Invalid scenario: '", scenario, "'\n",
      "Valid options are: ", paste(valid_scenarios, collapse = ", ")
    )
  }

  co2_file <- dplyr::case_when(
    scenario == "rcp26" ~ "RCP2-6.CO2",
    scenario == "rcp45" ~ "RCP4-5.CO2",
    scenario == "rcp60" ~ "RCP6-0.CO2",
    scenario == "rcp85" ~ "RCP8-5.CO2",
    scenario == "ssp119" ~ "SSP1_1.9.CO2",
    scenario == "ssp126" ~ "SSP1_2.6.CO2",
    scenario == "ssp245" ~ "SSP2_4.5.CO2",
    scenario == "ssp370" ~ "SSP3_7.0.CO2",
    scenario == "ssp585" ~ "SSP5_8.5.CO2",
    TRUE ~ "MaunaLoa.CO2"
  )

  # Copy CO2 file to path if it doesn't exist
  co2_target <- paste0(path, co2_file)
  if (!file.exists(co2_target)) {
    co2_source <- path_to_file(co2_file)
    if (!file.exists(co2_source)) {
      stop(
        "CO2 file '", co2_file, "' not found in package data.\n",
        "Source path: ", co2_source
      )
    }
    fs::file_copy(path = co2_source, new_path = co2_target)
  }

  # Build CLI file content
  header_1 <- "default"
  header_2 <- " 7.0   : AquaCrop Version (August 2022)"

  cli_content <- c(
    header_1,
    header_2,
    paste0(site_name, ".Tnx"),
    paste0(site_name, ".ETo"),
    paste0(site_name, ".PLU"),
    co2_file
  )

  # Get line ending
  sep <- .get_eol(eol = eol)

  # Write CLI file
  output_file <- paste0(path, site_name, ".CLI")

  con <- base::file(output_file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(cli_content), con, sep = sep, useBytes = TRUE)

  # Return invisibly with file path
  invisible(output_file)
}
