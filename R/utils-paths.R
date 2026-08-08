#' Add Trailing Slash to Path
#'
#' @description
#' Internal helper function that ensures a directory path ends with a forward slash (/).
#' This provides consistent path handling across all file writing functions.
#'
#' @param path Character string specifying a directory path
#'
#' @return Character string with a guaranteed trailing slash
#'
#' @examples
#' .add_trailing_slash("CLIMATE")
#' # Returns: "CLIMATE/"
#'
#' .add_trailing_slash("CLIMATE/")
#' # Returns: "CLIMATE/" (unchanged)
#'
#' @keywords internal
#' @noRd
.add_trailing_slash <- function(path) {
  if (!stringr::str_ends(path, pattern = "/")) {
    paste0(path, "/")
  } else {
    path
  }
}


#' Get CO2 File Based on Scenario
#'
#' @description
#' Internal helper function to map climate scenario names to CO2 concentration files.
#'
#' @param scenario Character string specifying the climate scenario
#'   Options: "hist", "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585"
#'
#' @return Character string with the CO2 filename
#' @keywords internal
#' @noRd
.get_co2_file <- function(scenario) {
  co2_files <- list(
    "hist" = "MaunaLoa.CO2",
    "rcp26" = "RCP2-6.CO2",
    "rcp45" = "RCP4-5.CO2",
    "rcp60" = "RCP6-0.CO2",
    "rcp85" = "RCP8-5.CO2",
    "ssp119" = "SSP1_1.9.CO2",
    "ssp126" = "SSP1_2.6.CO2",
    "ssp245" = "SSP2_4.5.CO2",
    "ssp370" = "SSP3_7.0.CO2",
    "ssp585" = "SSP5_8.5.CO2"
  )

  if (!scenario %in% names(co2_files)) {
    warning("Scenario '", scenario, "' not recognized. Using default 'MaunaLoa.CO2'")
    return("MaunaLoa.CO2")
  }

  return(co2_files[[scenario]])
}


#' Internal: Generate Path for PRM Directories
#'
#' @description
#' Internal helper to generate directory paths formatted for .PRM files.
#' Handles both standalone mode (relative paths) and non-standalone mode (absolute paths).
#'
#' @param dir_name Character. Directory name (e.g., "CLIMATE", "CROP", "SOIL")
#' @param use_standalone Logical. Whether running in standalone mode (default: TRUE)
#' @param base_path Character. Base absolute path (required if use_standalone=FALSE)
#'
#' @return Character string with formatted path:
#'   - Standalone + Windows: ".\\DIRNAME\\"
#'   - Standalone + Non-Windows: "'./DIRNAME/'"
#'   - Non-standalone: absolute path to /DIRNAME/
#'
#' @keywords internal
#' @noRd
path_for_prm <- function(dir_name, use_standalone = TRUE, base_path = NULL) {
  # Validate dir_name
  if (!is.character(dir_name) || length(dir_name) != 1 || nchar(dir_name) == 0) {
    stop("dir_name must be a non-empty character string")
  }

  # Validate use_standalone
  if (!is.logical(use_standalone) || length(use_standalone) != 1) {
    stop("use_standalone must be a single logical value")
  }

  # Clean dir_name: remove any leading/trailing slashes
  dir_name <- sub("^[/\\\\]+", "", dir_name)  # Remove leading slashes
  dir_name <- sub("[/\\\\]+$", "", dir_name)  # Remove trailing slashes

  # Auto-detect OS
  is_windows <- get_os() == "windows"

  # Generate path based on standalone mode
  if (use_standalone) {
    if (is_windows) {
      return(paste0(".\\", dir_name, "\\"))
    } else {
      return(paste0("'./", dir_name, "/'"))
    }
  } else {
    # Non-standalone: absolute path
    if (is.null(base_path) || nchar(base_path) == 0) {
      stop("base_path is required when use_standalone = FALSE")
    }

    # Normalize base_path (remove trailing separator if present)
    base_path <- sub("[/\\]+$", "", base_path)

    # Use forward slashes for consistency across platforms
    return(paste0(base_path, "/", dir_name, "/"))
  }
}
