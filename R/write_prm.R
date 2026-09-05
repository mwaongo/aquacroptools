#' Write AquaCrop Parameter (.PRM) File for Single Year
#'
#' @description
#' Creates an AquaCrop parameter file (.PRM) for a single station and year.
#' Includes the header (crop name and version) plus the year block.
#'
#' @param site_name Station identifier (used to find CLI, Tnx, ETo, PLU files)
#' @param path Output directory path for PRM files. Default: `"LIST/"`
#' @param year Year of cultivation
#' @param planting_doy Day of year for planting (1-365/366)
#' @param crop_name Crop name (e.g., "maize_90days")
#' @param crop_path Path to crop file directory
#' @param crop_duration Growing season length in days
#' @param climate_path Path to climate file directory (absolute or relative)
#' @param calendar_path Path to calendar file directory. NULL writes "(None)".
#' @param management_path Path to management file directory (absolute or relative)
#' @param irrigation_path Path to irrigation file directory. NULL writes "(None)".
#' @param soil_path Path to soil file directory (absolute or relative)
#' @param groundwater_path Path to groundwater file directory. NULL writes "(None)".
#' @param offseason_path Path to off-season file directory. NULL writes "(None)".
#' @param obs_path Path to field observations file directory. NULL writes "(None)".
#' @param simulation_start_doy Day of year for simulation start (NULL = April 1st)
#' @param scenario Climate scenario: "hist", "rcp26", "rcp45", "rcp60", "rcp85",
#'   "ssp119", "ssp126", "ssp245", "ssp370", "ssp585"
#' @param write_header Logical. If TRUE (default), writes header. FALSE for batch mode.
#' @param eol End-of-line character style. Options: "windows", "linux", or "macos".
#'   If `NULL` (default), eol is auto-detected.
#' @param use_standalone Logical. Whether running in standalone mode (default: TRUE)
#' @param base_path Character. Base absolute path (required for file existence checking)
#'
#' @return Invisibly returns the output file path
#' @keywords internal
#' @noRd
.write_prm <- function(
    site_name,
    path                 = "LIST/",
    year,
    planting_doy,
    crop_name,
    crop_path            = "CROP/",
    crop_duration        = NULL,
    climate_path         = "CLIMATE/",
    calendar_path        = NULL,
    management_path      = "MANAGEMENT/",
    irrigation_path      = NULL,
    soil_path            = "SOIL/",
    groundwater_path     = NULL,
    offseason_path       = NULL,
    obs_path             = NULL,
    simulation_start_doy = NULL,
    scenario             = "hist",
    write_header         = TRUE,
    eol                  = NULL,
    use_standalone       = TRUE,
    base_path            = getwd()
) {
  stopifnot(
    is.character(path)         && length(path)         == 1,
    is.character(site_name)    && length(site_name)    == 1,
    is.numeric(year)           && length(year)         == 1,
    is.numeric(planting_doy)   && length(planting_doy)   == 1,
    is.logical(write_header)   && length(write_header)   == 1,
    is.logical(use_standalone) && length(use_standalone) == 1,
    !missing(base_path) && is.character(base_path)
  )

  sep <- .get_eol(eol)

  # Climate files (Mandatory)
  cli_file <- fs::path(base_path, climate_path, paste0(site_name, ".CLI"))
  tnx_file <- fs::path(base_path, climate_path, paste0(site_name, ".Tnx"))
  eto_file <- fs::path(base_path, climate_path, paste0(site_name, ".ETo"))
  plu_file <- fs::path(base_path, climate_path, paste0(site_name, ".PLU"))

  if (!file.exists(cli_file)) stop("CLI file not found: ", cli_file)
  if (!file.exists(tnx_file)) stop("Tnx file not found: ", tnx_file)
  if (!file.exists(eto_file)) stop("ETo file not found: ", eto_file)
  if (!file.exists(plu_file)) stop("PLU file not found: ", plu_file)

  # Management (Mandatory ? should check)
  man_file <- fs::path(base_path, management_path, paste0(site_name, ".MAN"))
  if (!file.exists(man_file)) stop("MAN file not found: ", man_file)

  # Soil (Mandatory)
  sol_file <- fs::path(base_path, soil_path, paste0(site_name, ".SOL"))
  sw0_file <- fs::path(base_path, soil_path, paste0(site_name, ".SW0"))
  if (!file.exists(sol_file)) stop("SOL file not found: ", sol_file)
  if (!file.exists(sw0_file)) stop("SW0 file not found: ", sw0_file)

  # Simulation timing
  if (is.null(simulation_start_doy)) {
    simulation_start_doy <- ifelse(is_leap_year(year), 92, 91)  # April 1st
  }

  # Resolve crop duration, find crop_duration from crop file
  cro_file <- fs::path(base_path, crop_path, paste0(crop_name, ".CRO"))
  if (is.null(crop_duration)) {
    if (!fs::file_exists(cro_file)) stop("CRO file not found: ", cro_file, call. = FALSE)
    lines_cro <- readLines(cro_file, warn = FALSE)
    crop_duration <- as.integer(trimws(strsplit(lines_cro[55], ":")[[1]][1]))
  }

  planting_end_doy      <- planting_doy + crop_duration - 1
  sim_start_day_number  <- day_number(year, doy = simulation_start_doy)
  sim_end_day_number    <- day_number(year, doy = planting_end_doy)
  crop_start_day_number <- day_number(year, doy = planting_doy)
  crop_end_day_number   <- day_number(year, doy = planting_end_doy)

  co2_file            <- .get_co2_file(scenario)
  sim_start_date_str  <- .format_date_string(year, simulation_start_doy)
  crop_start_date_str <- .format_date_string(year, planting_doy)
  crop_end_date_str   <- .format_date_string(year, planting_end_doy)

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  output_file <- file.path(path, paste0(site_name, ".PRM"))

  # PRM path helpers
  climate_path_prm    <- path_for_prm(climate_path,    use_standalone = use_standalone, base_path = base_path)
  crop_path_prm       <- path_for_prm(crop_path,       use_standalone = use_standalone, base_path = base_path)
  management_path_prm <- path_for_prm(management_path, use_standalone = use_standalone, base_path = base_path)
  soil_path_prm       <- path_for_prm(soil_path,       use_standalone = use_standalone, base_path = base_path)

  # Optional section helpers
  # Returns list(name, dir) both set to "(None)" when path is NULL

  .opt_file <- function(opt_path, ext) {
    if (is.null(opt_path)) return(list(name = "(None)", dir = "(None)"))
     f <- fs::path(base_path, opt_path, paste0(site_name, ext))
     list(
       name = basename(f),
       dir  = path_for_prm(opt_path, use_standalone = use_standalone, base_path = base_path)
     )
  }
  .fmt_section <- function(info, sep) {
    paste0(
      .format_string4(info$name, "%s", nchar(info$name) + 3), sep,
      .format_string4(info$dir,  "%s", nchar(info$dir)  + 3), sep
    )
  }

  # Name of the file and relative path  for PRM sections

  cal_info <- .opt_file(calendar_path,    ".CAL")
  irr_info <- .opt_file(irrigation_path,  ".IRR")
  gwt_info <- .opt_file(groundwater_path, ".GWT")
  off_info <- .opt_file(offseason_path,   ".OFF")
  obs_info <- .opt_file(obs_path,         ".OBS")

  # Build PRM content, line by line
  prm_lines <- c(
    paste0(.format_string3(1,                     "%7.0f", 16), ": Year number of cultivation (Seeding/planting year)", sep),
    paste0(.format_string3(sim_start_day_number,  "%7.0f", 16), ": First day of simulation period - ",  sim_start_date_str,  sep),
    paste0(.format_string3(sim_end_day_number,    "%7.0f", 16), ": Last day of simulation period - ",   crop_end_date_str,   sep),
    paste0(.format_string3(crop_start_day_number, "%7.0f", 16), ": First day of cropping period - ",    crop_start_date_str, sep),
    paste0(.format_string3(crop_end_day_number,   "%7.0f", 16), ": Last day of cropping period - ",     crop_end_date_str,   sep),
    # 1. Climate
    paste0("-- 1. Climate (CLI) file", sep),
    paste0(.format_string4(basename(cli_file), "%s", nchar(basename(cli_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.1 Temperature (Tnx or TMP) file", sep),
    paste0(.format_string4(basename(tnx_file), "%s", nchar(basename(tnx_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.2 Reference ET (ETo) file", sep),
    paste0(.format_string4(basename(eto_file), "%s", nchar(basename(eto_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.3 Rain (PLU) file", sep),
    paste0(.format_string4(basename(plu_file), "%s", nchar(basename(plu_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.4 Atmospheric CO2 concentration (CO2) file", sep),
    paste0(.format_string4(co2_file,           "%s", nchar(co2_file)           + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    # 2. Calendar
    paste0("-- 2. Calendar (CAL) file", sep),
    .fmt_section(cal_info, sep),
    # 3. Crop
    paste0("-- 3. Crop (CRO) file", sep),
    paste0(.format_string4(paste0(crop_name, ".CRO"), "%s", nchar(paste0(crop_name, ".CRO")) + 3), sep),
    paste0(.format_string4(crop_path_prm, "%s", nchar(crop_path_prm) + 3), sep),
    # 4. Irrigation
    paste0("-- 4. Irrigation management (IRR) file", sep),
    .fmt_section(irr_info, sep),
    # 5. Field management
    paste0("-- 5. Field management (MAN) file", sep),
    paste0(.format_string4(basename(man_file),  "%s", nchar(basename(man_file))  + 3), sep),
    paste0(.format_string4(management_path_prm, "%s", nchar(management_path_prm) + 3), sep),
    # 6. Soil
    paste0("-- 6. Soil profile (SOL) file", sep),
    paste0(.format_string4(basename(sol_file), "%s", nchar(basename(sol_file)) + 3), sep),
    paste0(.format_string4(soil_path_prm,      "%s", nchar(soil_path_prm)      + 3), sep),
    # 7. Groundwater
    paste0("-- 7. Groundwater table (GWT) file", sep),
    .fmt_section(gwt_info, sep),
    # 8. Initial conditions
    paste0("-- 8. Initial conditions (SW0) file", sep),
    paste0(.format_string4(basename(sw0_file), "%s", nchar(basename(sw0_file)) + 3), sep),
    paste0(.format_string4(soil_path_prm,      "%s", nchar(soil_path_prm)      + 3), sep),
    # 9. Off-season
    paste0("-- 9. Off-season conditions (OFF) file", sep),
    .fmt_section(off_info, sep),
    # 10. Field data
    paste0("-- 10. Field data (OBS) file", sep),
    .fmt_section(obs_info, sep)
  )

  if (write_header) {
    # this is internal trick to have header for the first  year before writing repeated block
    .write_prm_header(path = path, crop = crop_name, site_name = site_name, eol = eol)
  }

  readr::write_file(
    x      = paste(prm_lines, collapse = ""),
    file   = output_file,
    append = TRUE
  )

  invisible(output_file)
}


#' Write AquaCrop Parameter (.PRM) Files
#'
#' Creates a `.PRM` file for one site across multiple years. The planting
#' schedule can be supplied directly or derived from a `.CAL` file via
#' \code{\link{find_onset}}.
#'
#' @param site_name Character. Station name used to locate climate, soil,
#'   management, and optional auxiliary files.
#' @param path Output directory path for PRM files. Default: \code{"LIST/"}.
#' @param planting_schedule A \code{data.frame} with columns \code{year} and
#'   \code{planting_doy}. Required when \code{calendar_path} is \code{NULL}.
#' @param crop_name Character. Crop name (without extension).
#' @param crop_path Character. Path to the crop file directory.
#'   Default: \code{"CROP/"}.
#' @param crop_duration Integer. Growing season length in days. Default: 90.
#' @param climate_path Character. Path to climate file directory.
#'   Default: \code{"CLIMATE/"}.
#' @param calendar_path Character or NULL. Path to the CAL directory. When
#'   provided, \code{find_onset()} is called to derive
#'   \code{planting_schedule}; any supplied \code{planting_schedule} is
#'   ignored with a warning.
#' @param management_path Character. Path to management file directory.
#'   Default: \code{"MANAGEMENT/"}.
#' @param irrigation_path Character or NULL. Path to the IRR file directory.
#'   If NULL (default), the section is written as \code{(None)}
#' @param soil_path Character. Path to soil file directory.
#'   Default: \code{"SOIL/"}.
#' @param groundwater_path Character or NULL. Path to the GWT file directory.
#'   Default: \code{NULL}.
#' @param offseason_path Character or NULL. Path to the OFF file directory.
#'   If the \code{.OFF} file is not found, a warning is issued and the
#'   section is written as \code{(None)}. Default: \code{NULL}.
#' @param obs_path Character or NULL. Path to the OBS file directory. If the
#'   \code{.OBS} file is not found, a warning is issued and the section is
#'   written as \code{(None)}. Default: \code{NULL}.
#' @param simulation_start_doy Integer or NULL. First DOY of simulation.
#'   Default: \code{NULL} (April 1st).
#' @param scenario Character. Climate scenario identifier.
#'   Default: \code{"hist"}.
#' @param eol Character. End-of-line style: \code{"windows"}, \code{"linux"},
#'   or \code{"macos"}. If \code{NULL} (default), eol is auto-detected.
#' @param use_standalone Logical. Whether paths are formatted for AquaCrop
#'   standalone mode. Default: \code{TRUE}.
#' @param base_path Character. Base absolute path. Default: \code{getwd()}.
#'
#' @return Invisibly returns the output file path.
#'
#' @family AquaCrop file writers
#' @export
write_prm <- function(
    site_name,
    path                 = "LIST/",
    planting_schedule    = NULL,
    crop_name            = NULL,
    crop_path            = "CROP/",
    crop_duration        = NULL,
    climate_path         = "CLIMATE/",
    calendar_path        = NULL,
    management_path      = "MANAGEMENT/",
    irrigation_path      = NULL,
    soil_path            = "SOIL/",
    groundwater_path     = NULL,
    offseason_path       = NULL,
    obs_path             = NULL,
    simulation_start_doy = NULL,
    scenario             = "hist",
    eol                  = NULL,
    use_standalone       = TRUE,
    base_path            = getwd()
) {
  stopifnot(
    is.character(path)         && length(path)         == 1,
    is.character(site_name)    && length(site_name)    == 1,
    is.logical(use_standalone) && length(use_standalone) == 1
  )

  if (is.null(crop_name) != is.null(crop_path)) {
    stop("crop_name and crop_path must both be provided or both be NULL.")
  }

  # Warn and set path to NULL, if optional path is provided with  missing file
  if (!is.null(irrigation_path)) {
    irr_file <- fs::path(base_path, irrigation_path, paste0(site_name, ".IRR"))
    if (!fs::file_exists(irr_file)) {
      warning("Irrigation file not found: ", irr_file,
              "irrigation_path set to NULL.", call. = FALSE)
      irrigation_path <- NULL
    }
  }

  if (!is.null(obs_path)) {
    obs_file <- fs::path(base_path, obs_path, paste0(site_name, ".OBS"))
    if (!fs::file_exists(obs_file)) {
      warning("Observations file not found: ", obs_file,
              "obs_path set to NULL.", call. = FALSE)
      obs_path <- NULL
    }
  }

  if (!is.null(offseason_path)) {
    off_file <- fs::path(base_path, offseason_path, paste0(site_name, ".OFF"))
    if (!fs::file_exists(off_file)) {
      warning("Off-season file not found: ", off_file,
              "offseason_path set to NULL.", call. = FALSE)
      offseason_path <- NULL
    }
  }

  # Calendar path  is used to derive planting/sowing dates

  if (!is.null(calendar_path)) {
    if (!is.null(planting_schedule)) {
      warning(
        "Both calendar_path and planting_schedule were provided. ",
        "calendar_path takes precedence; planting_schedule is ignored.",
        call. = FALSE
      )
    }
    # use of the calendar file to compute sowing dates
    onset <- find_onset(
      site_name    = site_name,
      cal_path     = calendar_path,
      climate_path = climate_path,
      base_path    = base_path
    )
    planting_schedule <- data.frame(year = onset$year, planting_doy = onset$onset_doy)
    na_years <- planting_schedule$year[is.na(planting_schedule$planting_doy)]
    if (length(na_years) > 0L) {
      warning(
        "Onset criterion not met for ", length(na_years), " year(s): ",
        paste(na_years, collapse = ", "),
        " these years are excluded from the PRM.",
        call. = FALSE
      )
      planting_schedule <- planting_schedule[!is.na(planting_schedule$planting_doy), ]
    }
  }

  if (is.null(planting_schedule)) {
    stop("planting_schedule is required when calendar_path is set to NULL.", call. = FALSE)
  }
  if (
    !is.data.frame(planting_schedule) ||
    !all(c("year", "planting_doy") %in% names(planting_schedule)) ||
    nrow(planting_schedule) == 0
  ) {
    stop(
      "planting_schedule must be a non-empty data.frame with columns year and planting_doy.",
      call. = FALSE
    )
  }

  #  Write PRM file
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  output_file <- file.path(path, paste0(site_name, ".PRM"))
  if (file.exists(output_file)) file.remove(output_file)

  for (i in seq_len(nrow(planting_schedule))) {
    .write_prm(
      site_name            = site_name,
      path                 = path,
      year                 = planting_schedule$year[i],
      planting_doy         = planting_schedule$planting_doy[i],
      crop_name            = crop_name,
      crop_path            = crop_path,
      crop_duration        = crop_duration,
      climate_path         = climate_path,
      calendar_path        = NULL, # set to (None) in PRM, already used tp get planting_doy for each year
      management_path      = management_path,
      irrigation_path      = irrigation_path,
      soil_path            = soil_path,
      groundwater_path     = groundwater_path,
      offseason_path       = offseason_path,
      obs_path             = obs_path,
      simulation_start_doy = simulation_start_doy,
      scenario             = scenario,
      write_header         = (i == 1),
      eol                  = eol,
      use_standalone       = use_standalone,
      base_path            = base_path
    )
  }

  invisible(output_file)
}
