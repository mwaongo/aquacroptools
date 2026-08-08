# =============================================================================
# AquaCrop Project Directory Structure
# =============================================================================

#' Standard AquaCrop directory names
#' @keywords internal
#' @noRd
.AQUACROP_DIRS <- c(
  "CLIMATE",
  "CAL",
  "CROP",
  "LIST",
  "MANAGEMENT",
  "OBS",
  "OUTP",
  "PARAM",
  "SIMUL",
  "SOIL"
)

#' Get version tag for AquaCrop releases
#' @param version Character string. Version number (e.g., "7.1", "7.2")
#' @return Character string. Git tag (e.g., "v7.1.1", "v7.2")
#' @keywords internal
#' @noRd
.get_version_tag <- function(version) {
  if (is.null(version) || is.na(version)) {
    return(NULL)
  }

  # Special case: version 7.1 uses tag v7.1.1
  if (version == "7.1") {
    return("v7.1.1")
  }

  # Default: tag is v{version}
  return(paste0("v", version))
}


#' Initialize AquaCrop Project Structure
#'
#' Creates a directory structure for AquaCrop crop water productivity model
#' simulations and installs the AquaCrop binary. After initialization,
#' you can run simulations directly in the project directory.
#'
#' @param path Character string specifying the root directory where the project
#'   should be created. Default is the current working directory (\code{"."}).
#' @param version AquaCrop version to install (NULL = latest, e.g. "7.1")
#' @param use_rproject Logical. If TRUE, creates an RStudio project file (.Rproj)
#'   in the project directory. Default is TRUE.
#' @param os Operating system ("windows", "linux", "macos", NULL = auto-detect)
#' @param force Force reinstall binary even if exists (default: FALSE)
#' @param overwrite Logical; if TRUE, overwrites existing directory structure.
#'   Default: FALSE.
#'
#' @return Invisibly returns the normalized path to the created directory.
#'
#' @details
#' The function creates the following directory structure:
#' \describe{
#'   \item{CLIMATE/}{Climate input files (temperature, rainfall, ETo)}
#'   \item{CROP/}{Crop parameter files}
#'   \item{GWT/}{Groundwater table files}
#'   \item{IRR/}{Irrigation schedule files}
#'   \item{LIST/}{Project simulation files \code{*.PRM}}
#'   \item{MANAGEMENT/}{Field management practice files \code{*.MAN}}
#'   \item{OBS/}{Field observation files}
#'   \item{OUTP/}{Simulation output files}
#'   \item{PARAM/}{Program parameters files \code{*.PPn}}
#'   \item{SIMUL/}{Simulation configuration files}
#'   \item{SOIL/}{Soil profile and water content characteristic files}
#' }
#'
#' The AquaCrop binary is automatically downloaded and installed in the project
#' directory. The appropriate executable for your operating system (Windows,
#' macOS, or Linux) is placed directly in the root of the project.
#'
#' @examples
#' \dontrun{
#' # Initialize new project
#' init_aquacrop("~/my-aquacrop-project")
#'
#' # Then work in that directory
#' setwd("~/my-aquacrop-project")
#' run_aquacrop()
#'
#' # Initialize with specific version
#' init_aquacrop("~/project", version = "7.1")
#'
#' # Overwrite existing structure
#' init_aquacrop("~/project", overwrite = TRUE, force = TRUE)
#' }
#'
#' @seealso
#' \link{install_binaries}, \link{run_aquacrop}
#'
#' @references
#' \url{https://www.fao.org/aquacrop/} for AquaCrop documentation
#'
#' @export

init_aquacrop <- function(path = ".",
                          version = NULL,
                          os = NULL,
                          force = FALSE,
                          overwrite = FALSE,
                          use_rproject = TRUE) {
  # Input validation
  stopifnot(
    "path must be a character string" = is.character(path) && length(path) == 1,
    "force must be logical" = is.logical(force) && length(force) == 1,
    "overwrite must be logical" = is.logical(overwrite) && length(overwrite) == 1
  )

  # Expand path (handle ~, relative paths, etc.)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  # Check if directory exists
  if (dir.exists(path) && !overwrite) {
    # Check if it already has AquaCrop structure (check core folders only)
    core_folders <- .AQUACROP_DIRS
    has_structure <- all(dir.exists(file.path(path, core_folders)))

    if (has_structure) {
      message("AquaCrop project structure already exists at: ", path)
      message("Use overwrite = TRUE to recreate directories")

      # Check if binary exists
      exe_name <- .aquacrop_exe_name()
      has_binary <- file.exists(file.path(path, exe_name))

      if (!has_binary) {
        message("\nBinary not found. Installing...")
        version <- install_binaries(version = version, os = os, path = path, force = force)
      } else if (force) {
        message("\nReinstalling binary (force = TRUE)...")
        version <- install_binaries(version = version, os = os, path = path, force = force)
      }

      message("\n", cli::symbol$info, " To work in this project, run:")
      message("  setwd(\"", path, "\")")
      message("  run_aquacrop()")

      return(invisible(path))
    } else {
      stop(
        "Directory '", path, "' already exists but is not an AquaCrop project.\n",
        "Use overwrite = TRUE to recreate, or choose a different path.",
        call. = FALSE
      )
    }
  }


  message(cli::symbol$arrow_right, " Creating AquaCrop project structure...")

  # Create root folder
  if (!dir.exists(path)) {
    tryCatch(
      {
        dir.create(path, recursive = TRUE)
      },
      error = function(e) {
        stop(
          "Failed to create directory: ", path, "\n",
          "Error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # Install AquaCrop binary
  message("\n", cli::symbol$arrow_right, " Installing AquaCrop binary...")
  tryCatch(
    {
      version <- install_binaries(version = version, os = os, path = path, force = force)
    },
    error = function(e) {
      warning(
        "Failed to install AquaCrop binary: ", conditionMessage(e), "\n",
        "You can install it manually later with: install_binaries(path = \"", path, "\")",
        call. = FALSE
      )
    }
  )
  # Create subdirectories
  for (d in .AQUACROP_DIRS) {
    subdir_path <- file.path(path, d)
    tryCatch(
      {
        dir.create(subdir_path, showWarnings = FALSE, recursive = TRUE)
      },
      error = function(e) {
        warning(
          "Failed to create subdirectory: ", d, "\n",
          "Error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  message(cli::symbol$tick, " Created directories: ", paste(.AQUACROP_DIRS, collapse = ", "))

  .install_templates(path, overwrite = TRUE)

  # Create README
  .create_readme(path, .AQUACROP_DIRS, version)

  # Create R project if use_rproject = TRUE
  if (use_rproject && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::openProject(path = path, newSession = FALSE)
  } else {
    message("\n", cli::symbol$info, " To work in this project, run:")
    message("  setwd(\"", path, "\")")
    message("  run_aquacrop()")
  }

  invisible(path)
}

#' Helper function to create README
#' @param path Character string. Path where the README.txt will be created
#' @param dirs Character vector. List of directory names
#' @param version Character string or NULL. AquaCrop version number. If NULL, displays "latest"
#' @param pkg_name Character string. Name of the package calling this function. Default is "aquacropr"
#' @keywords internal
#' @noRd
#'
.create_readme <- function(path, dirs = .AQUACROP_DIRS, version = NULL, pkg_name = "aquacropr") {
  readme_path <- file.path(path, "README.txt")

  # Determine executable name
  exe_name <- .aquacrop_exe_name()

  # Version info
  if (is.null(version)) {
    version_info <- "AquaCrop version: unknown"
  } else {
    tag <- .get_version_tag(version)
    version_info <- paste0(
      "AquaCrop version: ", version,
      " (https://github.com/KUL-RSDA/AquaCrop/releases/tag/", tag, ")"
    )
  }

  # Package version
  pkg_version <- tryCatch(
    as.character(utils::packageVersion(pkg_name)),
    error = function(e) "development"
  )

  # Directory descriptions
  dir_descriptions <- c(
    CLIMATE = "Climate input files (*.Tnx, *.PLU, *.ETo, *.CLI)",
    CROP = "Crop parameter files (*.CRO)",
    CAL = "Crop calendar files (*.CAL)",
    LIST = "Project simulation files (*.PRM)",
    MANAGEMENT = "Field management practice  and irrigation files (*.MAN, *.IRR)",
    OBS = "Field observation files (*.OBS)",
    OUTP = "Simulation output files *.OUT (created after running)",
    PARAM = "Program parameters files (*.PPn)",
    SIMUL = "Simulation configuration files (*.SIM)",
    SOIL = "Soil profile and water content files (*.SOL, *.SW0, *.GWT)"
  )

  # Generate directory listing with descriptions
  dir_listing <- unlist(lapply(dirs, function(d) {
    desc <- dir_descriptions[[d]]
    c(paste0("  ", d, "/"),
      if (!is.null(desc)) paste0("    ", desc) else NULL)
  }))

  readme_content <- c(
    "AquaCrop Project",
    "================",
    "",
    paste("Created:", Sys.Date()),
    paste("Executable:", exe_name),
    version_info,
    paste0("Generated by: ", pkg_name, " v", pkg_version, "::init_aquacrop()"),
    "",
    "Project Structure",
    "-----------------",
    "",
    "This project contains the following AquaCrop directories:",
    "",
    dir_listing,
    "",
    "Quick Start with aquacropr",
    "------------------------------",
    "",
    "aquacropr is an R package that simplifies AquaCrop workflows:",
    "  - Create input files programmatically (no manual text editing)",
    "  - Run batch simulations easily",
    "  - Read and analyze outputs in R",
    "  - Integrate with tidyverse, ggplot2, and R ecosystem",
    "",
    "Complete workflow example:",
    "",
    "library(aquacropr)",
    "",
    "# Option 1: Create climate files individually",
    "write_tnx(",
    "  path = 'CLIMATE/',",
    "  stn = 'MyLocation',",
    "  data = temperature_df",
    ")",
    "",
    "write_plu(",
    "  path = 'CLIMATE/',",
    "  stn = 'MyLocation',",
    "  data = rainfall_df",
    ")",
    "",
    "write_eto(",
    "  path = 'CLIMATE/',",
    "  stn = 'MyLocation',",
    "  data = eto_df",
    ")",
    "",
    "write_cli(",
    "  path = 'CLIMATE/',",
    "  stn = 'MyLocation'",
    ")",
    "",
    "# Option 2: Create all climate files at once (recommended)",
    "write_climate(",
    "  path = 'CLIMATE/',",
    "  stn = 'MyLocation',",
    "  data = weather_df  # Must contain: year, month, day, rain, et0, tmin, tmax",
    ")",
    "",
    "# Create crop and soil files",
    "write_cro(",
    "  path = 'CROP/',",
    "  crop_name = 'Maize',",
    "  params = crop_params",
    ")",
    "",
    "write_sol(",
    "  path = 'SOIL/',",
    "  site_name = 'Loam',",
    "  texture = 'loam'",
    ")",
    "",
    "write_swo(",
    "  path = 'SOIL/',",
    "  soil_name = 'Loam'",
    ")",
    "",
    "# (Optional) Field management",
    "write_man(",
    "  path = 'MANAGEMENT/',",
    "  management_name = 'LowFertility',",
    "  params = management_params",
    ")",
    "",
    "# Create project file",
    "write_prm(",
    "  path = 'LIST/',",
    "  station_name = 'MyLocation',",
    "  planting_schedule = planting_df,  # Must contain: year, planting_doy",
    "  crop_name = 'Maize',",
    "  crop_path = './CROP/',",
    "  climate_path = './CLIMATE/',",
    "  management_path = './MANAGEMENT/',",
    "  soil_path = './SOIL/',",
    "  base_path = getwd()",
    ")",
    "",
    "# Run simulation",
    "run_aquacrop()",
    "",
    "# Read and analyze results",
    "results <- read_seas_out('OUTP/MyLocationPRMseason.OUT')",
    "summary(results)",
    "",
    "Why aquacropr vs manual file creation?",
    "  - Automatic format validation - fewer errors",
    "  - Reproducible workflows - entire analysis in R scripts",
    "  - Batch processing - easily run 100+ scenarios",
    "  - Data integration - combine with other R packages",
    "",
    "Documentation: https://github.com/mwaongo/aquacropr",
    "Help in R: ?write_tnx, ?write_climate, vignette('aquacropr')",
    "",
    "About AquaCrop",
    "--------------",
    "AquaCrop is a crop water productivity model developed by FAO (Version 7.2, Aug 2024).",
    "The model simulates the soil-plant-atmosphere continuum with emphasis on water",
    "productivity, suitable for conditions where water is a key limiting factor.",
    "",
    "Key capabilities:",
    "  - Simulates crop growth, development, and yield under various water conditions",
    "  - Accounts for stress from: water, soil salinity, fertility, temperature, weeds",
    "  - Applications: irrigation scheduling, deficit irrigation, yield forecasting,",
    "    climate change impact assessment, water productivity analysis",
    "  - Stand-alone version (no GUI) designed for plug-in use and automation",
    "",
    "Compatibility:",
    "  - Version 7.2 is compatible with 7.1 and 7.0 project files (.PRO/.PRM, .PP1/.PPn)",
    "  - Input data files (.CRO, .SOL, .CLI) from version 6.x can be used",
    "  - Project files from version 6.x and below must be recreated",
    "",
    "Directory Structure Details",
    "---------------------------",
    "",
    "This project follows the standard AquaCrop directory organization:",
    "",
    "CLIMATE/",
    "  Climate input files: temperature (*.Tnx), rainfall (*.PLU),",
    "  reference ET (*.ETo), climate master files (*.CLI), CO2 (*.CO2)",
    "",
    "CROP/",
    "  Crop parameter files (*.CRO): growth, development, yield parameters,",
    "  water stress responses. Examples: Maize.CRO, Wheat.CRO, Potato.CRO",
    "",
    "CAL/",
    "  Calendar files (*.CAL): used to determinate sowing date.",
    "  Examples: Wakanda.CAL, Ottawa.CAL, Kaya.CAL",
    "",
    "SOIL/",
    "  Soil profile files (*.SOL): hydraulic properties, texture, water retention",
    "  Initial soil water (*.SW0): initial conditions for each soil layer",
    "  Groundwater (*.GWT): groundwater table depth and salinity (optional)",
    "",
    "MANAGEMENT/",
    "  Field management (*.MAN): fertility, bunds, mulches, weed control",
    "  Irrigation schedules (*.IRR): irrigation events and amounts",
    "",
    "SIMUL/",
    "  Simulation files (*.SIM):",
    "  Control files: AggregationResults.SIM, DailyResults.SIM, ParticularResults.SIM",
    "",
    "LIST/",
    "  Project files executed by AquaCrop:",
    "  - Single runs (*.PRO)",
    "  - Multiple runs (*.PRM) - for multi-year simulations or scenarios",
    "",
    "PARAM/",
    "  Program parameters (*.PP1, *.PPn) - optional",
    "  If absent, default FAO parameters are used",
    "",
    "OUTP/",
    "  Simulation outputs (created after running, depends on *.SIM files):",
    "  - *Season.OUT: seasonal results (biomass, yield, water balance)",
    "  - *Day.OUT: daily outputs (optional)",
    "  - *Harvests.OUT: multiple cuttings data (forage crops)",
    "  - *Evaluation.OUT: statistics when field observations available",
    "  - ListProjectsLoaded.OUT: status report of simulation runs",
    "",
    "File Extensions Reference",
    "-------------------------",
    "",
    "Climate: .Tnx/.TMP (temperature), .PLU (rainfall), .ETo (reference ET),",
    "         .CLI (climate master file), .CO2 (atmospheric CO2)",
    "",
    "Crop/Management: .CRO (crop), .CAL (calendar), .MAN (management), .IRR (irrigation)",
    "",
    "Soil: .SOL (soil profile), .SW0 (initial water), .GWT (groundwater table)",
    "",
    "Simulation: .SIM (simulation config), .PRO (single project), .PRM (multiple runs),",
    "            .PP1/.PPn (program parameters)",
    "",
    "Data/Output: .OBS (field observations), .OFF (off-season), .OUT (outputs)",
    "",
    "Getting Help",
    "------------",
    "",
    "aquacropr:",
    "  - Documentation: https://github.com/mwaongo/aquacropr",
    "  - In R: ?write_tnx, help(package='aquacropr'), example(write_prm)",
    "  - Vignettes: vignette('aquacropr')",
    "",
    "Common issues:",
    "  - Simulation fails: Check OUTP/ListProjectsLoaded.OUT for error details",
    "  - Format errors: Use aquacropr functions instead of manual editing",
    "  - Missing files: Verify paths in .PRM files are correct",
    "",
    "Resources",
    "---------",
    "",
    "aquacropr R Package:",
    "  Website: https://github.com/mwaongo/aquacropr",
    "  - Installation guide and troubleshooting",
    "  - Complete function reference with examples",
    "  - Vignettes and tutorials for common workflows",
    "",
    "  Key functions:",
    "    Input creation: write_tnx(), write_plu(), write_eto(), write_cli(),",
    "                    write_climate() [creates all climate files at once],",
    "                    write_cro(), write_sol(), write_swo(), write_man()",
    "    Project setup: write_prm(), write_prm_batch()",
    "    Execution: run_aquacrop()",
    "    Analysis: read_seas_out()",
    "",
    "AquaCrop Official Documentation:",
    "--------------------------------",
    "",
    "  Main website: https://www.fao.org/aquacrop",
    "  - Software downloads (Windows, Linux, macOS)",
    "  - User manuals and reference guides",
    "",
    "  Training materials:",
    "  https://www.fao.org/aquacrop/knowledge-resources/training-materials/en",
    "  - Tutorials and user guides",
    "  - Example datasets",
    "",
    "Citation:",
    "---------",
    "",
    "If you use aquacropr in your research, please cite:",
    "",
    paste0("Waongo, M., Ouedraogo, O.A.Y. (2025). aquacropr: R Tools for the FAO AquaCrop"),
    paste0("Crop Water Productivity Model. R package version ", pkg_version, "."),
    "https://github.com/mwaongo/aquacropr",
    "",
    "Raes, D., Steduto, P., Hsiao, T. C., & Fereres, E. (2009).",
    "  AquaCrop - The FAO Crop Model to Simulate Yield Response to Water:",
    "  II. Main Algorithms and Software Description. Agronomy Journal, 101(3),",
    "  438-447. https://doi.org/10.2134/agronj2008.0140s",
    ""
  )

  tryCatch(
    {
      writeLines(readme_content, readme_path)
      message(cli::symbol$tick, " Created README.txt")
    },
    error = function(e) {
      warning(
        "Failed to create README.txt: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(readme_path)
}
#' Install simulation template files
#' @keywords internal
.install_templates <- function(path, overwrite) {
  template_files <- c(
    "MaunaLoa.CO2",
    "ParticularResultsFullList.SIM"
  )

  simul_dest_dir <- file.path(path, "SIMUL")
  if (!dir.exists(simul_dest_dir)) {
    dir.create(simul_dest_dir, recursive = TRUE)
  }

  for (template_file in template_files) {
    template_src <- path_to_file(template_file)

    if (!is.null(template_src) && nzchar(template_src) && file.exists(template_src)) {
      template_dest <- file.path(simul_dest_dir, template_file)

      if (!file.exists(template_dest) || overwrite) {
        tryCatch(
          {
            invisible(file.copy(template_src, template_dest, overwrite = overwrite))
          },
          error = function(e) {
            warning(
              "Failed to copy template: ", template_file, "\n",
              "Error: ", conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    } else {
      warning(
        "Template file not found in package: ", template_file,
        call. = FALSE
      )
    }
  }

  message(cli::symbol$tick, " Simulation templates prepared in SIMUL/")
  invisible(TRUE)
}
