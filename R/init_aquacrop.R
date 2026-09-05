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
    version_info <- "unknown"
  } else {
    tag <- .get_version_tag(version)
    version_info <- paste0(
      version, " (https://github.com/KUL-RSDA/AquaCrop/releases/tag/", tag, ")"
    )
  }

  # Package version
  pkg_version <- tryCatch(
    as.character(utils::packageVersion(pkg_name)),
    error = function(e) "development"
  )

  # Directory descriptions -- single source of truth; edit here only.
  dir_descriptions <- c(
    CLIMATE    = "Rainfall, temperature, ETo, climate master files (.PLU .Tnx .ETo .CLI)",
    CROP       = "Crop parameter files (.CRO)",
    CAL        = "Sowing/onset calendar files (.CAL)",
    SOIL       = "Soil profile, initial water, groundwater files (.SOL .SW0 .GWT)",
    MANAGEMENT = "Field management and irrigation files (.MAN .IRR)",
    LIST       = "Project master files run by AquaCrop (.PRM)",
    PARAM      = "Program parameter files (.PPn) -- optional, FAO defaults used if absent",
    SIMUL      = "Simulation configuration files (.SIM)",
    OBS        = "Field observation files (.OBS), for model evaluation",
    OUTP       = "Simulation outputs, written here after running (*Season.OUT, *Day.OUT, ...)"
  )

  # One line per directory: name padded to a fixed column, then description.
  dir_listing <- vapply(dirs, function(d) {
    desc <- dir_descriptions[[d]]
    sprintf("%-13s%s", paste0(d, "/"), if (!is.null(desc)) desc else "")
  }, character(1))

  # Citation text generated live from the package's own metadata (Authors@R,
  # Title, Version, Date, URL in DESCRIPTION) via citation(), so it tracks
  # authorship/version/year automatically instead of being duplicated here.
  cite_lines <- tryCatch(
    {
      txt <- format(utils::citation(pkg_name), style = "text")
      txt <- gsub("_", "", txt)  # drop bibentry's plain-text italic markers
      strsplit(txt, "\n")[[1]]
    },
    error = function(e) paste0('See citation("', pkg_name, '") for how to cite this package.')
  )

  readme_content <- c(
    "AquaCrop Project",
    "================",
    paste("Created:    ", Sys.Date()),
    paste("Path:       ", path),
    paste("Executable: ", exe_name),
    paste("AquaCrop:   ", version_info),
    paste0("Generated by: ", pkg_name, " v", pkg_version, "::init_aquacrop()"),
    "",
    "Directories",
    "-----------",
    dir_listing,
    "",
    "Next step",
    "---------",
    paste0("  setwd(\"", path, "\")"),
    "",
    "  Then write your inputs -- write_climate(), write_cro(), write_sol(),",
    "  write_man(), write_cal(), write_prm() -- and call run_aquacrop().",
    "  Outputs land in OUTP/ and can be read with read_season_out().",
    "",
    "Getting help",
    "------------",
    "  ?init_aquacrop  ?write_prm  ?run_aquacrop  ?read_season_out",
    "  help(package = \"aquacropr\")",
    "  vignette(package = \"aquacropr\")   # if vignettes were built with this install",
    "  https://github.com/mwaongo/aquacropr",
    "",
    "Troubleshooting",
    "---------------",
    "  Simulation failed? Check OUTP/ListProjectsLoaded.OUT for the reported error.",
    "",
    "Citation",
    "--------",
    paste0("  ", cite_lines),
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
