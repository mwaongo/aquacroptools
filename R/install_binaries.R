#' Install AquaCrop Binary
#'
#' Downloads and installs the AquaCrop executable for the requested operating
#' system. The function automatically detects the current platform, resolves
#' the requested AquaCrop version, caches downloaded archives, and installs
#' the executable in the selected directory.
#'
#' Use `version = "dev"` to compile the latest development version from source.
#'
#' @param version Character. AquaCrop version to install. If `NULL`, installs
#'   the latest supported binary release. Accepted formats include `"7.1"`,
#'   `"v7.1"`, and `"7.1.0"`. Use `"dev"` to compile from source.
#' @param os Character. Operating system: `"windows"`, `"linux"`, or `"macos"`.
#'   If `NULL`, detected automatically.
#' @param path Character. Installation directory. Defaults to the current
#'   working directory.
#' @param force Logical. If `TRUE`, reinstalls an existing executable.
#'   Default: `FALSE`.
#' @param compiler Character or `NULL`. Fortran compiler used only when
#'   `version = "dev"`. If `NULL`, detected automatically.
#' @param keep_source Logical. Keep source code after compilation when
#'   `version = "dev"`. Default: `FALSE`.
#'
#' @return Invisibly returns the installed AquaCrop version.
#'
#' @examples
#' \dontrun{
#' install_binaries()
#' install_binaries(version = "7.1", path = "~/aquacrop")
#' install_binaries(version = "dev", path = "~/aquacrop")
#' install_binaries(force = TRUE)
#' }
#'
#' @importFrom glue glue
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils download.file unzip
#' @export
install_binaries <- function(
  version = NULL,
  os = NULL,
  path = getwd(),
  force = FALSE,
  compiler = NULL,
  keep_source = FALSE
) {
  # Development build
  if (
    !is.null(version) &&
      length(version) == 1L &&
      !is.na(version) &&
      identical(tolower(as.character(version)), "dev")
  ) {
    message("Installing development version from source...")

    return(
      install_source(
        install_dir = path,
        compiler = compiler,
        keep_source = keep_source,
        force = force
      )
    )
  }

  # Validate arguments
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop(
      "`path` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (
    !is.logical(force) ||
      length(force) != 1L ||
      is.na(force)
  ) {
    stop(
      "`force` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  path <- path.expand(path)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(path)) {
    stop(
      "Unable to create installation directory:\n",
      path,
      call. = FALSE
    )
  }

  # Resolve platform
  os <- .resolve_aquacrop_os(os)

  # Resolve version
  release <- .resolve_aquacrop_version(version)

  version <- release$version
  tag <- release$tag

  # Define executable
  exe_name <- .aquacrop_exe_name(os)
  exe_path <- file.path(path, exe_name)

  # Handle existing installation
  if (file.exists(exe_path)) {
    if (!force) {
      message("Binary already exists: ", exe_path)
      message("Use force = TRUE to reinstall.")

      return(invisible(version))
    }

    message("Removing existing binary...")
    unlink(exe_path)

    if (file.exists(exe_path)) {
      stop(
        "Unable to remove existing AquaCrop executable:\n",
        exe_path,
        call. = FALSE
      )
    }
  }

  # Build download information
  archive_name <- .aquacrop_binary_archive(
    version = version,
    os = os
  )

  url <- .aquacrop_download_url(
    tag = tag,
    archive = archive_name
  )

  # Download archive
  archive <- .download_aquacrop_binary(
    url = url,
    archive = archive_name,
    force = force
  )

  # Install executable
  .install_aquacrop_archive(
    archive = archive,
    destination = path,
    executable = exe_name,
    os = os
  )

  # Validate installation
  if (!file.exists(exe_path)) {
    stop(
      "AquaCrop installation failed: executable was not created.\n",
      "Expected location: ",
      exe_path,
      call. = FALSE
    )
  }

  if (
    os != "windows" &&
      file.access(exe_path, mode = 1L) != 0L
  ) {
    stop(
      "AquaCrop executable was installed but is not executable:\n",
      exe_path,
      call. = FALSE
    )
  }

  message("Installed: ", exe_path)

  invisible(version)
}


# Version configuration

.AQUACROP_MIN_BINARY_VERSION <- "7.0"

# TODO: Replace the temporary version resolver below once AquaCrop release
# tags follow a consistent versioning convention.
#
# Some upstream releases currently use inconsistent tags such as "7.3_typo",
# making automatic latest-release discovery unreliable.
#
# Expected convention:
#
#   tag:     v7.3.1
#   archive: aquacrop-7.3.1-x86_64-{os}.zip
#
# Once upstream adopts a consistent convention:
#
# 1. Replace the active `.resolve_aquacrop_version()` with this implementation.
# 2. Delete `.AQUACROP_MAX_BINARY_VERSION`.
# 3. Restore `gh` as a package dependency if necessary.
#
# .resolve_aquacrop_version <- function(version = NULL) {
#
#   min_version <- .AQUACROP_MIN_BINARY_VERSION
#
#   latest_release <- tryCatch(
#     gh::gh("/repos/KUL-RSDA/AquaCrop/releases/latest"),
#     error = function(e) {
#       stop(
#         "Failed to fetch release information from GitHub.\n",
#         "Check your internet connection or try again later.\n",
#         "Error: ",
#         conditionMessage(e),
#         call. = FALSE
#       )
#     }
#   )
#
#   latest_tag <- latest_release$tag_name
#   latest_version <- sub("^v", "", latest_tag)
#
#   if (!grepl(
#     "^[0-9]+(?:\\.[0-9]+){1,2}$",
#     latest_version
#   )) {
#     stop(
#       "Latest AquaCrop release tag is not a valid semantic version: ",
#       latest_tag,
#       call. = FALSE
#     )
#   }
#
#   if (is.null(version)) {
#     message(
#       "Installing latest available binary version: ",
#       latest_version
#     )
#
#     return(
#       list(
#         version = latest_version,
#         tag = latest_tag
#       )
#     )
#   }
#
#   if (length(version) != 1L || is.na(version)) {
#     stop(
#       "`version` must contain exactly one version.",
#       call. = FALSE
#     )
#   }
#
#   version <- sub(
#     "^v",
#     "",
#     as.character(version),
#     ignore.case = TRUE
#   )
#
#   if (!grepl(
#     "^[0-9]+(?:\\.[0-9]+){0,2}$",
#     version
#   )) {
#     stop(
#       "Invalid AquaCrop version: ",
#       version,
#       ". Expected a version such as \"7.0\", \"7.1\", or \"7.1.0\".",
#       call. = FALSE
#     )
#   }
#
#   requested <- numeric_version(version)
#   minimum <- numeric_version(min_version)
#   latest <- numeric_version(latest_version)
#
#   if (requested < minimum) {
#     message(
#       "Requested version (",
#       version,
#       ") is below the minimum supported version (",
#       min_version,
#       "); falling back to latest: ",
#       latest_version,
#       "."
#     )
#
#     return(
#       list(
#         version = latest_version,
#         tag = latest_tag
#       )
#     )
#   }
#
#   if (requested > latest) {
#     message(
#       "Requested version (",
#       version,
#       ") is above the latest available version (",
#       latest_version,
#       "); falling back to latest."
#     )
#
#     return(
#       list(
#         version = latest_version,
#         tag = latest_tag
#       )
#     )
#   }
#
#   list(
#     version = version,
#     tag = .get_version_tag(version)
#   )
# }

# Temporary fallback while upstream release tags remain inconsistent.
.AQUACROP_MAX_BINARY_VERSION <- "7.2"


# Resolve operating system
.resolve_aquacrop_os <- function(os = NULL) {
  if (is.null(os)) {
    os <- get_os()
  }

  if (
    !is.character(os) ||
      length(os) != 1L ||
      is.na(os)
  ) {
    stop(
      "`os` must be one of: windows, linux, macos.",
      call. = FALSE
    )
  }

  match.arg(
    tolower(os),
    choices = c("windows", "linux", "macos")
  )
}


# Resolve AquaCrop version
.resolve_aquacrop_version <- function(version = NULL) {
  min_version <- .AQUACROP_MIN_BINARY_VERSION
  max_version <- .AQUACROP_MAX_BINARY_VERSION

  if (is.null(version)) {
    message(
      "Installing latest available binary version: ",
      max_version
    )

    return(
      list(
        version = max_version,
        tag = .get_version_tag(max_version)
      )
    )
  }

  if (
    length(version) != 1L ||
      is.na(version)
  ) {
    stop(
      "`version` must contain exactly one version.",
      call. = FALSE
    )
  }

  version <- sub(
    "^v",
    "",
    as.character(version),
    ignore.case = TRUE
  )

  if (
    !grepl(
      "^[0-9]+(?:\\.[0-9]+){0,2}$",
      version
    )
  ) {
    stop(
      "Invalid AquaCrop version: ",
      version,
      ". Expected a version such as \"7.0\", \"7.1\", or \"7.1.0\".",
      call. = FALSE
    )
  }

  requested <- numeric_version(version)
  minimum <- numeric_version(min_version)
  maximum <- numeric_version(max_version)

  if (requested < minimum) {
    message(
      "Requested version (",
      version,
      ") is below the minimum supported version (",
      min_version,
      "); falling back to ",
      max_version,
      "."
    )

    version <- max_version
  } else if (requested > maximum) {
    message(
      "Requested version (",
      version,
      ") is above the latest supported binary version (",
      max_version,
      "); falling back to ",
      max_version,
      "."
    )

    version <- max_version
  }

  list(
    version = version,
    tag = .get_version_tag(version)
  )
}


# Build archive name
.aquacrop_binary_archive <- function(version, os) {
  glue::glue(
    "aquacrop-{version}-x86_64-{os}.zip"
  )
}


# Build download URL
.aquacrop_download_url <- function(tag, archive) {
  glue::glue(
    "https://github.com/KUL-RSDA/AquaCrop/releases/download/{tag}/{archive}"
  )
}


# Get cache directory
.aquacrop_cache_dir <- function() {
  cache_dir <- rappdirs::user_cache_dir("aquacropr")

  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (!dir.exists(cache_dir)) {
    stop(
      "Unable to create AquaCrop cache directory:\n",
      cache_dir,
      call. = FALSE
    )
  }

  cache_dir
}


# Validate ZIP archive
.is_valid_zip <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  info <- file.info(path)

  if (is.na(info$size) || info$size <= 0) {
    return(FALSE)
  }

  tryCatch(
    {
      contents <- utils::unzip(
        path,
        list = TRUE
      )

      nrow(contents) > 0L
    },
    error = function(e) {
      FALSE
    }
  )
}


# Download binary archive
.download_aquacrop_binary <- function(
  url,
  archive,
  force = FALSE
) {
  cache_dir <- .aquacrop_cache_dir()
  cached_zip <- file.path(cache_dir, archive)

  # Use valid cached archive
  if (
    file.exists(cached_zip) &&
      !force
  ) {
    if (.is_valid_zip(cached_zip)) {
      message("Using cached download: ", archive)
      return(cached_zip)
    }

    message("Cached archive is corrupted; downloading again.")
    unlink(cached_zip)
  }

  if (
    force &&
      file.exists(cached_zip)
  ) {
    unlink(cached_zip)
  }

  # Download to temporary file
  tmp_zip <- tempfile(
    pattern = "aquacrop-",
    tmpdir = cache_dir,
    fileext = ".zip"
  )

  on.exit(
    {
      if (file.exists(tmp_zip)) {
        unlink(tmp_zip)
      }
    },
    add = TRUE
  )

  message("Downloading AquaCrop binary...")

  old_timeout <- getOption("timeout")

  if (is.null(old_timeout)) {
    old_timeout <- 60
  }

  on.exit(
    options(timeout = old_timeout),
    add = TRUE
  )

  options(
    timeout = max(300, old_timeout)
  )

  tryCatch(
    {
      status <- utils::download.file(
        url = url,
        destfile = tmp_zip,
        mode = "wb",
        quiet = TRUE
      )

      if (!identical(status, 0L)) {
        stop(
          "download.file() returned status ",
          status,
          "."
        )
      }
    },
    error = function(e) {
      stop(
        "Failed to download AquaCrop binary.\n",
        "URL: ",
        url,
        "\nError: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Validate downloaded archive
  if (!.is_valid_zip(tmp_zip)) {
    stop(
      "Downloaded AquaCrop archive is invalid or corrupted.\n",
      "URL: ",
      url,
      call. = FALSE
    )
  }

  # Store archive in cache
  moved <- file.rename(
    tmp_zip,
    cached_zip
  )

  if (!moved) {
    copied <- file.copy(
      tmp_zip,
      cached_zip,
      overwrite = TRUE
    )

    if (!copied) {
      stop(
        "Unable to store AquaCrop archive in cache:\n",
        cached_zip,
        call. = FALSE
      )
    }
  }

  message("Downloaded: ", archive)

  cached_zip
}


# Install binary archive
.install_aquacrop_archive <- function(
  archive,
  destination,
  executable,
  os
) {
  # Extract to temporary directory
  extract_dir <- tempfile(
    pattern = "aquacrop-extract-"
  )

  dir.create(
    extract_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    unlink(
      extract_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  message("Extracting...")

  tryCatch(
    {
      utils::unzip(
        archive,
        exdir = extract_dir
      )
    },
    error = function(e) {
      stop(
        "Failed to extract AquaCrop archive.\n",
        "Error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Locate executable
  binary <- .find_aquacrop_executable(
    directory = extract_dir,
    executable = executable
  )

  target <- file.path(
    destination,
    executable
  )

  # Copy executable
  copied <- file.copy(
    binary,
    target,
    overwrite = TRUE
  )

  if (!copied) {
    stop(
      "Unable to install AquaCrop executable:\n",
      target,
      call. = FALSE
    )
  }

  # Set executable permission
  if (os != "windows") {
    Sys.chmod(
      target,
      mode = "0755"
    )
  }

  invisible(target)
}


# Find executable in extracted archive
.find_aquacrop_executable <- function(
  directory,
  executable
) {
  files <- list.files(
    directory,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )

  candidates <- files[
    tolower(basename(files)) == tolower(executable)
  ]

  if (length(candidates) == 0L) {
    stop(
      "AquaCrop executable was not found after extraction.\n",
      "Expected file: ",
      executable,
      call. = FALSE
    )
  }

  if (length(candidates) > 1L) {
    warning(
      "Multiple AquaCrop executables were found in the archive; ",
      "using the first one:\n",
      candidates[[1L]],
      call. = FALSE
    )
  }

  candidates[[1L]]
}
