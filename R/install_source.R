#' Detect Fortran Compiler via R CMD config FC
#'
#' Calls R CMD config FC to retrieve the Fortran compiler R itself was built
#' with. This is the canonical cross-platform approach: it reads directly from
#' R own Makeconf and requires no manual PATH handling.
#'
#' @return Character. Full path or name of the detected Fortran compiler.
#'   Stops with an informative message if none is found.
#'
#' @noRd
.detect_fc <- function() {
  compiler <- tryCatch(
    system2(
      "R",
      args = c("CMD", "config", "FC"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) ""
  )
  compiler <- compiler[nzchar(compiler)][1]

  if (is.na(compiler) || !nzchar(compiler)) {
    stop(
      "Could not detect a Fortran compiler via 'R CMD config FC'.\n",
      "Please specify one explicitly, e.g.: compiler = 'gfortran'",
      call. = FALSE
    )
  }

  compiler
}


#' Extract Fortran Compiler Binary Name from FC String
#'
#' R CMD config FC may return flags alongside the binary name (e.g.,
#' "gfortran -arch x86_64" on macOS). This helper extracts only the first
#' token for use with Sys.which() and file.exists().
#'
#' @param compiler Character. Raw FC string.
#' @return Character. Binary name or path, stripped of flags.
#'
#' @noRd
.fc_binary <- function(compiler) {
  trimws(strsplit(compiler, " ")[[1]][1])
}


#' Find GNU Make Executable
#'
#' Locates the make executable, accounting for platform differences.
#' On Windows, also checks for mingw32-make provided by Rtools.
#'
#' @return Character. Full path to the make executable.
#'   Stops with an informative message if not found.
#'
#' @noRd
find_make <- function() {
  os <- get_os()

  if (os == "windows") {
    candidates <- Sys.which(c("make", "mingw32-make"))
    make <- candidates[nzchar(candidates)][1]
  } else {
    make <- Sys.which("make")
  }

  if (is.na(make) || !nzchar(make)) {
    stop(
      "'make' not found. Please install build tools",
      if (os == "windows") " (Rtools)" else "",
      ".",
      call. = FALSE
    )
  }

  make
}


#' Check System Dependencies for Building AquaCrop
#'
#' Verifies that required build tools (make and a Fortran compiler) are
#' available on the system. The Fortran compiler is auto-detected via
#' R CMD config FC, then by searching known installation paths on macOS.
#'
#' Returns the detected values so that install_source can reuse them
#' without triggering redundant detection calls.
#'
#' @param verbose Logical. If TRUE (default), prints detected tool versions.
#'
#' @return Invisibly returns a named list with two elements:
#'   compiler (Character) and make (Character).
#'   Stops with an informative message if any dependency is missing.
#'
#' @noRd
check_sys_deps <- function(verbose = TRUE) {
  deps_ok <- TRUE
  make_out <- ""
  fc_out <- ""

  # Check make
  make_out <- tryCatch(find_make(), error = function(e) "")
  if (!nzchar(make_out)) {
    message("error: GNU Make not found")
    deps_ok <- FALSE
  } else if (verbose) {
    make_version <- tryCatch(
      system2("make", "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) "unknown"
    )
    message("make: ", make_out)
    if (make_version != "unknown") message("  ", make_version)
  }

  # Detect Fortran compiler
  fc_out <- tryCatch(.detect_fc(), error = function(e) "")
  if (!nzchar(fc_out)) {
    message("error: no Fortran compiler detected via R CMD config FC")
    deps_ok <- FALSE
  } else if (verbose) {
    compiler_bin <- .fc_binary(fc_out)
    fc_version <- tryCatch(
      system2(compiler_bin, "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) {
        tryCatch(
          system2(fc_out, "--version", stdout = TRUE, stderr = FALSE)[1],
          error = function(e2) "unknown"
        )
      }
    )
    message("ok: ", compiler_bin, ": ", fc_out)
    if (fc_version != "unknown") message("  ", fc_version)
  }

  if (!deps_ok) {
    stop(
      "\nMissing required dependencies. Please install:\n\n",
      "Ubuntu/Debian:\n",
      "  sudo apt-get install make gfortran\n\n",
      "Fedora/RHEL:\n",
      "  sudo dnf install make gcc-gfortran\n\n",
      "macOS (official R toolchain, recommended):\n",
      "  https://mac.r-project.org/tools/\n\n",
      "macOS (Homebrew):\n",
      "  brew install make gcc\n\n",
      "Windows:\n",
      "  Install Rtools (includes make and gfortran)",
      call. = FALSE
    )
  }

  invisible(list(compiler = fc_out, make = make_out))
}


#' Download AquaCrop Source Code
#'
#' Downloads and extracts AquaCrop source code from the GitHub main branch.
#'
#' @param dest_dir Character. Directory where source will be extracted.
#'   Default: a temporary directory.
#' @param url Character. URL to download source code.
#'   Default: official GitHub repository main branch.
#' @param timeout Integer. Download timeout in seconds. Default: 120.
#' @param verbose Logical. If TRUE (default), prints download progress.
#'
#' @return Character. Path to the extracted AquaCrop source directory.
#'
#' @examples
#' \dontrun{
#' source_dir <- download_source()
#' source_dir <- download_source(dest_dir = "~/builds")
#' }
#'
#' @export
download_source <- function(
  dest_dir = fs::path_temp("aquacrop_source"),
  url = "https://github.com/KUL-RSDA/AquaCrop/archive/refs/heads/main.zip",
  timeout = 120,
  verbose = TRUE
) {
  fs::dir_create(dest_dir, recurse = TRUE)
  zip_file <- fs::path(dest_dir, "aquacrop_main.zip")

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  if (verbose) {
    message("Downloading from: ", url)
  }
  tryCatch(
    utils::download.file(url, zip_file, mode = "wb", quiet = !verbose),
    error = function(e) {
      stop("Failed to download AquaCrop source: ", e$message, call. = FALSE)
    }
  )

  if (verbose) {
    message("Extracting...")
  }
  utils::unzip(zip_file, exdir = dest_dir)

  extracted_name <- fs::path(dest_dir, "AquaCrop-main")
  final_dir <- fs::path(dest_dir, "AquaCrop")

  if (fs::dir_exists(extracted_name)) {
    if (fs::dir_exists(final_dir)) {
      fs::dir_delete(final_dir)
    }
    fs::file_move(extracted_name, final_dir)
  }

  fs::file_delete(zip_file)

  if (!fs::dir_exists(final_dir)) {
    stop("Failed to locate extracted source directory.", call. = FALSE)
  }

  final_dir
}


#' Build AquaCrop from Source
#'
#' Compiles the AquaCrop executable from source code using make and a
#' Fortran compiler. Accepts a pre-resolved compiler to avoid redundant
#' detection calls when invoked from install_source.
#'
#' @param source_dir Character. Path to the AquaCrop source directory.
#' @param compiler Character or NULL. Fortran compiler binary name or full
#'   path, optionally including flags (e.g., "gfortran -arch x86_64").
#'   If NULL (default), auto-detected via .detect_fc().
#' @param target Character. Build target passed to make. One of "all"
#'   (default, produces both executable and library), "bin" (executable
#'   only), or "lib" (library only).
#' @param verbose Logical. If TRUE (default), prints build information.
#'
#' @return Character. Path to the compiled executable.
#'
#' @importFrom withr with_dir
#'
#' @examples
#' \dontrun{
#' exe_path <- build_source("AquaCrop")
#' exe_path <- build_source("AquaCrop", compiler = "ifort", target = "bin")
#' }
#'
#' @export
build_source <- function(
  source_dir,
  compiler = NULL,
  target = "all",
  verbose = TRUE
) {
  if (!target %in% c("all", "bin", "lib")) {
    stop("target must be 'all', 'bin', or 'lib'.", call. = FALSE)
  }

  source_dir <- fs::path_expand(source_dir)
  src_dir <- fs::path(source_dir, "src")

  if (!fs::dir_exists(src_dir)) {
    stop("Source directory not found: ", src_dir, call. = FALSE)
  }

  os <- get_os()
  make <- find_make()

  if (is.null(compiler)) {
    compiler <- .detect_fc()
  }

  compiler_bin <- .fc_binary(compiler)
  if (!nzchar(Sys.which(compiler_bin)) && !file.exists(compiler_bin)) {
    stop("Fortran compiler not found: ", compiler_bin, call. = FALSE)
  }

  make_args <- c(target, paste0("FC=", compiler))
  if (os == "windows") {
    make_args <- c(make_args, "CPPFLAGS=-D_WINDOWS")
  }

  if (verbose) {
    message(
      "Building AquaCrop (",
      os,
      ")\n",
      "  directory : ",
      src_dir,
      "\n",
      "  make      : ",
      make,
      "\n",
      "  compiler  : ",
      compiler,
      "\n",
      "  target    : ",
      target
    )
  }

  result <- withr::with_dir(src_dir, {
    system2(command = make, args = make_args, stdout = TRUE, stderr = TRUE)
  })

  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "Build failed with exit code ",
      status,
      ":\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  exe_name <- .aquacrop_exe_name(os)
  exe_path <- fs::path(src_dir, exe_name)

  if (!fs::file_exists(exe_path)) {
    stop(
      "Build succeeded but executable not found:\n  ",
      exe_path,
      call. = FALSE
    )
  }

  if (verbose) {
    message("Build successful: ", exe_path)
  }

  exe_path
}


#' Install AquaCrop from Source
#'
#' Complete workflow to download, compile, and install the AquaCrop executable
#' from source. Always compiles from the latest development version (main
#' branch). This function is called internally by
#' install_binaries(version = "dev").
#'
#' @param install_dir Character. Destination directory for the AquaCrop
#'   executable. Default: current working directory.
#' @param compiler Character or NULL. Fortran compiler to use. If NULL
#'   (default), auto-detected once via R CMD config FC and reused across
#'   all steps. Can be set explicitly as a binary name (e.g., "gfortran"),
#'   a versioned name (e.g., "gfortran-14"), or a full absolute path
#'   (e.g., "/opt/gfortran/bin/gfortran").
#' @param keep_source Logical. If TRUE, keeps source code after compilation.
#'   Default: FALSE.
#' @param force Logical. If TRUE, overwrites existing installation.
#'   Default: FALSE.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently except for errors.
#'
#' @details
#' System requirements on Linux and macOS: GNU Make (>= 3.82) and a Fortran
#' compiler (gfortran >= 6.4.0 or ifort >= 18.0.1). On macOS, the official
#' R toolchain from mac.r-project.org/tools is recommended. On Windows,
#' installing Rtools is sufficient as it provides both make and gfortran.
#'
#' The function detects the Fortran compiler once in step 1 and reuses the
#' result in step 3, avoiding redundant calls to R CMD config FC.
#'
#' @return Invisibly returns "dev" as the version string.
#'
#' @examples
#' \dontrun{
#' # Usually called via install_binaries
#' install_binaries(version = "dev", path = "~/aquacrop")
#'
#' # Install to current directory with auto-detected compiler
#' install_source()
#'
#' # Install to specific directory, silent
#' install_source(install_dir = "~/aquacrop", verbose = FALSE)
#'
#' # Keep source code after compilation
#' install_source(install_dir = "~/aquacrop", keep_source = TRUE)
#'
#' # Force reinstall with explicit compiler path
#' install_source(
#'   install_dir = "~/aquacrop",
#'   compiler    = "/opt/gfortran/bin/gfortran",
#'   force       = TRUE
#' )
#' }
#'
#' @export
install_source <- function(
  install_dir = getwd(),
  compiler = NULL,
  keep_source = FALSE,
  force = FALSE,
  verbose = TRUE
) {
  install_dir <- fs::path_expand(install_dir)
  fs::dir_create(install_dir, recurse = TRUE)

  exe_name <- .aquacrop_exe_name()
  final_exe <- fs::path(install_dir, exe_name)

  if (fs::file_exists(final_exe) && !force) {
    message("AquaCrop already installed at: ", final_exe)
    message("Use force = TRUE to reinstall.")
    return(invisible("dev"))
  }

  # Step 1: check build tools, detect compiler once
  if (verbose) {
    message("[1/4] Checking dependencies...")
  }
  deps <- check_sys_deps(verbose = FALSE)

  if (is.null(compiler)) {
    compiler <- deps$compiler
  }
  if (verbose) {
    message("      compiler: ", .fc_binary(compiler))
  }

  # Step 2: download
  if (verbose) {
    message("[2/4] Downloading source...")
  }
  temp_dir <- fs::path_temp("aquacrop_build")
  source_dir <- download_source(dest_dir = temp_dir, verbose = FALSE)

  # Step 3: compile
  if (verbose) {
    message("[3/4] Compiling...")
  }
  exe_path <- build_source(
    source_dir = source_dir,
    compiler = compiler,
    verbose = FALSE
  )

  # Step 4: install
  if (!fs::file_exists(exe_path)) {
    stop("Compilation succeeded but executable not found.", call. = FALSE)
  }
  fs::file_copy(exe_path, final_exe, overwrite = TRUE)
  if (get_os() != "windows") {
    fs::file_chmod(final_exe, "755")
  }

  if (!keep_source) {
    fs::dir_delete(temp_dir)
  }

  if (verbose) {
    message("[4/4] Installed: ", final_exe)
    if (keep_source) message("      source  : ", source_dir)
  }

  invisible("dev")
}
