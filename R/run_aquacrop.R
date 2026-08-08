#' Run an AquaCrop simulation
#'
#' Executes the AquaCrop executable in the current working directory. The
#' directory must contain the AquaCrop executable (`aquacrop.exe` on Windows,
#' `aquacrop` on Linux/macOS) together with a valid AquaCrop project structure.
#'
#' Before execution, all files in the `OUTP/` directory are removed to prevent
#' mixing outputs from previous simulations. Temporary files created by
#' AquaCrop (`AllDone.OUT` and `ListProjectsLoaded.OUT`) are automatically
#' removed after execution.
#'
#' @param verbose Logical. Should progress messages be printed?
#'   Defaults to `TRUE`.
#'
#' @return Invisibly returns the AquaCrop exit status (`0` indicates success).
#'
#' @seealso [init_aquacrop()], [install_binaries()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' init_aquacrop("~/my-project")
#' setwd("~/my-project")
#'
#' run_aquacrop()
#' run_aquacrop(verbose = FALSE)
#' }
run_aquacrop <- function(verbose = TRUE) {
  # determine executable name based on os

  os <- get_os()
  exe_name <- .aquacrop_exe_name(os)

  cmd <- if (os == "windows") {
    exe_name
  } else {
    paste0("./", exe_name)
  }

  # validate if project dir and executable can be runned

  if (!file.exists(exe_name)) {
    required_dirs <- c(
      "CLIMATE",
      "CROP",
      "SOIL",
      "SIMUL",
      "OUTP",
      "MANAGEMENT",
      "PARAM",
      "LIST"
    )

    if (all(dir.exists(required_dirs))) {
      stop(
        "AquaCrop executable not found: ",
        exe_name,
        ".\n\n",
        "This appears to be a valid AquaCrop project, but the executable ",
        "is missing.\n\n",
        "Install it with:\n",
        "  install_binaries(path = \".\")",
        call. = FALSE
      )
    }

    stop(
      "AquaCrop executable not found: ",
      exe_name,
      ".\n\n",
      "This directory does not appear to be a valid AquaCrop project.\n\n",
      "Possible solutions:\n",
      "  - Create a new project with init_aquacrop().\n",
      "  - Install the AquaCrop executable with install_binaries().\n",
      "  - Change to the correct project directory using setwd().",
      call. = FALSE
    )
  }

  # set binary executable permission on unix systems

  if (os != "windows") {
    Sys.chmod(exe_name, mode = "0755")
  }

  # clean OUTP dir
  on.exit(
    {
      tmp_files <- c(
        "AllDone.OUT",
        "ListProjectsLoaded.OUT"
      )

      tmp_files <- tmp_files[file.exists(tmp_files)]

      if (length(tmp_files) > 0L) {
        fs::file_delete(tmp_files)
      }
    },
    add = TRUE
  )

  # clean previous output

  if (dir.exists("OUTP")) {
    out_files <- fs::dir_ls("OUTP", recurse = FALSE)

    if (length(out_files) > 0L) {
      fs::file_delete(out_files)
    }
  }

  # run aquacrop binary
  if (verbose) {
    message("Running AquaCrop in ", normalizePath(getwd()))
  }

  output <- system2(
    command = cmd,
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  # report running result
  if (status == 0L) {
    if (verbose) {
      message("AquaCrop completed successfully.")
      message("Results written to OUTP/.")
    }
  } else {
    warning(
      paste0(
        "AquaCrop exited with status ",
        status,
        ".",
        if (length(output) > 0L) {
          paste0(
            "\n\nAquaCrop output:\n",
            paste(output, collapse = "\n")
          )
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }

  invisible(status)
}
