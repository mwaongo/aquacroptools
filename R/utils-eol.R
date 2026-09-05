#' Detect Operating System
#'
#' Detects the current operating system and returns a standardized name
#' compatible with AquaCrop binary distributions.
#'
#' @return Character string: one of "windows", "linux", or "macos".
#'
#' @details
#' The function uses a two-step detection process:
#' \enumerate{
#'   \item Attempts to use \code{Sys.info()['sysname']} (most reliable)
#'   \item Falls back to \code{.Platform$OS.type} and \code{R.version$os} for
#'         edge cases where \code{Sys.info()} is unavailable
#' }
#'
#' Operating system names are normalized to match AquaCrop conventions:
#' \itemize{
#'   \item Darwin → macos
#'   \item Linux → linux
#'   \item Windows → windows
#' }

#' @keywords internal
get_os <- function() {
  # The OS cannot change within a session; detecting it once matters because
  # the climate writers call this for every file they produce.
  cached <- .aquacropr_cache$os
  if (!is.null(cached)) return(cached)

  os <- .detect_os()
  .aquacropr_cache$os <- os
  os
}


#' Uncached Operating System Detection
#' @noRd
.detect_os <- function() {
  # Try Sys.info() first (most reliable)
  sysinf <- Sys.info()

  if (!is.null(sysinf)) {
    os <- sysinf[['sysname']]  # Double brackets for single value
  } else {
    # Fallback for mystery machines
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os, ignore.case = TRUE)) {
      os <- "Darwin"
    } else if (grepl("linux", R.version$os, ignore.case = TRUE)) {
      os <- "Linux"
    }
  }

  # Normalize to AquaCrop conventions
  os <- switch(
    tolower(os),
    "darwin" = "macos",
    "linux" = "linux",
    "windows" = "windows",
    stop("Unsupported OS: ", os)
  )

  return(os)
}


#' AquaCrop Executable Filename for an OS
#'
#' @param os Character. One of "windows", "linux", "macos". Defaults to
#'   the detected current OS via get_os().
#' @return Character. "aquacrop.exe" on Windows, "aquacrop" otherwise.
#'
#' @noRd
.aquacrop_exe_name <- function(os = get_os()) {
  if (os == "windows") "aquacrop.exe" else "aquacrop"
}


#' Get End-of-Line Character
#'
#' @description
#' Internal function to return the appropriate end-of-line character sequence
#' based on the operating system specification.
#'
#' @param eol Character string specifying the end-of-line style.
#'   Options: "windows", "unix", "linux", or "macos". If `NULL` (default), eol style is auto-detected.
#'
#' @return Character string containing the EOL sequence:
#'   \itemize{
#'     \item Windows: "\\r\\n" (carriage return + line feed)
#'     \item Unix/Linux/macOS: "\\n" (line feed only)
#'   }
#'
#' @keywords internal
#' @noRd
.get_eol <- function(eol = NULL) {
    if (is.null(eol)) eol <- get_os()
    eol <- match.arg(tolower(eol), choices = c("windows", "linux", "macos"))

    sep <- ifelse(
      test = eol %in% c("linux", "macos"),
      yes = "\n",
      no = "\r\n"
      )
    return(sep)
}
