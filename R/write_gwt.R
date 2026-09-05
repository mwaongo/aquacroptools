#' Write AquaCrop Groundwater Table (.GWT) File
#'
#' Creates an AquaCrop groundwater table file (.GWT) describing the depth
#' and salinity of the groundwater table. Three modes are supported:
#' no groundwater table (code 0), constant depth and salinity (code 1),
#' and variable depth and salinity over time (code 2).
#'
#' @param site_name Character. Name of the GWT file (without extension).
#'   Typically the station name.
#' @param code Integer. Groundwater table mode:
#'   0 = no groundwater table,
#'   1 = constant depth and salinity,
#'   2 = variable depth and salinity.
#' @param gwt_data For code 0: ignored, can be NULL.
#'   For code 1: a data.frame with one row and columns:
#'   \describe{
#'     \item{day}{Integer. Day number (must be 1 for constant mode).}
#'     \item{depth}{Numeric. Depth below soil surface in meters.}
#'     \item{ecw}{Numeric. Electrical conductivity in dS/m.}
#'   }
#'   For code 2: a data.frame with columns:
#'   \describe{
#'     \item{day}{Integer. Day number from reference date.}
#'     \item{depth}{Numeric. Depth below soil surface in meters.}
#'     \item{ecw}{Numeric. Electrical conductivity in dS/m.}
#'   }
#' @param path Output directory path for GWT files. Default: "SOIL/".
#' @param start_day Integer. First day of observations (code 2 only).
#'   Default: 1.
#' @param start_month Integer. First month of observations (code 2 only).
#'   Default: 1.
#' @param start_year Integer. First year of observations (code 2 only).
#'   Use 1901 if not linked to a specific year. Default: 1901.
#' @param description Character. Description written on the first line.
#'   Default: auto-generated based on code and data.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#'
#' @return Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' # Code 0: no groundwater table
#' write_gwt(site_name = "station_01", code = 0)
#'
#' # Code 1: constant depth and salinity
#' write_gwt(
#'   site_name = "station_01",
#'   code     = 1,
#'   gwt_data = data.frame(day = 1, depth = 1.50, ecw = 1.5)
#' )
#'
#' # Code 2: variable depth and salinity
#' write_gwt(
#'   site_name    = "station_01",
#'   code        = 2,
#'   gwt_data    = data.frame(
#'     day   = c(50, 100, 200, 300),
#'     depth = c(1.00, 2.00, 3.00, 1.50),
#'     ecw   = c(1.0,  2.0,  3.0,  1.7)
#'   ),
#'   start_day   = 1,
#'   start_month = 1,
#'   start_year  = 2000
#' )
#' }
#'
#' @family AquaCrop file writers
#' @export
write_gwt <- function(
  site_name,
  code,
  gwt_data = NULL,
  path = "SOIL/",
  start_day = 1,
  start_month = 1,
  start_year = 1901,
  description = NULL,
  version = 7.1,
  eol = NULL
) {
  # ---- Input validation ----
  if (!code %in% 0:2) {
    stop("code must be 0, 1, or 2.", call. = FALSE)
  }

  if (code %in% 1:2) {
    if (is.null(gwt_data)) {
      stop("gwt_data is required when code is 1 or 2.", call. = FALSE)
    }
    if (!is.data.frame(gwt_data)) {
      stop("gwt_data must be a data.frame.", call. = FALSE)
    }
    if (!all(c("day", "depth", "ecw") %in% names(gwt_data))) {
      stop("gwt_data must have columns: day, depth, ecw.", call. = FALSE)
    }
    if (nrow(gwt_data) == 0) {
      stop("gwt_data must have at least one row.", call. = FALSE)
    }
  }

  if (code == 1 && nrow(gwt_data) != 1) {
    stop(
      "gwt_data must have exactly one row for code 1 (constant).",
      call. = FALSE
    )
  }

  sep <- .get_eol(eol)

  # Auto-generate description if not provided
  if (is.null(description)) {
    description <- switch(
      as.character(code),
      "0" = "no shallow groundwater table",
      "1" = paste0(
        "constant groundwater table at ",
        gwt_data$depth[1],
        " m and with salinity level of ",
        gwt_data$ecw[1],
        " dS/m"
      ),
      "2" = paste0("variable groundwater table, first year is ", start_year)
    )
  }

  # ---- Build header ----
  content <- .get_gwt_header(
    code = code,
    description = description,
    version = version,
    start_day = start_day,
    start_month = start_month,
    start_year = start_year,
    eol = eol
  )

  # ---- Append data rows ----
  if (code == 1) {
    content <- paste0(
      content,
      sprintf(
        "%7d%10.2f%13.1f",
        gwt_data$day[1],
        gwt_data$depth[1],
        gwt_data$ecw[1]
      ),
      sep
    )
  }

  if (code == 2) {
    data_rows <- paste(
      mapply(
        function(day, depth, ecw) {
          paste0(sprintf("%7d%10.2f%13.1f", day, depth, ecw), sep)
        },
        gwt_data$day,
        gwt_data$depth,
        gwt_data$ecw
      ),
      collapse = ""
    )
    content <- paste0(content, data_rows)
  }

  # ---- Write file ----
  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, site_name, ".GWT")

  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}
