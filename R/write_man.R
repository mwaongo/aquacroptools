#' Write AquaCrop Management File
#'
#' @description
#' Creates an AquaCrop v7.0+ management (.MAN) file that defines field and crop
#' management practices for simulation. Management files specify practices such as
#' mulch coverage, soil fertility stress, weed competition, and harvesting strategies.
#'
#' @param path Character. Directory path where the .MAN file will be created.
#'   The directory is created automatically if it doesn't exist. Default: `"MANAGEMENT/"`.
#' @param site_name Character. Name identifier for the management scenario.
#'   This name is used as the filename (e.g., `"high-fertility"` creates
#'   `"high-fertility.MAN"`). Default: `"generic-management"`.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#' @param params Named list of management parameters to override defaults.
#'   Parameter names must be `var_02` through `var_21`. Any parameters not
#'   specified will use default values. Set to `NULL` or `list()` to use all defaults.
#'   See \code{\link{ManData}} for complete parameter descriptions, valid ranges,
#'   and default values.
#'
#' @details
#' ## Management Parameters
#'
#' Management practices are controlled by 20 parameters (`var_02` to `var_21`) covering:
#' - Surface management (mulching, bunds, runoff)
#' - Soil fertility stress
#' - Weed competition
#' - Harvest management (single or multiple cuttings)
#'
#' For detailed descriptions of all parameters, their valid ranges, units, and default
#' values, see \code{\link{ManData}}.
#'
#' You only need to specify parameters that differ from defaults. Unspecified
#' parameters automatically use default values from \code{\link{ManData}}.
#'
#' ## File Format
#'
#' The function creates a colon-delimited text file compatible with AquaCrop v7.0+:
#' - Filename: `<site_name>.MAN`
#' - Format: `value : description`
#' - Each parameter on a separate line
#' - Header contains management scenario name and key parameters
#'
#' @return
#' Invisibly returns the full path to the created .MAN file as a character string.
#' The main effect is writing the file to disk.
#'
#' @examples
#' \dontrun{
#' # Example 1: Use all default values
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "default"
#' )
#'
#' # Example 2: High fertility with moderate mulching
#' # See ?ManData for parameter details
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "high-fertility",
#'   params = list(
#'     var_05 = 10, # Low fertility stress (high fertility)
#'     var_03 = 30, # 30% mulch cover
#'     var_04 = 50 # 50% evaporation reduction
#'   )
#' )
#'
#' # Example 3: Low fertility, no mulch
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "low-fertility",
#'   params = list(
#'     var_05 = 70, # High fertility stress (poor soil)
#'     var_03 = 0, # No mulch
#'     var_04 = 0 # No mulch effect
#'   )
#' )
#'
#' # Example 4: Weed competition scenario
#' # Check ?ManData for weed-related parameters
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "with-weeds",
#'   params = list(
#'     var_05 = 50, # Moderate fertility stress
#'     var_09 = 20, # Weed cover at closure
#'     var_10 = 10, # Weed increase in mid-season
#'     var_12 = 50 # Weed replacement
#'   )
#' )
#'
#' # Example 5: Multiple cutting management (e.g., forage)
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "multiple-cuts",
#'   params = list(
#'     var_13 = 1, # Enable multiple cuttings
#'     var_14 = 10, # Canopy after cut
#'     var_15 = 20, # CGC increase after cut
#'     var_16 = 45, # First cutting day
#'     var_17 = 30 # Cutting window
#'   )
#' )
#'
#' # Example 6: Complete management scenario
#' # Consult ?ManData to understand each parameter
#' irrigation_params <- list(
#'   var_03 = 40, # Mulch cover
#'   var_04 = 70, # Mulch effect
#'   var_05 = 20, # Fertility stress
#'   var_06 = 0.15, # Bund height
#'   var_07 = 10, # Runoff not affected
#'   var_08 = 80 # Runoff prevented
#' )
#'
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = "irrigated-field",
#'   params = irrigation_params,
#'   eol = "linux"
#' )
#'
#' # Example 7: Create station-specific management
#' station <- "grid_001"
#' write_man(
#'   path = "MANAGEMENT/",
#'   site_name = station,
#'   params = list(var_05 = 35)
#' )
#' }
#'
#'
#' @family AquaCrop file writers
#'
#' @export
write_man <- function(
    path = "MANAGEMENT/",
    site_name,
    eol = NULL,
    params = list()) {
  # Handle NULL params - only change needed
  if (is.null(params)) {
    params <- list()
  }

  # Ensure params is a list - simple check
  if (!is.list(params)) {
    stop(
      "params must be a list or NULL.",
      call. = FALSE
    )
  }

  # Add only the missing critical parameters, just to construct header
  if (!"var_03" %in% names(params)) {
    params$var_03 <- 0
  }

  if (!"var_05" %in% names(params)) {
    params$var_05 <- 42
  }

  # Extract values for header (always available now)
  fertilization_rate <- params$var_05
  mulching_rate <- params$var_03

  # Validate fertilization_rate and mulching_rate
  if (
    !is.numeric(fertilization_rate) ||
      fertilization_rate < 0 ||
      fertilization_rate > 100
  ) {
    stop(
      "fertilization_rate must be numeric between 0 and 100. Received: ",
      fertilization_rate,
      "\nNote: Represents soil fertility stress (%) - var_05",
      call. = FALSE
    )
  }

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  fs::dir_create(path, recurse = TRUE)

  # Load ManData
  ManData <- .pkg_data("ManData")
  valid_names <- ManData$name

  # Validate parameter names
  invalid_names <- setdiff(names(params), valid_names)
  if (length(invalid_names) > 0) {
    stop(
      "Invalid parameter names: ",
      paste(invalid_names, collapse = ", "),
      "\nValid names are: ",
      paste(valid_names, collapse = ", "),
      call. = FALSE
    )
  }

  # Parameter-specific validation ranges
  param_ranges <- list(
    var_03 = c(0, 100), # mulch cover %
    var_04 = c(0, 100), # mulch effect %
    var_05 = c(0, 100), # soil fertility %
    var_06 = c(0, Inf), # bund height m
    var_07 = c(0, 100), # runoff not affected %
    var_08 = c(0, 100), # runoff prevented %
    var_09 = c(0, 100), # weed cover %
    var_10 = c(0, Inf), # weed increase %
    var_12 = c(0, 100), # weed replacement %
    var_13 = c(0, 1), # multiple cuttings flag
    var_14 = c(0, 100), # canopy cover after cutting %
    var_15 = c(0, Inf), # CGC increase %
    var_16 = c(1, Inf), # first day of window
    var_20 = c(0, 1) # final harvest flag
  )

  # Validate parameter ranges
  for (param_name in names(params)) {
    if (param_name %in% names(param_ranges)) {
      range_limits <- param_ranges[[param_name]]
      value <- params[[param_name]]

      if (value < range_limits[1] || value > range_limits[2]) {
        stop(
          "Parameter '",
          param_name,
          "' value (",
          value,
          ") is outside valid range [",
          range_limits[1],
          ", ",
          range_limits[2],
          "]",
          call. = FALSE
        )
      }
    }
  }

  # Process parameters
  value <- params |>
    unlist() |>
    as.vector()

  description <- ManData |>
    dplyr::filter((.data$name %in% names(params))) |>
    dplyr::pull(description)

  fmt <- ManData |>
    dplyr::filter((.data$name %in% names(params))) |>
    dplyr::pull(fmt)

  width <- ManData |>
    dplyr::filter((.data$name %in% names(params))) |>
    dplyr::pull(width)

  new_vars <- tibble::tibble(
    name = names(params),
    value = value,
    description = description,
    fmt = fmt,
    width = width
  )

  # Combine with defaults and prepare output
  man_info <- ManData |>
    dplyr::filter(!(.data$name %in% names(params))) |>
    dplyr::bind_rows(new_vars) |>
    dplyr::arrange(.data$name) |>
    dplyr::mutate(
      value = .format_string2(
        string = value,
        fmt = fmt,
        width = width
      )
    ) |>
    dplyr::select(value, description)

  # Get line ending and header
  sep <- .get_eol(eol = eol)

  header <- .get_man_header(
    man = site_name,
    fertilizer = fertilization_rate,
    mulch = mulching_rate,
    eol = eol
  ) |>
    glue::glue("\n", .sep = sep)

  # Write file
  output_file <- paste0(path, site_name, ".MAN")

  readr::write_file(
    x = header,
    file = output_file
  )

  readr::write_delim(
    x = man_info,
    file = output_file,
    col_names = FALSE,
    delim = ":",
    eol = sep,
    append = TRUE,
    quote = "none"
  )

  # Return invisibly with file path
  invisible(output_file)
}
