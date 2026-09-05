#' Write AquaCrop Irrigation File
#'
#' @description
#' Creates an AquaCrop v7.0+ irrigation (.IRR) file that defines irrigation
#' management for simulation. The irrigation file specifies the irrigation method,
#' percentage of soil surface wetted, and irrigation scheduling approach.
#'
#' @param path Character. Directory path where the .IRR file will be created.
#'   The directory is created automatically if it doesn't exist. Default: `"MANAGEMENT/"`.
#' @param site_name Character. Name identifier for the irrigation scenario.
#'   This name is used as the filename (e.g., `"drip-irrigation"` creates
#'   `"drip-irrigation.IRR"`).
#' @param method Integer. Irrigation method:
#'   - 1 = Sprinkler irrigation
#'   - 2 = Surface irrigation: Basin
#'   - 3 = Surface irrigation: Border
#'   - 4 = Surface irrigation: Furrow
#'   - 5 = Drip irrigation
#' @param wet_surface Integer. Percentage of soil surface wetted by irrigation (0-100).
#'   See Details for typical values by irrigation method.
#' @param mode Integer. Irrigation mode:
#'   - 1 = Specification of irrigation events
#'   - 2 = Generation of an irrigation schedule
#'   - 3 = Determination of net irrigation water requirement
#' @param irr_data Data.frame or numeric. Irrigation data. Structure depends on `mode`:
#'   - Mode 1: data.frame with columns `day`, `depth`, `ecw`. See [create_irr_events()]
#'   - Mode 2: data.frame with columns `from_day`, `time_crit`, `depth_crit`, `ecw`
#'     and attributes `time_crit_code`, `depth_crit_code`. See [create_irr_schedule()]
#'   - Mode 3: Single numeric value representing allowable depletion of RAW (%, 0-100)
#' @param version Numeric. AquaCrop version. Default: 7.1
#' @param eol Character. End-of-line character style. Options: "windows", "linux", or "macos".
#'   If `NULL` (default), eol is auto-detected.
#' @param crop_length Integer. Optional. Length of crop cycle in days for validation.
#'   If provided, warns about irrigation events beyond crop harvest.
#'
#' @details
#' ## Typical Wetted Surface by Irrigation Method
#'
#' The `wet_surface` parameter represents the percentage of soil surface wetted by irrigation.
#' Typical values by irrigation method:
#'
#' | Irrigation Method | Typical wet_surface (%) |
#' |-------------------|-------------------------|
#' | Sprinkler | 100 |
#' | Basin | 100 |
#' | Border | 100 |
#' | Furrow (every furrow, narrow bed) | 60-100 |
#' | Furrow (every furrow, wide bed) | 40-60 |
#' | Furrow (alternated furrows) | 30-50 |
#' | Drip/Micro irrigation | 15-40 |
#' | Subsurface drip | 0 |
#'
#' ## Irrigation Modes
#'
#' **Mode 1 - Specified Events**: Define exact irrigation dates, depths, and water quality.
#' Use [create_irr_events()] to create properly formatted data.
#'
#' **Mode 2 - Generated Schedule**: Define rules that generate irrigation events automatically
#' based on time or depletion criteria. Use [create_irr_schedule()] to create properly
#' formatted data with required attributes.
#'
#' **Mode 3 - Net Requirement**: Calculate total irrigation water needed to maintain
#' soil water above a specified threshold (% RAW). Provide a single numeric value.
#'
#' @return
#' Invisibly returns the full path to the created .IRR file as a character string.
#' The main effect is writing the file to disk.
#'
#' @examples
#' \dontrun{
#' # Example 1: Fixed irrigation schedule (Mode 1)
#' events <- create_irr_events(
#'   day = c(10, 20, 30, 40, 50),
#'   depth = c(50, 50, 50, 50, 50),
#'   ecw = c(1.0, 1.0, 1.2, 1.4, 1.6)
#' )
#'
#' write_irr(
#'   path = "MANAGEMENT/",
#'   site_name = "ouaga-fixed",
#'   method = 5,        # Drip irrigation
#'   wet_surface = 30,  # 30% surface wetted
#'   mode = 1,          # Specified events
#'   irr_data = events,
#'   crop_length = 120  # Optional: validate against 120-day cycle
#' )
#'
#' # Example 2: Generated schedule with fixed interval (Mode 2)
#' schedule <- create_irr_schedule(
#'   from_day = c(1, 41, 116),
#'   time_crit = c(999, 7, 999),      # 7-day interval from day 41-115
#'   depth_crit = c(0, 40, 0),        # 40mm application depth
#'   ecw = c(0.4, 0.6, 0.8),
#'   time_crit_code = 1,              # Fixed interval
#'   depth_crit_code = 2              # Fixed depth
#' )
#'
#' write_irr(
#'   path = "MANAGEMENT/",
#'   site_name = "bobo-schedule",
#'   method = 1,         # Sprinkler
#'   wet_surface = 100,  # 100% surface wetted
#'   mode = 2,           # Generated schedule
#'   irr_data = schedule
#' )
#'
#' # Example 3: RAW-based automatic irrigation (Mode 2)
#' schedule_raw <- create_irr_schedule(
#'   from_day = c(1, 30, 60),
#'   time_crit = c(50, 40, 30),       # % RAW depletion threshold
#'   depth_crit = c(0, 0, 5),         # Back to FC (+ 5mm on last period)
#'   ecw = c(0.5, 0.6, 0.7),
#'   time_crit_code = 3,              # % RAW criterion
#'   depth_crit_code = 1              # Back to field capacity
#' )
#'
#' write_irr(
#'   path = "MANAGEMENT/",
#'   site_name = "auto-raw",
#'   method = 5,
#'   wet_surface = 30,
#'   mode = 2,
#'   irr_data = schedule_raw
#' )
#'
#' # Example 4: Net irrigation requirement (Mode 3)
#' write_irr(
#'   path = "MANAGEMENT/",
#'   site_name = "koudougou-netreq",
#'   method = 1,         # Sprinkler
#'   wet_surface = 100,
#'   mode = 3,           # Net requirement
#'   irr_data = 50       # Do not deplete below 50% RAW
#' )
#' }
#'
#' @family AquaCrop file writers
#' @export
write_irr <- function(
    path = "MANAGEMENT/",
    site_name,
    method,
    wet_surface,
    mode,
    irr_data,
    version = 7.1,
    eol = NULL,
    crop_length = NULL) {

  # Validation
  if (!method %in% 1:5) {
    stop("method must be 1-5", call. = FALSE)
  }

  if (wet_surface < 0 || wet_surface > 100) {
    stop("wet_surface must be 0-100", call. = FALSE)
  }

  if (!mode %in% 1:3) {
    stop("mode must be 1-3", call. = FALSE)
  }

  # Validate irrigation days against crop cycle length
  if (!is.null(crop_length) && mode == 1) {
    max_day <- max(irr_data$day)

    if (max_day > crop_length) {
      offseason_events <- sum(irr_data$day > crop_length)

      warning(
        sprintf(
          "%d irrigation event(s) beyond crop cycle (day %d > %d). ",
          offseason_events, max_day, crop_length
        ),
        "These will be ignored. Consider creating a separate .OFF file for off-season irrigation.",
        call. = FALSE
      )
    }
  }

  if (!is.null(crop_length) && mode == 2) {
    max_from_day <- max(irr_data$from_day)

    if (max_from_day > crop_length) {
      offseason_rules <- sum(irr_data$from_day > crop_length)

      warning(
        sprintf(
          "%d schedule rule(s) start beyond crop cycle (day %d > %d). ",
          offseason_rules, max_from_day, crop_length
        ),
        "These will be ignored. Consider creating a separate .OFF file for off-season irrigation.",
        call. = FALSE
      )
    }
  }

  # Setup
  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  sep <- .get_eol(eol = eol)

  # Build header
  method_names <- c("Sprinkler", "Basin", "Border", "Furrow", "Drip")
  mode_names <- c("Specified events", "Generated schedule", "Net requirement")

  content <- paste0(
    site_name, sep,
    .format_string2(version, '%.1f', 7), "  : AquaCrop Version (August 2023)", sep,
    .format_string2(method, '%d', 7), "  : ", method_names[method], " irrigation", sep,
    .format_string2(wet_surface, '%d', 7), "  : Percentage of soil surface wetted", sep,
    .format_string2(mode, '%d', 7), "  : ", mode_names[mode], sep
  )

  # Add data section based on mode
  if (mode == 1) {
    content <- paste0(content, .build_irr_events(irr_data, sep))
  } else if (mode == 2) {
    content <- paste0(content, .build_irr_schedule(irr_data, sep))
  } else if (mode == 3) {
    content <- paste0(content, .build_irr_netreq(irr_data, sep))
  }

  # Write
  output_file <- paste0(path, site_name, ".IRR")
  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}


#' Create Irrigation Events Data
#'
#' @description
#' Helper function to create properly formatted irrigation events data
#' for use with [write_irr()] in mode 1 (specification of irrigation events).
#'
#' @param day Integer vector. Day after sowing/planting for each irrigation event (1-366)
#' @param depth Numeric vector. Net irrigation application depth in mm for each event.
#'   This is the net amount only - do not include conveyance losses.
#' @param ecw Numeric vector. Electrical conductivity of irrigation water in dS/m for each event
#' @param crop_length Integer. Optional. Length of crop cycle in days for validation.
#'   If provided, warns about irrigation events beyond crop harvest.
#'
#' @return A tibble with columns `day`, `depth`, `ecw`, ready for use with [write_irr()]
#'
#' @examples
#' \dontrun{
#' # Fixed irrigation schedule for dry season tomato
#' events <- create_irr_events(
#'   day = c(10, 20, 30, 40, 50, 60, 70, 80),
#'   depth = c(40, 40, 50, 50, 50, 40, 40, 30),
#'   ecw = rep(0.5, 8),
#'   crop_length = 120  # Optional: validate against 120-day cycle
#' )
#'
#' # Variable water quality scenario
#' events_variable <- create_irr_events(
#'   day = c(15, 30, 45, 60),
#'   depth = c(50, 50, 50, 50),
#'   ecw = c(0.5, 0.8, 1.2, 1.5)  # Increasing salinity
#' )
#' }
#'
#' @export
create_irr_events <- function(day, depth, ecw, crop_length = NULL) {

  # Validation
  if (length(day) != length(depth) || length(day) != length(ecw)) {
    stop("day, depth, and ecw must have the same length", call. = FALSE)
  }

  if (any(day < 1 | day > 366)) {
    stop("day values must be between 1 and 366", call. = FALSE)
  }

  if (any(depth < 0)) {
    stop("depth values must be positive", call. = FALSE)
  }

  if (any(ecw < 0)) {
    stop("ecw values must be positive", call. = FALSE)
  }

  # Validate against crop cycle length
  if (!is.null(crop_length)) {
    max_day <- max(day)

    if (max_day > crop_length) {
      offseason_events <- sum(day > crop_length)

      warning(
        sprintf(
          "%d irrigation event(s) beyond crop cycle (day %d > %d). ",
          offseason_events, max_day, crop_length
        ),
        "These will be ignored. Consider creating a separate .OFF file for off-season irrigation.",
        call. = FALSE
      )
    }
  }

  tibble::tibble(
    day = as.integer(day),
    depth = as.numeric(depth),
    ecw = as.numeric(ecw)
  )
}


#' Create Irrigation Schedule Rules Data
#'
#' @description
#' Helper function to create properly formatted irrigation schedule rules
#' for use with [write_irr()] in mode 2 (generation of an irrigation schedule).
#'
#' @param from_day Integer vector. Day after sowing/planting from which each rule is valid.
#'   First value must be 1.
#' @param time_crit Numeric vector. Time criterion value. Meaning depends on `time_crit_code`:
#'   - Code 1: Fixed interval in days between irrigations
#'   - Code 2: Allowable depletion in mm before irrigation
#'   - Code 3: Allowable depletion as % of RAW before irrigation
#' @param depth_crit Numeric vector. Depth criterion value. Meaning depends on `depth_crit_code`:
#'   - Code 1: Extra water (mm) on top of amount to reach FC (can be 0, positive, or negative)
#'   - Code 2: Fixed net irrigation application depth in mm
#' @param ecw Numeric vector. Electrical conductivity of irrigation water in dS/m
#' @param time_crit_code Integer. Time criterion type (applies to all rules):
#'   - 1 = Fixed interval (days)
#'   - 2 = Allowable depletion (mm water)
#'   - 3 = Allowable depletion (% of RAW)
#' @param depth_crit_code Integer. Depth criterion type (applies to all rules):
#'   - 1 = Back to Field Capacity (with optional adjustment)
#'   - 2 = Fixed net application depth
#' @param crop_length Integer. Optional. Length of crop cycle in days for validation.
#'   If provided, warns about schedule rules starting beyond crop harvest.
#'
#' @return A tibble with columns `from_day`, `time_crit`, `depth_crit`, `ecw`
#'   and attributes `time_crit_code` and `depth_crit_code`, ready for use with [write_irr()]
#'
#' @details
#' Rules remain valid from their `from_day` until the next rule's `from_day` or until
#' the end of the cropping period.
#'
#' @examples
#' \dontrun{
#' # Fixed 7-day interval irrigation
#' schedule_fixed <- create_irr_schedule(
#'   from_day = c(1, 30, 90),
#'   time_crit = c(999, 7, 999),      # Irrigate every 7 days from day 30-89
#'   depth_crit = c(0, 40, 0),        # 40mm each time
#'   ecw = c(0.5, 0.5, 0.5),
#'   time_crit_code = 1,              # Fixed interval
#'   depth_crit_code = 2,             # Fixed depth
#'   crop_length = 120                # Optional validation
#' )
#'
#' # RAW-based irrigation (back to FC)
#' schedule_raw <- create_irr_schedule(
#'   from_day = c(1, 40),
#'   time_crit = c(50, 40),           # Trigger at 50% RAW, then 40% RAW
#'   depth_crit = c(0, 5),            # Back to FC, then FC + 5mm
#'   ecw = c(0.6, 0.8),
#'   time_crit_code = 3,              # % RAW threshold
#'   depth_crit_code = 1              # Back to FC
#' )
#'
#' # Depletion in mm criterion
#' schedule_mm <- create_irr_schedule(
#'   from_day = 1,
#'   time_crit = 30,                  # Irrigate when 30mm depleted
#'   depth_crit = -5,                 # Return to FC - 5mm (slight deficit)
#'   ecw = 0.5,
#'   time_crit_code = 2,              # Depletion in mm
#'   depth_crit_code = 1              # Back to FC (with adjustment)
#' )
#' }
#'
#' @export
create_irr_schedule <- function(from_day, time_crit, depth_crit, ecw,
                                time_crit_code, depth_crit_code,
                                crop_length = NULL) {

  # Validation
  if (length(from_day) != length(time_crit) ||
      length(from_day) != length(depth_crit) ||
      length(from_day) != length(ecw)) {
    stop("All vectors must have the same length", call. = FALSE)
  }

  if (!time_crit_code %in% 1:3) {
    stop("time_crit_code must be 1, 2, or 3", call. = FALSE)
  }

  if (!depth_crit_code %in% 1:2) {
    stop("depth_crit_code must be 1 or 2", call. = FALSE)
  }

  if (from_day[1] != 1) {
    stop("First from_day must be 1", call. = FALSE)
  }

  # Validate against crop cycle length
  if (!is.null(crop_length)) {
    max_from_day <- max(from_day)

    if (max_from_day > crop_length) {
      offseason_rules <- sum(from_day > crop_length)

      warning(
        sprintf(
          "%d schedule rule(s) start beyond crop cycle (day %d > %d). ",
          offseason_rules, max_from_day, crop_length
        ),
        "These will be ignored. Consider creating a separate .OFF file for off-season irrigation.",
        call. = FALSE
      )
    }
  }

  # Create tibble with attributes
  df <- tibble::tibble(
    from_day = as.integer(from_day),
    time_crit = as.numeric(time_crit),
    depth_crit = as.numeric(depth_crit),
    ecw = as.numeric(ecw)
  )

  attr(df, "time_crit_code") <- time_crit_code
  attr(df, "depth_crit_code") <- depth_crit_code

  df
}


# Internal helpers ----

#' Build irrigation events section
#' @noRd
.build_irr_events <- function(irr_data, sep) {

  stopifnot(
    "irr_data must be data.frame with columns: day, depth, ecw" =
      is.data.frame(irr_data) && all(c("day", "depth", "ecw") %in% names(irr_data))
  )

  # Sort data
  irr_data <- irr_data |> dplyr::arrange(.data$day)

  # Build section header
  section <- paste0(
    sep,
    "   Day    Depth (mm)   ECw (dS/m)", sep,
    "====================================", sep
  )

  # Add data rows
  for (i in seq_len(nrow(irr_data))) {
    section <- paste0(
      section,
      .format_string2(irr_data$day[i], "%d", 8),
      .format_string2(irr_data$depth[i], "%.0f", 12),
      .format_string2(irr_data$ecw[i], "%.1f", 12),
      sep
    )
  }

  section
}


#' Build irrigation schedule section
#' @noRd
.build_irr_schedule <- function(irr_data, sep) {

  stopifnot(
    "irr_data must have columns: from_day, time_crit, depth_crit, ecw" =
      is.data.frame(irr_data) &&
      all(c("from_day", "time_crit", "depth_crit", "ecw") %in% names(irr_data)),
    "Missing time_crit_code attribute" = !is.null(attr(irr_data, "time_crit_code")),
    "Missing depth_crit_code attribute" = !is.null(attr(irr_data, "depth_crit_code"))
  )

  time_crit_code <- attr(irr_data, "time_crit_code")
  depth_crit_code <- attr(irr_data, "depth_crit_code")

  time_labels <- c("Interval (days)", "Depletion (mm)", "Depletion (% RAW)")
  depth_labels <- c("Back to FC (mm)", "Application depth (mm)")

  # Build section header
  section <- paste0(
    .format_string2(time_crit_code, '%d', 7), "  : Time criterion", sep,
    .format_string2(depth_crit_code, '%d', 7), "  : Depth criterion", sep,
    sep,
    "  From day   ", time_labels[time_crit_code], "   ", depth_labels[depth_crit_code], "   ECw (dS/m)", sep,
    "  =======================================================================", sep
  )

  # Add data rows
  for (i in seq_len(nrow(irr_data))) {
    section <- paste0(
      section,
      .format_string2(irr_data$from_day[i], "%d", 10), " ",
      .format_string2(irr_data$time_crit[i], "%.0f", 17), " ",
      .format_string2(irr_data$depth_crit[i], "%.0f", 23), " ",
      .format_string2(irr_data$ecw[i], "%.1f", 12), sep
    )
  }

  section
}


#' Build net irrigation requirement section
#' @noRd
.build_irr_netreq <- function(irr_data, sep) {

  stopifnot(
    "irr_data must be single numeric (% RAW)" =
      is.numeric(irr_data) && length(irr_data) == 1,
    "RAW depletion must be 0-100" =
      irr_data >= 0 && irr_data <= 100
  )

  paste0(.format_string2(irr_data, '%.0f', 7), "  : Allowable depletion of RAW (%)", sep)
}
