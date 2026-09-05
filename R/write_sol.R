#' Write AquaCrop Soil Profile File (.SOL)
#'
#' @description
#' Writes an AquaCrop v7.0 (August 2022) soil profile file (`.SOL`) containing
#' hydraulic and physical properties for each soil horizon.
#' Optionally, when `write_sw0 = TRUE`, also writes the matching initial
#' soil-water content file (`.SW0`) by calling [write_swo()].
#'
#' @param path Directory where the `.SOL` file will be written.
#'   Default: `"SOIL/"`.
#' @param site_name Name of the soil profile (used in the filename).
#'   Default: `"default-site"`.
#' @param cn Curve number for runoff estimation (0–100). Default: `46`.
#' @param rew Readily evaporable water (mm) from the top soil layer. Default: `5`.
#' @param texture Character vector of soil texture names (USDA classes).
#'   Must match entries in the `SoilWater` dataset.
#' @param thickness Numeric vector of horizon thicknesses (m).
#'   Must have the same length as `texture`.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#'
#' @param write_sw0 Logical; if `TRUE`, also write the corresponding `.SW0`
#'   file by calling [write_swo()]. Default: `TRUE`.
#'
#' @inheritParams write_swo

#' @param swo_layers Optional `data.frame` to pass as `layers` to [write_swo()].
#'   If `NULL` (default), [write_swo()] will construct layers from `texture`
#'   and `thickness`.
#'
#' @details
#' ### File content
#' The `.SOL` file defines hydraulic and physical parameters for each soil
#' horizon, including:
#'
#' - **SAT**: Saturation (vol %)
#' - **FC**: Field capacity (vol %)
#' - **WP**: Wilting point (vol %)
#' - **Ksat**: Saturated hydraulic conductivity (mm day⁻¹)
#' - **Penetrability, Gravel, CRa, CRb**: Additional physical characteristics
#'
#' The number of horizons is inferred from the lengths of `texture` and `thickness`.
#'
#' ### Writing the `.SW0` file
#' When `write_sw0 = TRUE`, [write_swo()] is called using the same `path`,
#' `site_name` (as `soil_name`), `texture`, `thickness` (as `layer_thickness`),
#' and `eol`. The following arguments are passed through:
#' `initial_water`, `salinity`, `initial_cc`, `initial_biomass`,
#' `initial_root_depth`, `bund_water`, `bund_ec`, and `swo_layers` (as `layers`).
#'
#' @return Invisibly returns the path to the written `.SOL` file.
#'
#' @seealso
#' [SoilWater] for hydraulic property data,
#' [write_swo()] for initial water content files (.SW0),
#' [write_cro()] for crop files,
#' [write_man()] for management files.
#'
#' @examples
#' \dontrun{
#' # Basic soil profile (single horizon)
#' write_sol(
#'   path = "soil/",
#'   site_name = "field1",
#'   texture = "loam",
#'   thickness = 1.0
#' )
#'
#' # Two horizons with automatic SW0 generation (field capacity)
#' write_sol(
#'   path = "soil/",
#'   site_name = "layered",
#'   cn = 65,
#'   rew = 7,
#'   texture = c("sandy loam", "clay loam"),
#'   thickness = c(0.4, 0.6),
#'   initial_water = "FC",
#'   salinity = "none"
#' )
#'
#' # Write only the .SOL file (no SW0)
#' write_sol(
#'   site_name = "soil_only",
#'   texture = "loam",
#'   thickness = 1.0,
#'   write_sw0 = FALSE
#' )
#' }
#'
#' @export
write_sol <- function(
    site_name = "default-site",
    path = "SOIL/",
    cn = 46,
    rew = 5,
    texture = c("loam", "silt loam"),
    thickness = c(0.5, 1.0),
    eol = NULL,
    # optional .SW0 side-effect
    write_sw0 = TRUE,
    initial_water = "WP",
    salinity = "none",
    initial_cc = -9.00,
    initial_biomass = 0.000,
    initial_root_depth = -9.00,
    bund_water = 0.0,
    bund_ec = 0.00,
    swo_layers = NULL) {
  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  if (!fs::dir_exists(path)) fs::dir_create(path)

  # Get unique soils texture and thickness if continuous
  soil_info <- consolidate_soils(
    texture = texture,
    thickness = thickness
  )

  nsoils <- soil_info[["nsoils"]]
  texture <- soil_info[["texture"]]
  thickness <- soil_info[["thickness"]]

  # Check total depth (warn if > 2 m)
  total_depth <- sum(thickness)
  if (total_depth > 2) {
    warning(
      "Total soil depth (", sprintf("%.2f", total_depth), " m) exceeds 2 meters.",
      "\nThis is deeper than typical root zones for most annual crops."
    )
  }

 # Validate CN
if (anyNA(cn)) {
  na_idx <- which(is.na(cn))
  stop(sprintf(
    "cn contains NA value(s) at position(s): %s",
    paste(na_idx, collapse = ", ")
  ))
}
if (any(cn < 0 | cn > 100)) {
  invalid_idx <- which(cn < 0 | cn > 100)
  stop(sprintf(
    "cn (curve number) must be between 0 and 100.\nInvalid value(s) at position(s) %s: %s",
    paste(invalid_idx, collapse = ", "),
    paste(cn[invalid_idx], collapse = ", ")
  ))
}

# Validate REW
if (anyNA(rew)) {
  na_idx <- which(is.na(rew))
  stop(sprintf(
    "rew contains NA value(s) at position(s): %s",
    paste(na_idx, collapse = ", ")
  ))
}
if (any(rew < 0)) {
  invalid_idx <- which(rew < 0)
  stop(sprintf(
    "rew (readily evaporable water) must be non-negative.\nInvalid value(s) at position(s) %s: %s",
    paste(invalid_idx, collapse = ", "),
    paste(rew[invalid_idx], collapse = ", ")
  ))
}

  # Normalize texture names
  texture <- tolower(trimws(texture))

  # Load SoilWater data
  SoilWater <- .pkg_data("SoilWater")

  # Validate textures
  invalid_textures <- setdiff(texture, SoilWater$description)
  if (length(invalid_textures) > 0) {
    stop(
      "Invalid texture(s): ", paste(unique(invalid_textures), collapse = ", "),
      "\nValid textures are:\n  ",
      paste(SoilWater$description, collapse = ", ")
    )
  }

  # Build header
  header <- .get_soil_header(
    cn = cn,
    rew = rew,
    nsoils = nsoils,
    eol = eol
  )

  # Write header
  output_file <- paste0(path, site_name, ".SOL")
  readr::write_file(
    x = header,
    file = output_file
  )

  # Join texture with SoilWater and add thickness; format fields
  data <- dplyr::right_join(
    SoilWater,
    tibble::tibble(description = texture),
    by = "description",
    multiple = "all"
  ) |>
    dplyr::relocate("description", .after = dplyr::everything()) |>
    dplyr::mutate(
      thickness = thickness, .before = "sat",
      thickness = sprintf("%8.2f", .data$thickness),
      sat = sprintf("%7.1f", .data$sat),
      fc = sprintf("%5.1f", .data$fc),
      wp = sprintf("%5.1f", .data$wp),
      ksat = sprintf("%7.1f", .data$ksat),
      penetrability = sprintf("%10.0f", .data$penetrability),
      gravel = sprintf("%9.0f", .data$gravel),
      cra = sprintf("%13.6f", .data$cra),
      crb = sprintf("%13.6f", .data$crb),
      description = sprintf("%25s", .data$description)
    )

  # Get line ending
  sep <- .get_eol(eol = eol)

  # Append soil data
  readr::write_delim(
    x = data,
    file = output_file,
    col_names = FALSE,
    delim = "",
    eol = sep,
    append = TRUE,
    quote = "none"
  )

  # Optional: write .SW0 using write_swo()
  if (isTRUE(write_sw0)) {
    if (!exists("write_swo", mode = "function")) {
      stop("`write_swo()` is required to write the .SW0 file, but was not found.")
    }

    invisible(write_swo(
      path = path,
      soil_name = site_name,
      texture = texture,
      eol = eol,
      initial_cc = initial_cc,
      initial_biomass = initial_biomass,
      initial_root_depth = initial_root_depth,
      bund_water = bund_water,
      bund_ec = bund_ec,
      initial_water = initial_water,
      salinity = salinity,
      layer_thickness = thickness,
      layers = swo_layers
    ))
  }

  # Return the .SOL path (original behavior preserved)
  invisible(output_file)
}

#' Consolidate consecutive soil horizons with identical texture
#'
#' Merges consecutive horizons with the same texture into a single horizon
#' by summing their thicknesses.
#'
#' @param texture Character vector of soil texture names.
#' @param thickness Numeric vector of horizon thicknesses (m).
#'
#' @return List with consolidated texture, thickness, and n_horizons.
#'
#' @keywords internal
#' @noRd
consolidate_soils <- function(texture, thickness) {
  # Validate inputs
  if (length(texture) != length(thickness)) {
    stop(
      "Length of texture (", length(texture), ") must match ",
      "length of thickness (", length(thickness), ")",
      call. = FALSE
    )
  }

  if (length(texture) == 0) {
    stop("texture and thickness cannot be empty", call. = FALSE)
  }

  if (!is.numeric(thickness) || any(is.na(thickness))) {
    stop("thickness must be numeric without NA values", call. = FALSE)
  }

  if (any(thickness <= 0)) {
    stop("All thickness values must be positive (> 0)", call. = FALSE)
  }

  # Use run-length encoding to find consecutive identical textures
  rle_texture <- rle(texture)

  # Initialize vectors for consolidated horizons
  consolidated_texture <- rle_texture$values
  consolidated_thickness <- numeric(length(consolidated_texture))

  # Calculate consolidated thickness for each group
  end_idx <- cumsum(rle_texture$lengths)
  start_idx <- c(1, end_idx[-length(end_idx)] + 1)

  for (i in seq_along(consolidated_texture)) {
    consolidated_thickness[i] <- sum(thickness[start_idx[i]:end_idx[i]])
  }

  # Return consolidated soil profile
  list(
    texture = consolidated_texture,
    thickness = consolidated_thickness,
    nsoils = length(consolidated_texture)
  )
}
#'
#' @rdname write_sol
#' @export
#'
write_soil <- write_sol
