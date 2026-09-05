#' Write AquaCrop Soil Initial Water Content File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) and higher soil initial water content (.SW0) file that
#' specifies the initial water content for each soil layer at the start of the
#' simulation. This file defines the soil moisture status at planting or simulation start.
#'
#' @param path Directory path where the .SW0 file will be written. Default = "SOIL/"
#' @param soil_name Name identifier for the soil initial conditions file (used in filename).
#'   Default = "default-soil-init"
#' @param texture Character vector of soil texture names for each layer.
#'   Valid textures: "sand", "loamy sand", "sandy loam", "loam", "silt loam", "silt",
#'   "sandy clay loam", "clay loam", "silty clay loam", "sandy clay", "silty clay", "clay".
#'   Default = "loam"
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#'   Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#' @param initial_cc Numeric. Initial canopy cover that can be reached without water stress (%).
#'   Use -9.00 for automatic calculation by AquaCrop. Default = -9.00
#' @param initial_biomass Numeric. Biomass produced before simulation start (ton/ha). Default = 0.000
#' @param initial_root_depth Numeric. Initial effective rooting depth without water stress (m).
#'   Use -9.00 for automatic calculation. Default = -9.00
#' @param bund_water Numeric. Water layer stored between soil bunds if present (mm). Default = 0.0
#' @param bund_ec Numeric. Electrical conductivity of water between bunds (dS/m). Default = 0.00
#' @param initial_water Character string or numeric. Specifies initial water content for all layers.
#'   Options:
#'   \itemize{
#'     \item \strong{"WP"} or \strong{"wilting_point"}: Sets to wilting point based on soil texture
#'     \item \strong{"FC"} or \strong{"field_capacity"}: Sets to field capacity (1.5 × WP)
#'     \item \strong{"SAT"} or \strong{"saturation"}: Sets to saturation (2.5 × WP)
#'     \item \strong{Numeric value}: Sets all layers to specified value (0-100%)
#'   }
#'   Only used if layers parameter is not provided. Default = "WP"
#' @param salinity Character string or numeric specifying soil salinity level for all layers.
#'   Options: "none", "very slight", "slight", "moderate", "strong", "very strong",
#'   or numeric ECe value (0-100 dS/m). Only used if layers parameter is not provided.
#'   Default = "none"
#' @param layer_thickness Numeric vector specifying thickness of each soil layer in meters.
#'   Only used if layers parameter is not provided. Default = 1.0 (single 1m layer)
#' @param layers Data frame with soil layer information. If provided, overrides initial_water.
#'   Must contain columns:
#'   \itemize{
#'     \item \strong{thickness}: Layer thickness in meters (m)
#'     \item \strong{water_content}: Volumetric water content (vol%)
#'     \item \strong{ece}: Electrical conductivity (dS/m). Optional, defaults to 0
#'   }
#' @family AquaCrop file writers
#' @details
#' The .SW0 file defines the initial soil water conditions for each layer at the
#' start of the AquaCrop simulation. These conditions significantly affect early
#' crop growth and development.
#'
#' ## Initial Conditions:
#' - **Canopy cover**: Set to -9.00 to let AquaCrop calculate, or specify actual value (0-100%)
#' - **Biomass**: Usually 0 at planting, but can specify pre-existing biomass
#' - **Root depth**: Set to -9.00 for automatic, or specify initial depth in meters
#'
#' ## Soil Textures and Default Wilting Points:
#' The function uses the \code{\link{SWOData}} dataset which contains wilting point values
#' for 12 standard USDA soil textures. See \code{data(SWOData)} for details.
#'
#' ## Water Content Levels:
#' - **Wilting Point (WP)**: Minimum water for plant survival (texture-specific)
#' - **Field Capacity (FC)**: WP × 1.5 (approximate)
#' - **Saturation (SAT)**: WP × 2.5 (approximate)
#'
#' ## Salinity Levels:
#' Use \code{\link{salinity_to_ece}} to convert qualitative descriptions to ECe values:
#' - "none": 0 dS/m (non-saline)
#' - "slight": 3 dS/m (slightly saline)
#' - "moderate": 6 dS/m (moderately saline)
#' - "strong": 12 dS/m (strongly saline)
#' - "very strong": 20 dS/m (very strongly saline)
#'
#' @return
#' Invisibly returns the full path to the created .SW0 file.
#'
#' @examples
#' \dontrun{
#' # Simple: Single layer at wilting point, no salinity
#' write_swo(
#'   path = "soil/",
#'   soil_name = "loam-wp",
#'   texture = "loam",
#'   initial_water = "WP"
#' )
#'
#' # Single layer at field capacity
#' write_swo(
#'   soil_name = "sandy-loam-fc",
#'   texture = "sandy loam",
#'   initial_water = "FC"
#' )
#'
#' # With salinity
#' write_swo(
#'   soil_name = "saline-soil",
#'   texture = "clay loam",
#'   initial_water = "WP",
#'   salinity = "moderate"
#' )
#'
#' # Multiple layers with different textures
#' write_swo(
#'   soil_name = "layered-profile",
#'   texture = c("sandy loam", "loam", "clay loam"),
#'   layer_thickness = c(0.3, 0.4, 0.3),
#'   initial_water = "FC",
#'   salinity = "slight"
#' )
#'
#' # Mid-season with existing crop
#' write_swo(
#'   soil_name = "mid-season-maize",
#'   texture = "loam",
#'   initial_water = "FC",
#'   initial_cc = 60,
#'   initial_biomass = 3.5,
#'   initial_root_depth = 0.7
#' )
#'
#' # Custom numeric values
#' write_swo(
#'   soil_name = "custom",
#'   texture = "loam",
#'   layer_thickness = c(0.5, 0.5),
#'   initial_water = 28,
#'   salinity = 4.5
#' )
#'
#' # Manual layers (full control)
#' layers <- data.frame(
#'   thickness = c(0.3, 0.4, 0.3),
#'   water_content = c(20, 25, 30),
#'   ece = c(0, 3, 6)
#' )
#'
#' write_swo(
#'   soil_name = "manual-layers",
#'   texture = c("Sandy Loam", "Loam", "Clay Loam"),
#'   layers = layers
#' )
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{SWOData}} for soil texture wilting point data
#'   \item \code{\link{salinity_to_ece}} for salinity level conversions
#'   \item \code{\link{write_sol}} for writing soil profile characteristics
#' }
#'
#' @export

write_swo <- function(
    path = "SOIL/",
    soil_name = "default-soil-init",
    texture = "loam",
    eol = NULL,
    initial_cc = -9.00,
    initial_biomass = 0.000,
    initial_root_depth = -9.00,
    bund_water = 0.0,
    bund_ec = 0.00,
    initial_water = "WP",
    salinity = "none",
    layer_thickness = 1.0,
    layers = NULL) {
  # Load soil water content default data
  SWOData <- .pkg_data("SWOData")

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  if (!fs::dir_exists(path)) fs::dir_create(path)

  # If layers not provided, create from texture, thickness, and initial_water
  if (is.null(layers)) {
    # Validate layer_thickness
    if (!is.numeric(layer_thickness)) {
      stop("layer_thickness must be numeric (in meters)")
    }

    if (any(layer_thickness <= 0)) {
      stop("All layer_thickness values must be positive (> 0)")
    }

    # Check total depth
    total_depth <- sum(layer_thickness)
    if (total_depth > 2) {
      warning(
        "Total soil depth (", sprintf("%.2f", total_depth), " m) exceeds 2 meters.",
        "\nThis is deeper than typical root zones for most annual crops.",
        "\nConsider if this depth is appropriate for your simulation."
      )
    }

    # Derive number of layers from layer_thickness
    n_layers <- length(layer_thickness)

    # Ensure texture matches number of layers
    if (length(texture) == 1) {
      texture <- rep(texture, n_layers)
    } else if (length(texture) != n_layers) {
      stop(
        "Length of texture (", length(texture), ") must match ",
        "length of layer_thickness (", n_layers, ")",
        "\nProvide either a single texture for all layers or one texture per layer."
      )
    }



    # Create layers data frame
    layers <- data.frame(
      texture = tolower(trimws(texture)),
      thickness = layer_thickness,
      stringsAsFactors = FALSE
    )

    # Join with SWOData to get wilting points
    layers <- dplyr::left_join(
      layers,
      SWOData,
      by = "texture"
    )

    # Check for unmatched textures
    unmatched <- layers[is.na(layers$wp), "texture"]
    if (length(unmatched) > 0) {
      stop(
        "Texture(s) not recognized: ", paste(unique(unmatched), collapse = ", "),
        "\nValid textures are:\n  ",
        paste(SWOData$texture, collapse = ", ")
      )
    }


    # Calculate water content based on initial_water parameter
    if (is.character(initial_water)) {
      initial_water_lower <- tolower(trimws(initial_water))

      if (initial_water_lower %in% c("wp", "wilting_point")) {
        layers$water_content <- layers$wp
      } else if (initial_water_lower %in% c("fc", "field_capacity")) {
        layers$water_content <- layers$wp * 1.5
      } else if (initial_water_lower %in% c("sat", "saturation")) {
        layers$water_content <- layers$wp * 2.5
      } else {
        stop(
          "Invalid initial_water: '", initial_water, "'",
          "\nValid options: 'WP', 'FC', 'SAT', or numeric value (0-100)"
        )
      }
    } else if (is.numeric(initial_water)) {
      if (initial_water < 0 || initial_water > 100) {
        stop(
          "initial_water must be between 0 and 100 when numeric.",
          "\nReceived: ", initial_water
        )
      }
      layers$water_content <- initial_water
    } else {
      stop("initial_water must be character ('WP', 'FC', 'SAT') or numeric")
    }
    # Convert salinity to ECe (handle vectors)
    if (length(salinity) == 1) {
      ece_value <- salinity_to_ece(salinity)
      layers$ece <- ece_value
    } else if (length(salinity) == n_layers) {
      # Different salinity for each layer
      layers$ece <- sapply(salinity, salinity_to_ece)
      ece_value <- NULL # Set to NULL since it varies
    } else {
      stop(
        "Length of salinity (", length(salinity), ") must be either 1 or match ",
        "length of layer_thickness (", n_layers, ")"
      )
    }


    # Select and reorder columns
    layers <- layers |>
      dplyr::select("thickness", "water_content", "ece")

    message(
      "Created ", n_layers, " soil layer(s) with ",
      sprintf("%.1f", mean(layers$water_content)), "% avg water content ",
      "and ",
      if (length(unique(layers$ece)) == 1) {
        paste0(unique(layers$ece), " dS/m salinity")
      } else {
        paste0("variable salinity (", paste(layers$ece, collapse = ", "), " dS/m)")
      }
    )
  }

  # Validate layers data frame
  if (!is.data.frame(layers)) {
    stop(
      "layers must be a data frame with columns: thickness, water_content, ece",
      "\nExample: data.frame(thickness = 1.0, water_content = 30, ece = 0)"
    )
  }

  required_cols <- c("thickness", "water_content")
  missing_cols <- setdiff(required_cols, names(layers))
  if (length(missing_cols) > 0) {
    stop(
      "layers data frame is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "\nRequired columns: thickness, water_content",
      "\nOptional column: ece"
    )
  }

  # Add ece column if missing (default to 0)
  if (!"ece" %in% names(layers)) {
    layers$ece <- 0
  }

  # Validate number of layers
  n_layers <- nrow(layers)
  if (n_layers == 0) {
    stop("layers data frame must have at least one row")
  }

  if (n_layers > 13) {
    warning(
      "Number of layers (", n_layers, ") exceeds AquaCrop recommended maximum of 13.",
      "\nThis may cause issues in some AquaCrop versions."
    )
  }

  # Validate layer thickness values
  if (any(layers$thickness <= 0)) {
    stop("All layer thickness values must be positive (> 0)")
  }

  # Check total depth
  total_depth <- sum(layers$thickness)
  if (total_depth > 2) {
    warning(
      "Total soil depth (", sprintf("%.2f", total_depth), " m) exceeds 2 meters.",
      "\nThis is deeper than typical root zones for most annual crops.",
      "\nConsider if this depth is appropriate for your simulation."
    )
  }

  # Derive nsoils from layers (for consistency)
  nsoils <- n_layers

  # Validate layer values
  if (any(layers$water_content < 0 | layers$water_content > 100)) {
    stop(
      "water_content must be between 0 and 100 (vol%).",
      "\nReceived values outside this range: ",
      paste(layers$water_content[layers$water_content < 0 | layers$water_content > 100],
        collapse = ", "
      )
    )
  }

  if (any(layers$ece < 0)) {
    stop("ece (electrical conductivity) values must be non-negative (>= 0)")
  }

  # Validate initial conditions
  if (initial_cc != -9.00 && (initial_cc < 0 || initial_cc > 100)) {
    stop(
      "initial_cc must be -9.00 (automatic) or between 0 and 100%.",
      "\nReceived: ", initial_cc
    )
  }

  if (initial_biomass < 0) {
    stop("initial_biomass must be non-negative. Received: ", initial_biomass)
  }

  if (initial_root_depth != -9.00 && initial_root_depth < 0) {
    stop(
      "initial_root_depth must be -9.00 (automatic) or positive.",
      "\nReceived: ", initial_root_depth
    )
  }

  if (bund_water < 0) {
    stop("bund_water must be non-negative. Received: ", bund_water)
  }

  if (bund_ec < 0) {
    stop("bund_ec must be non-negative. Received: ", bund_ec)
  }

  # Get line ending
  sep <- .get_eol(eol = eol)

  # Derive nsoils from actual layers
  nsoils <- nrow(layers)

  # Generate header using helper function (pass all parameters including initial_water)
  header <- .get_soil_init_header(
    texture = texture,
    nsoils = nsoils,
    initial_water = initial_water,
    initial_cc = initial_cc,
    initial_biomass = initial_biomass,
    initial_root_depth = initial_root_depth,
    bund_water = bund_water,
    bund_ec = bund_ec,
    eol = eol
  )

  # Write header
  output_file <- paste0(path, soil_name, ".SW0")
  readr::write_file(
    x = header,
    file = output_file
  )

  # Format layer data for writing
  layer_output <- layers |>
    dplyr::mutate(
      thickness_fmt = sprintf("%13.2f", .data$thickness),
      water_content_fmt = sprintf("%21.2f", .data$water_content),
      ece_fmt = sprintf("%22.2f", .data$ece)
    ) |>
    dplyr::select("thickness_fmt", "water_content_fmt", "ece_fmt")

  # Write layer data
  readr::write_delim(
    x = layer_output,
    file = output_file,
    col_names = FALSE,
    delim = "",
    eol = sep,
    append = TRUE,
    quote = "none"
  )

  # Return invisibly with file path
  invisible(output_file)
}

#' @rdname write_swo
#' @export

write_sw0 <- write_swo
