#' Write AquaCrop Crop File
#'
#' @description
#' Write an AquaCrop v7.2 (August 2024) crop (.CRO) file containing crop-specific parameters.
#' This file specifies physiological characteristics, growth parameters, water stress responses,
#' and other crop-specific properties that will be used during the AquaCrop simulation.
#'
#' @param path Directory path where the output .CRO file will be written. Default = "CROP/"
#' @param crop_name Name identifier for the crop (used in output filename). Default = "generic-crop-name"
#' @param eol End-of-line character style for the output file. Options: "windows", "linux", "macos". If `NULL` (default), eol is auto-detected.
#' @param params Named list of crop parameter values to override defaults from CropData.
#'   Parameter names should be var_02 through var_84 (e.g., `list(var_02 = 7.0, var_35 = 1.05)`).
#'   Unspecified parameters use default values from CropData.
#'
#' @family AquaCrop file writers
#' @return
#' Invisibly returns the full path to the created .CRO file. Creates a colon-delimited .CRO file
#' in the specified directory with the format: <<crop_name>>.CRO containing crop parameters
#' and descriptions as required by AquaCrop v7.2.
#'
#' @examples
#' \dontrun{
#' # Write default crop file
#' write_cro(path = "crops/", crop_name = "maize")
#'
#' # Write custom crop with modified parameters
#' write_cro(
#'   path = "crops/",
#'   crop_name = "maize-irrigated",
#'   params = list(
#'     var_35 = 1.10, # Higher crop coefficient
#'     var_50 = 0.85, # Max canopy cover
#'     var_64 = 35 # Reference harvest index
#'   )
#' )
#'
#' # Write crop with phenology parameters
#' write_cro(
#'   path = "crops/",
#'   crop_name = "wheat",
#'   params = list(
#'     var_52 = 10, # Days to emergence
#'     var_53 = 120, # Days to max rooting
#'     var_54 = 100, # Days to senescence
#'     var_55 = 130 # Days to maturity
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link{CropData}} for complete list of all 83 crop parameters with descriptions and valid ranges
#'
#' @export
write_cro <- function(
    path = "CROP/",
    crop_name = "generic-crop-name",
    eol = NULL,
    params = NULL) {
  # Handle NULL params
  if (is.null(params)) {
    params <- list(var_02 = 7.2)
  }

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  fs::dir_create(path, recurse = TRUE)

  # Load CropData

  CropData <- .pkg_data("CropData")
  valid_names <- CropData$name

  # Validate parameter names

  invalid_names <- setdiff(names(params), valid_names)

  if (length(invalid_names) > 0) {
    stop(
      "Invalid parameter names: ",
      paste(invalid_names, collapse = ", "),
      "\nValid names are: ",
      paste(valid_names, collapse = ", ")
    )
  }

  # Process parameters
  value <- params |>
    unlist() |>
    as.vector()

  description <- CropData |>
    dplyr::filter(.data$name %in% names(params)) |>
    dplyr::pull(description)

  fmt <- CropData |>
    dplyr::filter(.data$name %in% names(params)) |>
    dplyr::pull(fmt)

  width <- CropData |>
    dplyr::filter(.data$name %in% names(params)) |>
    dplyr::pull(width)

  new_vars <- tibble::tibble(
    name = names(params),
    value = value,
    description = description,
    fmt = fmt,
    width = width
  )
  # Combine with defaults and prepare output
  crop_info <- CropData |>
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

  header <- .get_crop_header(crop = crop_name) |>
    glue::glue("\n", .sep = sep)

  # Write file
  output_file <- paste0(path, crop_name, ".CRO")

  readr::write_file(
    x = header,
    file = output_file
  )

  readr::write_delim(
    x = crop_info,
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
