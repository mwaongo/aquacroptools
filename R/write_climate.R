#' Write Complete AquaCrop Climate Dataset
#'
#' @description
#' Convenience wrapper function that writes all required AquaCrop climate files
#' (.PLU, .ETo, .Tnx, and .CLI) in a single call. This ensures all files are
#' created consistently with the same station name and settings.
#'
#' @param data Data frame containing complete weather data with columns:
#'   - year, month, day (date information)
#'   - rain or var_name_rain (rainfall, mm)
#'   - et0 or var_name_et0 (reference evapotranspiration, mm/day)
#'   - tmin or var_name_tmin (minimum temperature,  deg C)
#'   - tmax or var_name_tmax (maximum temperature, deg C)
#'   - station (optional, station name)
#' @param path Directory path where all climate files will be written.
#'   Default = "weather/" (relative to current working directory).
#'   See \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}},
#'   and \code{\link{write_cli}} for details.
#' @param site_name Station name or identifier. Default = NULL (extracted from data if available,
#'   otherwise uses "station").
#'   See \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}},
#'   and \code{\link{write_cli}} for details.
#' @param var_name_rain Name of rainfall column in \code{data}. Default = "rain".
#'   Passed to \code{\link{write_plu}} as \code{var_name}.
#' @param var_name_et0 Name of ETo column in \code{data}. Default = "et0".
#'   Passed to \code{\link{write_eto}} as \code{var_name}.
#' @param var_name_tmin Name of minimum temperature column in \code{data}. Default = "tmin".
#'   Passed to \code{\link{write_tnx}} as \code{var_name_min}.
#' @param var_name_tmax Name of maximum temperature column in \code{data}. Default = "tmax".
#'   Passed to \code{\link{write_tnx}} as \code{var_name_max}.
#' @param scenario CO2 scenario for .CLI file. Options: "hist" (historical/Mauna Loa),
#'   "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585".
#'   Default = "hist". See \code{\link{write_cli}} for details on scenarios.
#' @param eol End-of-line character style for the output file. Options: "windows", "linux", "macos". If `NULL` (default), eol is auto-detected.
#' @param quiet Logical. If `TRUE`, suppress the progress messages. Each call
#'   otherwise reports six lines, which is noisy when writing files for many
#'   stations at once. Default = `FALSE`.
#' @param syear Start year of the data period (extracted from \code{data} if not provided).
#'   Default = NULL.
#'   See \code{\link{write_plu}}, \code{\link{write_eto}}, and \code{\link{write_tnx}} for details.
#' @param eyear End year of the data period (extracted from \code{data} if not provided).
#'   Default = NULL.
#'   See \code{\link{write_plu}}, \code{\link{write_eto}}, and \code{\link{write_tnx}} for details.
#'
#' @details
#' This function creates a complete set of AquaCrop climate files in the correct order:
#' \enumerate{
#'   \item Creates output directory if it doesn't exist
#'   \item Writes .PLU file (rainfall data) using \code{\link{write_plu}}
#'   \item Writes .ETo file (reference evapotranspiration data) using \code{\link{write_eto}}
#'   \item Writes .Tnx file (temperature data) using \code{\link{write_tnx}}
#'   \item Writes .CLI file (climate file that references the above files) using \code{\link{write_cli}}
#'   \item Copies appropriate CO2 file based on \code{scenario}
#' }
#'
#' All files will use the same station name, ensuring consistency. The function passes
#' appropriate parameters to each underlying write function.
#'
#' ## Parameter Mapping:
#' Arguments are passed to the underlying functions as follows:
#' \itemize{
#'   \item \code{data} passed to \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}}
#'   \item \code{path} passed to all write functions
#'   \item \code{site_name} passed to all write functions
#'   \item \code{var_name_rain} passed to \code{\link{write_plu}} as \code{var_name}
#'   \item \code{var_name_et0} passed to \code{\link{write_eto}} as \code{var_name}
#'   \item \code{var_name_tmin} passed to \code{\link{write_tnx}} as \code{var_name_min}
#'   \item \code{var_name_tmax} passed to \code{\link{write_tnx}} as \code{var_name_max}
#'   \item \code{scenario} passed to \code{\link{write_cli}}
#'   \item \code{eol} passed to all write functions
#'   \item \code{syear}, \code{eyear} passed to \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}}
#' }
#'
#' @return
#' Invisibly returns a named list with paths to all created files:
#' \describe{
#'   \item{cli}{Path to .CLI file (from \code{\link{write_cli}})}
#'   \item{plu}{Path to .PLU file (from \code{\link{write_plu}})}
#'   \item{eto}{Path to .ETo file (from \code{\link{write_eto}})}
#'   \item{tnx}{Path to .Tnx file (from \code{\link{write_tnx}})}
#' }
#'
#' @examples
#' \dontrun{
#' # Load example weather data
#' data("weather")
#'
#' # Write all climate files with defaults
#' files <- write_climate(
#'   data = weather,
#'   path = "weather/",
#'   site_name = "Wakanda"
#' )
#'
#' # Access individual file paths
#' files$cli
#' files$plu
#'
#' # Write climate files with RCP 4.5 scenario
#' write_climate(
#'   data = weather,
#'   path = "weather/",
#'   site_name = "Wakanda_Station",
#'   scenario = "rcp45"
#' )
#'
#' # Write climate files with custom column names
#' write_climate(
#'   data = my_weather_data,
#'   path = "climate/",
#'   site_name = "MyStation",
#'   var_name_rain = "precipitation",
#'   var_name_et0 = "pet",
#'   var_name_tmin = "temp_min",
#'   var_name_tmax = "temp_max"
#' )
#'
#' # Using data with station column (backward compatibility)
#' write_climate(data = weather_with_station, path = "climate/")
#'
#' # Specify year range explicitly
#' write_climate(
#'   data = weather,
#'   path = "weather/",
#'   site_name = "Wakanda",
#'   syear = 2000,
#'   eyear = 2010
#' )
#' }
#'
#' @family AquaCrop file writers
#' Data:
#' \code{\link{weather}} for example weather data
#'
#' @export
write_climate <- function(
    data = NULL,
    path = "CLIMATE/",
    site_name = NULL,
    var_name_rain = "rain",
    var_name_et0 = "et0",
    var_name_tmin = "tmin",
    var_name_tmax = "tmax",
    scenario = "hist",
    eol = NULL,
    syear = NULL,
    eyear = NULL,
    quiet = FALSE) {
  # Validate data
  if (is.null(data) || !is.data.frame(data)) {
    stop(
      "data must be a data frame with weather information.\n",
      "Required columns: year, month, day, ",
      var_name_rain, ", ", var_name_et0, ", ",
      var_name_tmin, ", ", var_name_tmax
    )
  }

  # Progress reporting, silenced by `quiet`
  inform <- function(...) if (!isTRUE(quiet)) message(...)

  # Extract station from data if available and site_name not provided
  if (is.null(site_name) && "station" %in% names(data)) {
    site_name <- utils::head(data, 1) |>
      dplyr::pull("station")
    inform("Using station name from data: '", site_name, "'")
  }

  # Set default station if still NULL
  if (is.null(site_name)) {
    site_name <- "station"
    inform("Using default station name: '", site_name, "'")
  }

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  if (!dir.exists(path)) fs::dir_create(path, recurse = TRUE)

  inform("Writing climate files to: ", path)

  # Write rainfall file
  inform("  [1/4] Writing rainfall file (.PLU)...")
  plu_file <- write_plu(
    data = data,
    path = path,
    site_name = site_name,
    var_name = var_name_rain,
    syear = syear,
    eyear = eyear,
    eol = eol
  )

  # Write ETo file
  inform("  [2/4] Writing ETo file (.ETo)...")
  eto_file <- write_eto(
    data = data,
    path = path,
    site_name = site_name,
    var_name = var_name_et0,
    syear = syear,
    eyear = eyear,
    eol = eol
  )

  # Write temperature file
  inform("  [3/4] Writing temperature file (.Tnx)...")
  tnx_file <- write_tnx(
    data = data,
    path = path,
    site_name = site_name,
    var_name_min = var_name_tmin,
    var_name_max = var_name_tmax,
    syear = syear,
    eyear = eyear,
    eol = eol
  )

  # Write CLI file
  inform("  [4/4] Writing climate file (.CLI) with scenario: ", scenario, "...")
  cli_file <- write_cli(
    path = path,
    site_name = site_name,
    eol = eol,
    scenario = scenario,
    check_files = TRUE
  )

  inform("Successfully created all climate files for station '", site_name, "'")

  # Return paths to all created files
  result <- list(
    cli = cli_file,
    plu = plu_file,
    eto = eto_file,
    tnx = tnx_file
  )

  invisible(result)
}
