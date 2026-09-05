# Package-level state. Both environments are internal and not exported.

# Currently unused; kept as scaffolding for a configurable version.
.aquacrop_state <- new.env(parent = emptyenv())
.aquacrop_state$version <- "7.1 (August 2023)"

# Values that are constant for the lifetime of the session but would otherwise
# be recomputed on every file written (currently the OS name; see get_os()).
.aquacropr_cache <- new.env(parent = emptyenv())


#' Retrieve a Package Dataset
#'
#' @description
#' Fetches one of the package's lazy-loaded datasets (`SoilWater`, `CropData`,
#' `ManData`, `SWOData`) by name.
#'
#' Package code used to reach these via `utils::data(name, envir =
#' environment())`, which binds the object at run time but leaves it invisible
#' to `R CMD check` -- every dataset then had to be declared in
#' `utils::globalVariables()`. Going through the namespace keeps the reference
#' explicit and checkable.
#'
#' @param name Character. Name of the dataset.
#' @return The dataset.
#' @keywords internal
#' @noRd
.pkg_data <- function(name) {
  get(name, envir = asNamespace("aquacropr"))
}
