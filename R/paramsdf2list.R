#' Convert data.frame to parameter list for batch functions
#'
#' @description
#' Converts a data.frame where each row represents one site/station into a list
#' of parameter lists suitable for batch writing functions like `write_sol_batch()`
#' or `write_man_batch()`.
#'
#' @param df Data.frame where each row represents one site/station
#' @param target Character string specifying the target function. One of:
#'   - `"write_sol_batch"`: For soil parameters (texture, thickness, cn, rew)
#'   - `"write_man_batch"`: For management parameters (var_02 to var_21)
#' @param id_col Name of the column containing site/station identifiers.
#'   Default: `"site"` for soil, `"station"` for management.
#'   Set to `NULL` to exclude from parameters.
#' @param validate Logical. If `TRUE` (default), validates parameter values.
#'
#' @return List of parameter lists suitable for the target batch function.
#'
#' @export
params_df2list <- function(
    df,
    target = c("write_sol_batch", "write_man_batch"),
    id_col = NULL,
    validate = TRUE) {
  # === Changement 1 : Renommer 'for' en 'target' ===
  # 'for' est un mot-clé R, mieux vaut l'éviter comme nom de paramètre
  target_function <- match.arg(target)

  # === Changement 2 : Vérifier que df est un data.frame ===
  if (!is.data.frame(df)) {
    stop("'df' must be a data.frame or tibble", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("'df' must have at least one row", call. = FALSE)
  }

  # Determine default id_col if not specified
  if (is.null(id_col)) {
    id_col <- switch(target_function,
      write_sol_batch = "site",
      write_man_batch = "station"
    )
  }

  # Check if id_col exists (if not NULL)
  if (!is.null(id_col) && !id_col %in% names(df)) {
    stop("Column '", id_col, "' not found in data.frame.\n",
      "Available columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  # Dispatch to appropriate converter
  params <- switch(target_function,
    write_sol_batch = .df2list_soil(df, validate = validate),
    write_man_batch = .df2list_man(df, validate = validate)
  )

  # Add names if id_col is provided
  if (!is.null(id_col) && id_col %in% names(df)) {
    names(params) <- df[[id_col]]
  }

  return(params)
}

#' Convert data.frame to soil parameter list
#'
#' @param df Data.frame with soil parameters
#' @param validate Logical. Validate parameters?
#'
#' @return List of soil parameter lists
#' @keywords internal
#' @noRd
.df2list_soil <- function(df, validate = TRUE) {
  # Check required columns
  required <- c("texture", "thickness", "cn", "rew")
  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop("Missing required columns for soil parameters: ",
      paste(missing, collapse = ", "), "\n",
      "Required: ", paste(required, collapse = ", "),
      call. = FALSE
    )
  }

  # === Changement 3 : Ajouter un compteur de lignes pour erreurs ===
  row_num <- 0

  # Convert each row to parameter list
  params <- purrr::pmap(df, function(texture, thickness, cn, rew, ...) {
    row_num <<- row_num + 1 # Incrémenter pour traçabilité

    # Handle texture (convert to character vector)
    if (is.list(texture)) {
      texture_vec <- as.character(unlist(texture))
    } else {
      texture_vec <- as.character(texture)
    }

    # Handle thickness (convert to numeric vector)
    if (is.list(thickness)) {
      thickness_vec <- as.numeric(unlist(thickness))
    } else {
      thickness_vec <- as.numeric(thickness)
    }

    # === Changement 4 : Retirer les NA automatiquement ===
    valid_idx <- !is.na(texture_vec) & !is.na(thickness_vec)
    texture_vec <- texture_vec[valid_idx]
    thickness_vec <- thickness_vec[valid_idx]

    # Validation
    if (validate) {
      # Check lengths match
      if (length(texture_vec) != length(thickness_vec)) {
        stop("Row ", row_num, " - Length mismatch: texture (", length(texture_vec),
          ") vs thickness (", length(thickness_vec), ")",
          call. = FALSE
        )
      }

      # Check at least one layer
      if (length(texture_vec) == 0) {
        stop("Row ", row_num, " - No valid soil layers found (all NA)",
          call. = FALSE
        )
      }

      # Check thickness > 0
      if (any(thickness_vec <= 0)) {
        stop("Row ", row_num, " - All thickness values must be positive (> 0)",
          call. = FALSE
        )
      }

      # Check cn range
      if (is.na(cn)) {
        stop("Row ", row_num, " - cn cannot be NA", call. = FALSE)
      }
      if (cn < 0 || cn > 100) {
        stop("Row ", row_num, " - cn must be between 0 and 100. Got: ", cn,
          call. = FALSE
        )
      }

      # Check rew >= 0
      if (is.na(rew)) {
        stop("Row ", row_num, " - rew cannot be NA", call. = FALSE)
      }
      if (rew < 0) {
        stop("Row ", row_num, " - rew must be non-negative. Got: ", rew,
          call. = FALSE
        )
      }
    }

    # Return parameter list
    list(
      texture = texture_vec,
      thickness = thickness_vec,
      cn = cn,
      rew = rew
    )
  })

  return(params)
}

#' Convert data.frame to management parameter list
#'
#' @param df Data.frame with management parameters
#' @param validate Logical. Validate parameters?
#'
#' @return List of management parameter lists
#' @keywords internal
#' @noRd
.df2list_man <- function(df, validate = TRUE) {
  # Valid management parameter columns (var_02 to var_21)
  valid_vars <- paste0("var_", sprintf("%02d", 2:21))

  # Find which parameters are present in df
  present_vars <- intersect(valid_vars, names(df))

  if (length(present_vars) == 0) {
    warning("No management parameters (var_02 to var_21) found in data.frame.\n",
      "Using default values for all parameters.",
      call. = FALSE
    )
  }

  # === Changement 5 : Ajouter compteur de lignes ===
  row_num <- 0

  # Convert each row to parameter list
  params <- purrr::pmap(df, function(...) {
    args <- list(...)
    row_num <<- row_num + 1

    # Extract only the var_* parameters
    param_list <- args[names(args) %in% valid_vars]

    # Validation (if requested and ManData is available)
    if (validate && length(param_list) > 0) {
      # === Changement 6 : Vérifier existence de ManData d'abord ===
      tryCatch(
        {
          ManData <- .pkg_data("ManData")

          for (var_name in names(param_list)) {
            if (!is.na(param_list[[var_name]])) {
              var_num <- as.numeric(sub("var_", "", var_name))
              var_info <- ManData[ManData$var_number == var_num, ]

              if (nrow(var_info) > 0) {
                value <- param_list[[var_name]]
                min_val <- var_info$min_value
                max_val <- var_info$max_value

                if (!is.na(min_val) && value < min_val) {
                  warning("Row ", row_num, " - ", var_name, " = ", value,
                    " is below minimum (", min_val, ")",
                    call. = FALSE
                  )
                }
                if (!is.na(max_val) && value > max_val) {
                  warning("Row ", row_num, " - ", var_name, " = ", value,
                    " is above maximum (", max_val, ")",
                    call. = FALSE
                  )
                }
              }
            }
          }
        },
        error = function(e) {
          # ManData non disponible, skip validation silencieusement
        }
      )
    }

    # Return empty list if no parameters, otherwise return param_list
    if (length(param_list) == 0) {
      return(list())
    } else {
      return(param_list)
    }
  })

  return(params)
}
