#' Read an AquaCrop Season Output File
#'
#' Parses an AquaCrop \code{PRMSeason.OUT} file into one or two tidy data frames.
#' Seasonal totals (rows starting with \code{Tot}) are always returned.
#' Intermediate rows (\code{10Day} or \code{Month}) are optionally returned
#' as a second data frame.
#'
#' @param file Character. Path to the \code{PRMSeason.OUT} file.
#' @param intermediate Logical. If \code{TRUE}, returns a named list with two
#'   elements: \code{season} (seasonal totals) and \code{intermediate}
#'   (decadal or monthly rows). If \code{FALSE} (default), returns only the
#'   seasonal totals data frame.
#'
#' @return When \code{intermediate = FALSE}: a \code{data.frame} of seasonal
#'   totals. When \code{intermediate = TRUE}: a named list with:
#'   \describe{
#'     \item{season}{data.frame of seasonal totals (\code{Tot} rows).}
#'     \item{intermediate}{data.frame of decadal or monthly rows
#'       (\code{10Day} or \code{Month} rows), or \code{NULL} if none found.}
#'   }
#'
#' @details
#' Column names are derived from the header line. The \code{RunNr} column
#' is parsed as integer. The \code{prm_file} column contains the station/PRM
#' file name from the last field of each data row. Columns \code{E/Ex},
#' \code{Tr/Trx}, \code{Y(dry)}, \code{Y(fresh)} and \code{Brelative} are
#' renamed to valid R names:
#' \code{E_Ex}, \code{Tr_Trx}, \code{Y_dry}, \code{Y_fresh},
#' \code{Brelative}.
#'
#' @examples
#' \dontrun{
#' # Seasonal totals only
#' season <- read_season_out("LIST/C1PRMSeason.OUT")
#'
#' # With daily or decadal or monthly intermediates
#' out <- read_season_out("LIST/C1PRMSeason.OUT", intermediate = TRUE)
#' out$season
#' out$intermediate
#' }
#'
#' @family AquaCrop readers
#' @export
read_season_out <- function(file, intermediate = FALSE) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  if (!grepl("PRMSeason\\.OUT$", basename(file), ignore.case = TRUE)) {
    warning(
      "File does not appear to be a PRMSeason.OUT file: ",
      basename(file),
      call. = FALSE
    )
  }

  lines <- readLines(file, warn = FALSE)

  # ---- Locate header line (contains RunNr) ----------------------------------
  hdr_idx <- grep("RunNr", lines)[1L]
  if (is.na(hdr_idx)) {
    stop("Cannot find header line in: ", file, call. = FALSE)
  }

  # ---- Parse column names from header --------------------------------------
  raw_cols <- strsplit(trimws(lines[hdr_idx]), "\\s+")[[1L]]
  # Append prm_file for the last unnamed column
  col_names <- c(raw_cols, "prm_file")
  # Sanitise problematic names
  col_names <- gsub("E/Ex", "E_Ex", col_names, fixed = TRUE)
  col_names <- gsub("Tr/Trx", "Tr_Trx", col_names, fixed = TRUE)
  col_names <- gsub("Y(dry)", "Y_dry", col_names, fixed = TRUE)
  col_names <- gsub("Y(fresh)", "Y_fresh", col_names, fixed = TRUE)
  col_names <- gsub("Brelative", "Brelative", col_names, fixed = TRUE)

  # ---- Identify data lines --------------------------------------------------
  # Data lines start with Tot(N), 10Day(N), or Month(N)
  tot_idx <- grep("^\\s*Tot\\(", lines)
  day_idx <- grep("^\\s*Day", lines)
  tenday_idx <- grep("^\\s*10Day", lines)
  month_idx <- grep("^\\s*Month", lines)
  inter_idx <- sort(c(day_idx, tenday_idx, month_idx))

  # ---- Parser: fixed-width split then prm_file -----------------------------
  .parse_lines <- function(idx) {
    if (length(idx) == 0L) {
      return(NULL)
    }

    rows <- lapply(idx, function(i) {
      ln <- lines[i]
      # Split on whitespace; last token is prm_file (filename with no spaces)
      toks <- strsplit(trimws(ln), "\\s+")[[1L]]
      # Expect length(col_names) tokens; pad or trim if needed
      n_expected <- length(col_names)
      if (length(toks) < n_expected) {
        toks <- c(toks, rep(NA_character_, n_expected - length(toks)))
      } else if (length(toks) > n_expected) {
        # Extra tokens: collapse excess into prm_file
        extra <- paste(toks[n_expected:length(toks)], collapse = " ")
        toks <- c(toks[seq_len(n_expected - 1L)], extra)
      }
      toks
    })

    df <- as.data.frame(
      do.call(rbind, rows),
      stringsAsFactors = FALSE
    )
    names(df) <- col_names

    # ---- Type conversions ---------------------------------------------------
    # RunNr: strip parentheses, coerce to integer
    df$RunNr <- as.integer(gsub("[^0-9]", "", df$RunNr))

    # All numeric columns except RunNr and prm_file
    num_cols <- setdiff(col_names, c("RunNr", "prm_file"))
    df[num_cols] <- lapply(df[num_cols], function(x) {
      suppressWarnings(as.numeric(x))
    })

    tibble::as_tibble(df)
  }

  season_df <- .parse_lines(tot_idx)
  inter_df <- if (length(inter_idx) > 0L) .parse_lines(inter_idx) else NULL

  season_df <- tibble::as_tibble(season_df[, 2:length(season_df)])

  inter_df <- tibble::as_tibble(inter_df[, 2:length(inter_df)])

  if (!intermediate) {
    return(season_df)
  }

  if (is.null(inter_df)) {
    warning(
      "intermediate = TRUE but no, daily (Day) or decadal (10Day) or monthly (Month) rows ",
      "found in: ",
      file,
      " returning NULL for $intermediate.",
      call. = FALSE
    )
  }

  list(season = season_df, intermediate = inter_df)
}

# Daily output, more complicated

# Constants

.DAY_FILE_RE <- "day\\.out$" # fix 1
.DAY_NUM_RE <- "^-?[0-9]+\\.?[0-9]*([eE][+-]?[0-9]+)?$"
.DAY_RUN_RE <- "^\\s*Run:\\s*([0-9]+)\\s*$"
.DAY_INT_COLS <- c("Day", "Month", "Year", "DAP", "Stage")
.DAY_NA_VALS <- c(-9, -9.9, -99, -99.9, -999, -999.9)


# Merge isolated numeric suffix: "WC" "2" -> "WC2"
.merge_numeric_suffixes <- function(tokens) {
  out <- character()
  i <- 1L
  while (i <= length(tokens)) {
    fused <- i < length(tokens) && grepl("^[0-9]+$", tokens[i + 1L])
    out <- c(out, if (fused) paste0(tokens[i], tokens[i + 1L]) else tokens[i])
    i <- i + if (fused) 2L else 1L
  }
  out
}

#  Minimal tidying: WC(1.20) -> WC_120 | Tr/Trx -> Tr_Trx
.tidy_names <- function(x) {
  x %>%
    gsub(pattern = "[(/]", replacement = "_") %>%
    gsub(pattern = "[). ]", replacement = "") %>%
    gsub(pattern = "_{2,}", replacement = "_") %>%
    gsub(pattern = "_$", replacement = "")
}

# Deduplicate: Z -> Z_1, Z_2, Z_3
.dedup_names <- function(x) {
  counts <- table(x)
  for (nm in names(counts[counts > 1L])) {
    idx <- which(x == nm)
    x[idx] <- paste0(nm, "_", seq_along(idx))
  }
  x
}

# TRUE if line has exactly n_cols numeric tokens
.is_numeric_line <- function(line, n_cols) {
  tok <- strsplit(trimws(line), "\\s+")[[1L]]
  length(tok) == n_cols && all(grepl(.DAY_NUM_RE, tok))
}

# Header = line with most tokens before data
.find_day_header <- function(lines) {
  n_tok <- vapply(
    lines,
    function(l) length(strsplit(trimws(l), "\\s+")[[1L]]),
    integer(1L)
  )
  lines[[which.max(n_tok)]]
}

# First fully numeric line with more than 5 tokens
.find_data_start <- function(lines) {
  for (i in seq_along(lines)) {
    tok <- strsplit(trimws(lines[[i]]), "\\s+")[[1L]]
    if (length(tok) >= 5L && all(grepl(.DAY_NUM_RE, tok))) return(i)
  }
  NA_integer_
}

# Split lines into a tibble; all columns as double (caller handles NA/typing)
.parse_result_lines <- function(lines, idx, col_names) {
  if (length(idx) == 0L) {
    return(NULL)
  }

  n_cols <- length(col_names)
  tok_list <- strsplit(trimws(lines[idx]), "\\s+")

  mat <- do.call(
    rbind,
    lapply(tok_list, function(tok) {
      n <- length(tok)
      if (n < n_cols) {
        length(tok) <- n_cols
      } else if (n > n_cols) {
        tok <- c(
          tok[seq_len(n_cols - 1L)],
          paste(tok[n_cols:n], collapse = " ")
        )
      }
      tok
    })
  )

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- col_names
  df[] <- lapply(df, function(x) suppressWarnings(as.double(x))) # fix 2: no NA here
  tibble::as_tibble(df)
}


#' Read an AquaCrop day.out file
#'
#' Auto-detects columns and multi-run structure. Handles duplicated column
#' names (suffixed `_1`, `_2`, ...) and space-separated numeric suffixes
#' (e.g. `"WC 2"` becomes `WC2`).
#'
#' @param file    Path to the `day.out` file.
#' @param na      Numeric sentinel values coerced to `NA`.
#'   Default: `c(-9, -9.9, -99, -99.9, -999, -999.9)`.
#' @param verbose If `TRUE`, prints column count and duplicate names.
#'
#' @return A [tibble][tibble::tibble] with a trailing `prm_file` column
#'   (base name of the `.PRM` file inferred from `file`).
#'
#' @examples
#' \dontrun{
#' # Single-run file
#' df <- read_day_out("path/to/day.out")
#' df
#'
#' # Multi-run file with diagnostic info
#' df <- read_day_out("path/to/day.out", verbose = TRUE)
#'
#' # Filter one year after reading
#' library(dplyr)
#' df %>% filter(Year == 1976)
#' }
#'
#' @export
read_day_out <- function(
  file,
  na = .DAY_NA_VALS,
  verbose = FALSE
) {
  # 1. Validate
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  if (!grepl(.DAY_FILE_RE, basename(file), ignore.case = TRUE)) {
    warning(
      "File does not appear to be a day.out: ",
      basename(file),
      call. = FALSE
    )
  }

  # 2. Single read
  all_lines <- readLines(file, warn = FALSE)
  head_lines <- head(all_lines, 10)

  # 3. Detect header and build column names
  data_start <- .find_data_start(head_lines)
  if (is.na(data_start)) {
    stop("Cannot detect data start in: ", file, call. = FALSE)
  }

  header <- .find_day_header(head_lines[seq_len(data_start - 1L)])
  raw_tokens <- strsplit(trimws(header), "\\s+")[[1L]]
  tidy_names <- .merge_numeric_suffixes(raw_tokens) %>% .tidy_names()
  final_names <- .dedup_names(tidy_names)

  # 4. Guard: token count vs data column count
  n_cols <- length(strsplit(trimws(head_lines[[data_start]]), "\\s+")[[1L]])

  if (length(final_names) != n_cols) {
    warning(
      sprintf(
        "%d header tokens but %d data columns in '%s'. Falling back to V1...V%d.",
        length(final_names),
        n_cols,
        basename(file),
        n_cols
      ),
      call. = FALSE
    )
    tidy_names <- paste0("V", seq_len(n_cols))
    final_names <- tidy_names
  }

  if (verbose) {
    message(sprintf(
      "  %d columns | duplicates: [%s]",
      length(final_names),
      paste(tidy_names[duplicated(tidy_names)], collapse = ", ")
    ))
  }

  # 5. Locate data lines (vectorised)
  candidate_idx <- grep("^\\s*-?[0-9]", all_lines)
  data_idx <- candidate_idx[
    vapply(
      all_lines[candidate_idx],
      .is_numeric_line,
      logical(1L),
      n_cols = n_cols
    )
  ]

  if (length(data_idx) == 0L) {
    stop("No data lines found in: ", file, call. = FALSE)
  }

  # 6. Parse
  df <- .parse_result_lines(all_lines, data_idx, final_names)

  # 7. Re-type integer columns
  int_present <- intersect(.DAY_INT_COLS, final_names)
  df[int_present] <- lapply(df[int_present], as.integer)

  # 8. Sentinel -> NA (column-wise — %in% on a whole tibble is unreliable)
  df[] <- lapply(df, function(x) replace(x, x %in% na, NA))

  # 9. Append prm_file
  tibble::add_column(
    df,
    prm_file = sub(.DAY_FILE_RE, ".PRM", basename(file), ignore.case = TRUE)
  )
}
