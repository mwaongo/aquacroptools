# aquacropr 0.3.0

## Breaking changes

- **`%>%` is no longer re-exported.** Package code uses the native pipe `|>`
  throughout, so `R/utils-pipe.R` and the `magrittr` dependency are gone.
  Code that relied on `library(aquacropr)` providing `%>%` must now load it
  from magrittr or dplyr directly, or switch to `|>`.
- **`Depends: R (>= 4.1.0)`**, raised from `R (>= 3.5)`. Package code now uses
  the native pipe `|>` throughout (51 call sites converted from `%>%`), which
  requires R 4.1. The shipped `regionalsim` vignette already used `|>`, so the
  previous `R (>= 3.5)` declaration was not accurate in any case.

## Climate writers

- `write_climate()` is substantially faster. Writing the 427-cell regional grid
  (156,282 rows) went from **25.2 s to 4.4 s** (~5.7x). Three changes account for
  it: each climate file is now written through a single connection instead of a
  `readr::write_file()` header pass followed by a `readr::write_lines()` append
  pass; `readr`'s per-call overhead is replaced with base connections; and the
  repeated OS/path lookups (`Sys.info()` via `get_os()`, `fs::dir_exists()`)
  are cached or moved to their base equivalents. Output is byte-identical.
- Fixed `write_climate()` erroring on a **tibble containing `NA`**. `write_fwf()`
  replaced missing values with `x[is.na(x)] <- replace_na`, which a tibble
  rejects as an incompatible assignment. This affected the bundled `weather`
  data and anything coming from `readr::read_csv()`.
- Fixed `eol` being ignored for data rows. The header honoured the requested
  `eol` but `write_fwf()` re-detected it from the host OS, so
  `eol = "windows"` on Linux or macOS produced a file with CRLF header lines
  and LF data lines.
- `write_fwf()` now warns when a formatted value is wider than its column.
  Previously such values silently ran into the neighbouring column, emitting
  e.g. `14.61234567890129.487654321098` with no separator between `tmin` and
  `tmax` -- a corrupt file that AquaCrop reads without complaint.
- `.get_eol()` accepts `eol` case-insensitively, as documented.
- `write_climate()` gains `quiet`. A single call reports six lines, which is
  ~2,500 lines of output over the 427-cell regional grid; pass `quiet = TRUE`
  to silence it. Default `FALSE`, so existing output is unchanged.
- `write_cli()` now defaults to `eol = NULL` (auto-detect), matching every other
  writer -- it was the only one hard-coded to `"windows"`. Behaviour change for
  direct `write_cli()` calls on Linux and macOS, which now get LF rather than
  CRLF; a CRLF `.CLI` on a unix host can leave a trailing `\r` on the climate
  filenames it lists. `write_climate()` is unaffected, as it already passed
  `eol` through explicitly.

## Consistency

- `eol` now defaults to `NULL` (auto-detect) in every writer. `write_cro()` and
  `write_prm()` were the last two hard-coded to `"windows"`; `write_cro()`'s
  documentation already claimed `NULL`, so its code and docs now agree.
  Files written with the default on Linux/macOS change from CRLF to LF;
  passing `eol = "windows"` explicitly is unchanged.
- `write_cli()` defaults to `path = "CLIMATE/"`, matching `write_plu()`,
  `write_eto()`, `write_tnx()` and `write_climate()`. It was the only climate
  writer defaulting to `"weather/"`.
- Package-level environments are now declared together in `globals.R`;
  `zzz.R` holds only the attach hook.
- `CONTRIBUTING.md` updated for the native pipe, the R 4.1 minimum, the
  `.data`/string column-reference rules, the repo's mixed line endings, and
  the new test suite.

## Internals

- Removed `utils::globalVariables()`. The 130-name list was roughly 85% stale
  (every `var_02`..`var_84` entry, plus `x`, `path`, `%>%`, `.`, `value`,
  `fmt`, `width`, `thickness_vec`), and it masked real lookup errors rather
  than fixing them. The 23 names actually needed are now explicit:
  data-masking references use the `rlang::.data` pronoun, tidyselect
  references use strings, and the four bundled datasets are fetched through a
  new internal `.pkg_data()` helper instead of `utils::data(envir =
  environment())`. `R CMD check` reports no visible-binding notes.

## Testing

- Added `tests/testthat/` with regression tests covering the above.

## Other

- Renamed `write_irrig_batch()` to `write_irr_batch()`, matching the `write_<type>_batch()` pattern used by every other batch writer (`write_sol_batch()`, `write_man_batch()`, `write_cal_batch()`, `write_prm_batch()`) and the single-site `write_irr()`. Breaking change: update any code calling `write_irrig_batch()`.
- `install_binaries()`: `compiler` defaults to `NULL` (was `"gfortran"`), matching `install_source()`.
- `install_binaries()`: safer archive extraction, download validation, and post-install checks.
- `install_binaries()`: stricter `path`/`force`/version argument validation.
- `run_aquacrop()`: clearer error messages for a missing executable.
- Fixed `|>` and `\(...)` lambda usage inconsistent with `Depends: R (>= 3.5)`; switched to `%>%`/`function()`.
- Removed unused `gh` dependency and dead code in `utils-readers.R`.
- Deduplicated OS/executable-name detection via `get_os()` and `.aquacrop_exe_name()`.
- Fixed non-ASCII characters in `run_aquacrop()` and excluded `.claude/` from the build (`R CMD check` now passes clean).

---

# aquacropr 0.2.0

## Breaking changes

- Package renamed from `aquacroptools` to `aquacropr`. Update all `library()` calls and imports accordingly.
- `read_cal()` was re-implemented as a standalone improved function (previously part of `readers.R`).
- Dependency on `snakecase` removed; `withr` added.

## New features

### Installation from source

- `install_source()` — compile and install AquaCrop from Fortran source code (cross-platform).
- `build_source()` — build the AquaCrop binary from source.
- `download_source()` — download the AquaCrop source code.

### Onset detection (fuzzy logic)

- `find_onset()` — detect the rainy season onset using a fuzzy logic algorithm.
  See `vignette("sowingdate")` for a full worked example.

### New writers

- `write_cal()` / `write_cal_batch()` — write AquaCrop calendar (`.CAL`) files, single and batch.
- `write_gwt()` / `write_gwt_batch()` — write groundwater table (`.GWT`) files.
- `write_irr()` / `write_irrig_batch()` — write irrigation schedule (`.IRR`) files.
- `write_obs()` / `write_obs_batch()` — write field observation (`.OBS`) files.
- `write_off()` / `write_off_batch()` — write off-season condition files.
- `write_ppn()` — write plot/project parameter files.
- `write_sim()` — write simulation settings files.
- `create_irr_events()` / `create_irr_schedule()` — helper functions to build irrigation event data frames.

### New readers

- `read_cal()` — read AquaCrop calendar files.
- `read_day_out()` — read AquaCrop daily output files as a tibble.
- `read_season_out()` now returns a `tibble` instead of a plain data frame.

### New validators

- `is_cli()`, `is_eto()`, `is_tnx()` — validate climate input file formats.

## Improvements

- `write_prm()` / `write_prm_batch()` — major overhaul: dynamic optional file passing
  (SW0, GWT, IRR), improved day-of-year handling, calendar file integration via
  `find_onset()` + `read_cal()`, and better warnings for missing optional files.
- `write_climate()` — default output path changed to `"CLIMATE/"`.
- `write_cal_batch()` — additional parameters added for finer control.
- `write_fwf()` — auto-detects EOF when `NULL` is passed.
- `install_binaries()` — fixed cross-platform behavior; version 7.3 (typo-tagged release) is now excluded.
- `init_aquacrop()` — improved startup messaging and initialization reliability.
- `read_fwf()` — output coerced to numeric for single-column results.
- Internal codebase refactored from monolithic `readers.R` into focused modules:
  `read_inputs.R`, `read_outputs.R`, `read_cal.R`, `utils-batch.R`, `utils-climate.R`,
  `utils-readers.R`, `utils-validation.R`, `utils-misc.R`.

## Bug fixes

- Fixed regex escaping in the internal clean-directory utility (#internal).
- Fixed crop duration handling and associated warnings in `write_cro()`.
- Fixed section header indentation in `write_prm()`.
- Fixed edge-case crash in `find_onset()`.
- Fixed `install_source()` for cross-platform compilation.

## Documentation

- Three new vignettes: `settingup`, `sowingdate`, `regionalsim`.
- pkgdown website updated and rebuilt.
- README substantially revised to reflect new package name and capabilities.
- Repository URLs updated to <https://github.com/mwaongo/aquacropr>.

---

# aquacropr 0.1.0

- Initial release as `aquacroptools`.
- Core writers: `write_cli()`, `write_eto()`, `write_plu()`, `write_tnx()`, `write_sol()`,
  `write_swo()`, `write_man()`, `write_man_batch()`, `write_sol_batch()`, `write_cro()`,
  `write_climate()`, `write_prm()`, `write_prm_batch()`.
- Core readers: `read_cli()`, `read_eto()`, `read_plu()`, `read_tnx()`.
- `install_binaries()` — download and install pre-built AquaCrop binaries.
- `init_aquacrop()` — initialize an AquaCrop project directory.
- `run_aquacrop()` — run an AquaCrop simulation.
- Helper utilities: `build_crop_parameters()`, `calculate_crop_stages()`,
  `calculate_plant_density()`, `day_number()`, `to_aquacrop_day()`, `weather()`,
  `round_to()`, `ece_to_salinity()`, `salinity_to_ece()`.
