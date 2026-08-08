# aquacropr 0.2.1

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
