# Contributing to aquacropr

Thanks for taking the time to contribute! This document covers how to report
issues, propose changes, and get a pull request merged. By participating,
you agree to abide by the [Code of Conduct](CODE_OF_CONDUCT.md).

## Ways to contribute

- **Bug reports** — open an issue at
  <https://github.com/mwaongo/aquacropr/issues>.
- **Feature requests** — open an issue describing the use case, not just the
  desired API.
- **Documentation fixes** — typos, unclear roxygen docs, vignette
  improvements.
- **Code contributions** — bug fixes, new readers/writers, new features.

For anything beyond a small fix, please open an issue first to discuss the
approach before writing code — it saves rework on both sides.

## Reporting a bug

Good bug reports are fixed faster. Please include:

- A minimal reproducible example (a [reprex](https://reprex.tidyverse.org/)
  is ideal). Sample AquaCrop input files are more useful than a description
  of them.
- The output of `sessionInfo()` and `packageVersion("aquacropr")`.
- What you expected to happen vs. what actually happened.

## Development setup

Requires R >= 4.1.

```r
# install.packages(c("devtools", "roxygen2", "testthat"))
devtools::install_github("mwaongo/aquacropr")

# or, from a local clone:
devtools::load_all()
```

Useful commands while working:

```r
devtools::load_all()    # load the package for interactive testing
devtools::document()    # regenerate NAMESPACE and man/ from roxygen comments
devtools::test()        # run the test suite
devtools::check()       # full R CMD check
```

## Code style

- Follow the style already in the file you're editing (2-space indent,
  `snake_case` for exported functions, `.snake_case` prefix with `@noRd` for
  internal helpers).
- Document every exported function with roxygen2 (`@param`, `@return`,
  `@examples`); internal helpers get a short `@noRd` comment explaining things.
- Avoid introducing new dependencies unless necessary; prefer base R or a
  package already imported (see `DESCRIPTION`).
- The package requires **R >= 4.1** (see `Depends` in `DESCRIPTION`), so the
  native pipe `|>` is available and is what package code uses. Prefer it over
  magrittr's `%>%` in new code. `%>%` is still re-exported for users, so it
  remains valid in examples and vignettes, but don't reach for it internally.
  Note that `|>` has no `.` placeholder — if you need one, restructure the
  call rather than switching back to `%>%`.
- Lambda shorthand `\(x)` is likewise available, but the codebase currently
  spells these `function(x)`; match the file you're editing.
- Refer to data-frame columns inside dplyr verbs with the `.data` pronoun
  (`.data$year`) and inside tidyselect calls with strings (`select("year")`).
  The package carries no `utils::globalVariables()` list, and `R CMD check`
  will flag a bare column name as an undefined global.
- Source files have mixed line endings — some are LF, some CRLF. Preserve
  whatever the file you're editing already uses; a whole-file conversion
  buries a one-line change in a few hundred lines of diff.

## Before opening a pull request

1. Run `devtools::document()` if you touched any roxygen comment or function
   signature, and commit the resulting changes to `NAMESPACE` and `man/`.
   Stale generated docs are a common source of review friction here.
2. Run `devtools::check()` and make sure it reports **0 errors, 0 warnings,
   0 notes** before submitting. Vignettes require Pandoc locally; skipping
   vignette building (`devtools::check(vignettes = FALSE)`, or
   `R CMD check --no-build-vignettes`) produces two warnings about a missing
   `inst/doc` that are an artifact of skipping, not a defect — everything
   else should still be clean.
3. Add or update tests in `tests/testthat/` for the behavior you changed.
   For the file writers, the most useful test is usually a comparison against
   known-good output: several of them are byte-sensitive, and a round-trip
   through the matching `read_*()` catches formatting regressions that a
   "does it run" test will not.
4. Add an entry to the top of [`NEWS.md`](NEWS.md) describing the user-facing
   change. Small releases are a flat bullet list; larger ones group bullets
   under `##` headings (see 0.2.0). Say what changed and, for a fix, what the
   old behaviour was — a reader should be able to tell whether it affected
   them without opening the diff.
5. Target the `master` branch with your pull request.

## Pull request review

- Keep PRs focused — one fix or feature per PR is easier to review and
  revert if needed.
- Explain *why* the change is needed in the PR description, not just what
  changed (the diff already shows that).
- Be responsive to review comments; if you disagree with feedback, explain
  your reasoning rather than re-pushing silently.

## License

By contributing, you agree that your contributions will be licensed under
the same [MIT License](LICENSE.md) that covers the project.
