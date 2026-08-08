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
- Don't use syntax newer than R 3.5 (the package's declared minimum) —
  notably, no native pipe `|>` or lambda shorthand `\(x)`. Use `%>%`
  (already imported) and `function(x)` instead.

## Before opening a pull request

1. Run `devtools::document()` if you touched any roxygen comment or function
   signature, and commit the resulting changes to `NAMESPACE` and `man/`.
   Stale generated docs are a common source of review friction here.
2. Run `devtools::check()` and make sure it reports **0 errors, 0 warnings,
   0 notes** before submitting. Vignettes require Pandoc locally
   (`devtools::check(vignettes = FALSE)` if you don't have it installed).
3. Add or update tests for the behavior you changed, if applicable.
4. Add a one-line entry to the top of [`NEWS.md`](NEWS.md) describing the
   user-facing change. Keep it to a single bullet per change — this project
   favors brief, flat changelog entries over elaborate multi-section write-ups.
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
