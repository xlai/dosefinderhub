# Regenerate the renv lockfiles for dev from an empty library.
#
# Run from the repository root, without the project's .Rprofile and without
# any site or user library:
#
#   R_LIBS_SITE=/nonexistent R_LIBS_USER=/nonexistent Rscript --vanilla tools/regenerate-lock.R
#
# It installs the latest versions of the packages below into a temporary
# library, then writes two lockfiles with those packages and all their
# dependencies:
#
# - renv.lock: what the app needs to run.
# - renv/profiles/test/renv.lock: the app's packages plus the test packages.
#   Use it with RENV_PROFILE=test (as CI does).
#
# Edit the lists below when the app or the tests start or stop using a package.

app_packages <- c(
  "shiny", "bslib", "bsicons", "htmltools", "markdown", "DT", "shinyjs",
  "ggplot2", "ggpattern", "magrittr", "rlang",
  "dplyr", "tidyr", "stringr", "tibble",
  "escalation", "BOIN", "dfcrm",
  # Conduct export, until the HTML and CSV exports replace it
  # (https://github.com/xlai/dosefinderhub/issues/94)
  "rmarkdown", "knitr", "openxlsx"
)

test_packages <- c("testthat", "shinytest2")

lockfiles <- list(
  "renv.lock" = app_packages,
  "renv/profiles/test/renv.lock" = c(app_packages, test_packages)
)

repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")

lib <- tempfile("renv-lock-library-")
dir.create(lib)
.libPaths(lib)

# Only the empty library and base R may be visible, so nothing already
# installed on this machine ends up in the lockfiles.
others <- setdiff(normalizePath(.libPaths()), normalizePath(c(lib, .Library)))
if (length(others) > 0) {
  stop("Other libraries are visible: ", paste(others, collapse = ", "),
       "\nRun with R_LIBS_SITE=/nonexistent R_LIBS_USER=/nonexistent.")
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv must be installed in the R installation's own library.")
}

# Without the renv cache, so no package built earlier on this machine is reused.
options(repos = repos, renv.config.cache.enabled = FALSE)
renv::install(c(app_packages, test_packages), library = lib, repos = repos, prompt = FALSE)

for (lockfile in names(lockfiles)) {
  dir.create(dirname(lockfile), recursive = TRUE, showWarnings = FALSE)
  renv::snapshot(
    project = getwd(),
    library = c(lib, .Library),
    lockfile = lockfile,
    packages = c(lockfiles[[lockfile]], "renv"),
    repos = repos,
    prompt = FALSE
  )

  # renv installs Linux binaries from a platform-specific URL and records that
  # URL for each package. Record the repository name instead, so the lockfile
  # also restores on macOS and Windows.
  lock <- renv::lockfile_read(lockfile)
  for (name in names(lock$Packages)) {
    if (grepl("^https://packagemanager\\.posit\\.co/cran/__linux__/",
              lock$Packages[[name]]$Repository %||% "")) {
      lock$Packages[[name]]$Repository <- "CRAN"
    }
  }
  invisible(renv::lockfile_write(lock, lockfile))
}
