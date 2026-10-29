# Regenerate renv.lock for dev from an empty library.
#
# Run from the repository root, without the project's .Rprofile and without
# any site or user library:
#
#   R_LIBS_SITE=/nonexistent R_LIBS_USER=/nonexistent Rscript --vanilla tools/regenerate-lock.R
#
# It installs the latest versions of the packages the app and its tests use
# into a temporary library, then writes renv.lock with those packages and
# all their dependencies. Edit `packages` below when the app starts or stops
# using a package.

packages <- c(
  # App
  "shiny", "bslib", "bsicons", "htmltools", "markdown", "DT", "shinyjs",
  "ggplot2", "ggpattern", "magrittr", "rlang",
  "dplyr", "tidyr", "stringr", "tibble",
  "escalation", "BOIN", "dfcrm",
  # Conduct export, until the HTML and CSV exports replace it
  # (https://github.com/xlai/dosefinderhub/issues/94)
  "rmarkdown", "knitr", "openxlsx",
  # Tests
  "testthat", "shinytest2"
)

repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")

lib <- tempfile("renv-lock-library-")
dir.create(lib)
.libPaths(lib)

# Only the empty library and base R may be visible, so nothing already
# installed on this machine ends up in the lockfile.
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
renv::install(packages, library = lib, repos = repos, prompt = FALSE)

renv::snapshot(
  project = getwd(),
  library = c(lib, .Library),
  lockfile = "renv.lock",
  packages = c(packages, "renv"),
  repos = repos,
  prompt = FALSE
)

# renv installs Linux binaries from a platform-specific URL and records that
# URL for each package. Record the repository name instead, so the lockfile
# also restores on macOS and Windows.
lock <- renv::lockfile_read("renv.lock")
for (name in names(lock$Packages)) {
  if (grepl("^https://packagemanager\\.posit\\.co/cran/__linux__/",
            lock$Packages[[name]]$Repository %||% "")) {
    lock$Packages[[name]]$Repository <- "CRAN"
  }
}
renv::lockfile_write(lock, "renv.lock")
