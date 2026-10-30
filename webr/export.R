# Build the Shinylive (browser) version of the app into `_site/`.
#
# webR cannot install trialr: escalation imports it, and it needs rstan.
# A stand-in trialr is installed here as a pure-R package and added to
# the export's package bundle, where Shinylive installs it like any other
# package. Its functions stop with an error (see webr/trialr); the app
# never calls them.
#
# Adapted from the Shinylive build of ICR-EPAT/dtptite-shiny
# (https://github.com/ICR-EPAT/dtptite-shiny/tree/spike/shinylive-pages).
#
# Run from the repository root: Rscript webr/export.R
stage <- file.path(tempdir(), "app")
lib <- file.path(tempdir(), "webr-lib")
unlink(c(stage, lib), recursive = TRUE)
dir.create(stage)
dir.create(lib)

status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", "-l", lib, "webr/trialr"))
if (status != 0) stop("R CMD INSTALL failed for webr/trialr", call. = FALSE)

# Only the app's own files go into the site, not its tests.
app_files <- setdiff(list.files("app", full.names = TRUE), "app/tests")
file.copy(app_files, stage, recursive = TRUE)
shinylive::export(stage, "_site")

# Add trialr to the bundle, in the form Shinylive uses for the packages it
# downloads: the installed package folder as a .tgz, and an entry in
# packages/metadata.rds pointing to it.
packages <- normalizePath("_site/shinylive/webr/packages")
metadata_file <- file.path(packages, "metadata.rds")
metadata <- readRDS(metadata_file)
for (pkg in "trialr") {
  version <- unname(read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")[1, 1])
  filename <- sprintf("%s_%s.tgz", pkg, version)
  dir.create(file.path(packages, pkg), showWarnings = FALSE)
  old <- setwd(lib)
  utils::tar(file.path(packages, pkg, filename), pkg, compression = "gzip",
             tar = "internal")
  setwd(old)
  path <- file.path("packages", pkg, filename)
  metadata[[pkg]] <- list(
    name = stats::setNames(pkg, pkg), version = version, cached = TRUE,
    assets = list(list(filename = filename, url = path, version = version)),
    type = "package", ref = paste0(pkg, "@", version), path = path
  )
}
saveRDS(metadata, metadata_file)
