# Load what the app loads before app.R runs: the files in R/, then
# pages/global.R, which holds the simulation functions.
shiny::loadSupport(test_path("..", ".."), renv = environment())
source(test_path("..", "..", "pages", "global.R"), local = TRUE)
