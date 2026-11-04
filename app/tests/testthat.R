# Run the app's tests with shiny::runTests("app") or shinytest2::test_app("app").
# shinytest2 skips its browser tests "on CRAN" unless NOT_CRAN is set, and a
# silent skip would hide a broken app.
Sys.setenv(NOT_CRAN = "true")
shinytest2::test_app()
