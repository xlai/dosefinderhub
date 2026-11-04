# The preview notice shows on every tab (RELEASE.md: "The preview notice is shown").
# https://github.com/xlai/dosefinderhub/issues/99

library(shinytest2)

test_that("the preview notice shows on every tab", {
  app <- AppDriver$new(test_path("..", ".."), name = "preview-notice",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  for (tab in c("Introduction", "Questionnaire", "Trial Design", "Simulation", "Conduct")) {
    app$set_inputs(nav = tab)
    app$wait_for_idle()
    visible <- app$get_js("(function() { const n = document.getElementById('preview-notice'); return !!n && n.offsetParent !== null && n.getBoundingClientRect().height > 0; })()")
    expect_true(visible, label = paste("notice visible on", tab))
    text <- app$get_text("#preview-notice")
    expect_match(text, "Preview release", label = paste("notice text on", tab))
    expect_match(text, "checked by a statistician", label = paste("notice text on", tab))
  }
})
