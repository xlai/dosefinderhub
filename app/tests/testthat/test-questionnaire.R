# Questionnaire buttons: Previous, Save, Reset and Upload.
# https://github.com/xlai/dosefinderhub/issues/93

library(shinytest2)

question_on_screen <- function(app) app$get_text("#questionnaire-question_counter")

test_that("Previous keeps the earlier answer", {
  app <- AppDriver$new(test_path("..", ".."), name = "questionnaire-previous",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(nav = "Questionnaire")
  app$wait_for_idle()
  app$set_inputs(`questionnaire-n_doses` = 6)
  app$click("questionnaire-next_button")
  app$wait_for_idle()
  app$set_inputs(`questionnaire-ttl` = 0.3)
  app$click("questionnaire-prev_button")
  app$wait_for_idle()
  expect_equal(question_on_screen(app), "Question 1 of 9")
  expect_equal(app$get_value(input = "questionnaire-n_doses"), 6)
  app$click("questionnaire-next_button")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "questionnaire-ttl"), 0.3)
})

test_that("Save, Reset and Upload keep the session, and uploaded answers reach Trial Design", {
  app <- AppDriver$new(test_path("..", ".."), name = "questionnaire-buttons",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)
  used <- function(name) app$get_value(export = paste0("trial_design-", name))

  # Answer every question, with non-default answers where it matters.
  answers <- list(n_doses = 6, ttl = 0.3, max_n = 24, start_dose = 2, cohort_size = 2,
                  toxicity_confidence = "Somewhat confident",
                  time_resources = "Limited by time/resources",
                  statistical_support = "No statistician",
                  decision_transparency = "Simple fixed rules fine")
  app$set_inputs(nav = "Questionnaire")
  app$wait_for_idle()
  for (i in seq_along(answers)) {
    expect_equal(question_on_screen(app), sprintf("Question %d of 9", i))
    app$set_inputs(!!paste0("questionnaire-", names(answers)[i]) := answers[[i]])
    if (i < length(answers)) {
      app$click("questionnaire-next_button")
      app$wait_for_idle()
    }
  }

  # Save writes every answer.
  saved <- app$get_download("questionnaire-save_button")
  saved_answers <- read.csv(saved)
  expect_equal(saved_answers$inputId, names(answers))
  expect_equal(saved_answers$value, unname(vapply(answers, as.character, "")))

  # Reset goes back to question 1, drawn without the answer, and the session stays open.
  app$click("questionnaire-reset_button")
  app$wait_for_idle()
  expect_equal(question_on_screen(app), "Question 1 of 9")
  expect_equal(app$get_value(input = "questionnaire-n_doses"), 2)
  expect_true(app$get_js("Shiny.shinyapp.isConnected()"))

  # Upload restores every answer: the question on screen at once, the rest
  # when the user gets to them, and Save writes them all again.
  app$upload_file(`questionnaire-file_upload` = saved)
  app$wait_for_idle()
  expect_equal(app$get_value(input = "questionnaire-n_doses"), 6)
  saved_again <- read.csv(app$get_download("questionnaire-save_button"))
  expect_equal(saved_again$value, saved_answers$value)

  for (i in 2:length(answers)) {
    app$click("questionnaire-next_button")
    app$wait_for_idle()
    expect_equal(app$get_value(input = paste0("questionnaire-", names(answers)[i])),
                 answers[[i]], label = names(answers)[i])
  }
  app$click("questionnaire-next_button")
  app$wait_for_idle()
  app$click("questionnaire-advanced_settings")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "nav"), "Trial Design")
  expect_equal(used("n_doses"), 6)
  expect_equal(used("ttl"), 0.3)
  expect_equal(used("max_size"), 24)
  expect_equal(used("start_dose"), 2)
  expect_equal(used("cohort_size"), 2)
})
