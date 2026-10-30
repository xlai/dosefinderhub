# Smoke test: start the app with the default design, open every tab and run
# one simulation for each design.

library(shinytest2)

count_errors <- function(app) {
  app$get_js("document.querySelectorAll('.shiny-output-error').length")
}

test_that("every tab loads without errors", {
  app <- AppDriver$new(test_path("..", ".."), name = "tabs",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  for (tab in c("Introduction", "Questionnaire", "Trial Design", "Simulation", "Conduct")) {
    app$set_inputs(nav = tab)
    app$wait_for_idle()
    expect_equal(app$get_value(input = "nav"), tab)
    expect_equal(count_errors(app), 0, label = paste("errors on", tab))
  }
})

test_that("the default design runs one simulation for each design", {
  app <- AppDriver$new(test_path("..", ".."), name = "simulation",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # One input at a time: each picker is drawn after the one before it is set.
  app$set_inputs(nav = "Simulation")
  app$set_inputs(`simulation-simulation_design_selection_input` = c("CRM", "3+3", "BOIN"))
  app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
  app$set_inputs(`simulation-metric_selection_input` = c(
    "% participants treated at dose", "% times dose was selected as MTD",
    "Accuracy", "Duration", "% patients with a DLT"
  ))
  app$click("simulation-run_simulation")
  app$wait_for_idle(timeout = 10 * 60 * 1000)

  # Results are only drawn on a visible tab. The table picker then lists the
  # results of every design that ran.
  app$click(selector = "a[data-value='Simulation Output - Tables']")
  app$wait_for_idle()
  titles <- app$get_js(
    "Object.keys(document.getElementById('simulation-ind_tables').selectize.options)"
  )
  titles <- unlist(titles)
  expect_true(any(grepl("^3\\+3", titles)), label = "3+3 results")
  expect_true(any(grepl("^CRM", titles)), label = "CRM results")
  expect_true(any(grepl("^BOIN", titles)), label = "BOIN results")
  expect_equal(count_errors(app), 0)
})

test_that("CRM runs after a skeleton value is edited", {
  app <- AppDriver$new(test_path("..", ".."), name = "skeleton-edit",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # Edit dose 2 of the skeleton to a valid value, as DT sends a cell edit
  # (0-based column; column 0 is the dose name, column 1 the prior).
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(`trial_design-skeleton_df_cell_edit` = list(row = 2, col = 1, value = "0.15"),
                 allow_no_input_binding_ = TRUE)

  app$set_inputs(nav = "Simulation")
  app$set_inputs(`simulation-simulation_design_selection_input` = "CRM")
  app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
  app$set_inputs(`simulation-metric_selection_input` = "Accuracy")
  app$click("simulation-run_simulation")
  app$wait_for_idle(timeout = 10 * 60 * 1000)

  app$click(selector = "a[data-value='Simulation Output - Tables']")
  app$wait_for_idle()
  titles <- unlist(app$get_js(
    "Object.keys(document.getElementById('simulation-ind_tables').selectize.options)"
  ))
  expect_true(any(grepl("^CRM", titles)), label = "CRM results")
  expect_equal(count_errors(app), 0)
})
