# Report and table downloads (decision 0003).
# https://github.com/xlai/dosefinderhub/issues/94

library(shinytest2)

test_that("the Simulation report and tables download after a run", {
  app <- AppDriver$new(test_path("..", ".."), name = "simulation-report",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # One questionnaire answer, for the appendix.
  app$set_inputs(nav = "Questionnaire")
  app$wait_for_idle()
  app$click("questionnaire-next_button")
  app$wait_for_idle()
  app$set_inputs(`questionnaire-ttl` = 0.3)

  app$set_inputs(nav = "Simulation")
  app$wait_for_idle()
  disabled <- function(id) app$get_js(sprintf("document.getElementById('%s').classList.contains('disabled') || document.getElementById('%s').hasAttribute('disabled')", id, id))
  expect_true(disabled("simulation-download_report"))
  expect_true(disabled("simulation-download_tables"))

  # One input at a time: each picker is drawn after the one before it is set.
  app$set_inputs(`simulation-n_sims_input` = 20)
  app$set_inputs(`simulation-simulation_design_selection_input` = c("CRM", "3+3", "BOIN"))
  app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
  app$set_inputs(`simulation-metric_selection_input` = c(
    "% participants treated at dose", "% times dose was selected as MTD", "Accuracy"
  ))
  app$click("simulation-run_simulation")
  app$wait_for_idle(timeout = 10 * 60 * 1000)
  expect_false(disabled("simulation-download_report"))

  report <- paste(readLines(app$get_download("simulation-download_report"), warn = FALSE), collapse = "\n")
  expect_match(report, "<title>Simulation Report</title>")
  # Settings first, with the seed and the number of simulations.
  expect_lt(regexpr("<h2>Settings</h2>", report), regexpr("<h2>Results</h2>", report))
  expect_match(report, "<th>Seed</th>\\s*<td>12345</td>")
  expect_match(report, "<th>Simulations per design and scenario</th>\\s*<td>20</td>")
  expect_match(report, "<th>CRM skeleton</th>")
  expect_match(report, "<th>BOIN p.tox</th>")
  # Tables and plots for every design.
  for (design in c("3\\+3", "CRM", "BOIN")) {
    expect_match(report, paste0("<h3>", design, " Simulation for Scenario 1 - "), label = design)
  }
  expect_match(report, "data:image/png;base64,")
  # Appendices at the end.
  expect_gt(regexpr("Appendix: questionnaire answers", report), regexpr("<h2>Plots</h2>", report))
  expect_match(report, "What is the target toxicity level for this trial\\?</th>\\s*<td>0.3</td>")
  expect_match(report, "<th>escalation</th>")

  tables <- read.csv(app$get_download("simulation-download_tables"), check.names = FALSE)
  expect_equal(names(tables), c("Table", "Row", "Column", "Value"))
  expect_true("True DLT probabilities" %in% tables$Table)
  for (design in c("3\\+3", "CRM", "BOIN")) {
    expect_true(any(grepl(paste0("^", design, " Simulation for Scenario 1 - "), tables$Table)), label = design)
  }
})
