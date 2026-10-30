# Smoke test: start the app with the default design, open every tab and run
# one simulation for each design.

library(shinytest2)

count_errors <- function(app) {
  app$get_js("document.querySelectorAll('.shiny-output-error').length")
}

# Waits until a picker drawn by the server is ready to be set.
wait_for_picker <- function(app, id) {
  app$wait_for_js(sprintf("!!(document.getElementById('%s') || {}).selectize", id))
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

test_that("the skeleton is rebuilt when the target, the prior MTD guess or the halfwidth changes", {
  app <- AppDriver$new(test_path("..", ".."), name = "skeleton-rebuild",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # The priors as shown in the skeleton table. DT draws the table in the
  # browser shortly after the server sends it, so wait (up to 10 s) until
  # it shows the expected values, then return what it shows.
  skeleton_shown <- function() {
    as.numeric(unlist(app$get_js(
      "Array.from(document.querySelectorAll('#trial_design-skeleton_df tbody td:nth-child(2)')).map(td => td.innerText)"
    )))
  }
  skeleton <- function(want) {
    for (i in 1:50) {
      shown <- skeleton_shown()
      if (length(shown) == length(want) && isTRUE(all.equal(shown, want, tolerance = 1e-6))) break
      Sys.sleep(0.2)
    }
    shown
  }
  expected <- function(ttl, prior_mtd, halfwidth = 0.25 * ttl) {
    dfcrm::getprior(halfwidth = halfwidth, target = ttl, nu = prior_mtd,
                    nlevel = default_design$n_doses)
  }
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(display_crm = TRUE, display_input_all = TRUE)
  want <- expected(default_design$ttl, default_design$crm$prior_mtd)
  expect_equal(skeleton(want), want, tolerance = 1e-6)

  # A new target also resets the halfwidth to 0.25 x target.
  app$set_inputs(`trial_design-ttl_inputt` = 0.3)
  want <- expected(0.3, default_design$crm$prior_mtd)
  expect_equal(skeleton(want), want, tolerance = 1e-6)
  expect_equal(app$get_value(input = "trial_design-halfwidth_input"), 0.075, tolerance = 1e-8)

  app$set_inputs(`trial_design-prior_mtd_input` = 2)
  want <- expected(0.3, 2)
  expect_equal(skeleton(want), want, tolerance = 1e-6)

  app$set_inputs(`trial_design-halfwidth_input` = 0.05)
  want <- expected(0.3, 2, halfwidth = 0.05)
  expect_equal(skeleton(want), want, tolerance = 1e-6)
  expect_equal(app$get_value(output = "trial_design-halfwidth_warning"), "")

  # The halfwidth must be below the target (and below 1 - target).
  app$set_inputs(`trial_design-halfwidth_input` = 0.35)
  expect_match(app$get_value(output = "trial_design-halfwidth_warning"), "at most 0.3")
})

test_that("an edited skeleton is checked, and CRM runs after a valid edit", {
  app <- AppDriver$new(test_path("..", ".."), name = "skeleton-edit",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # Edit dose 2 of the skeleton as DT sends a cell edit (0-based column;
  # column 0 is the dose name, column 1 the prior). The default skeleton is
  # 0.057, 0.136, 0.250, 0.382, 0.512, so 0.5 at dose 2 is not increasing.
  edit_dose_2 <- function(value) {
    app$set_inputs(`trial_design-skeleton_df_cell_edit` = list(row = 2, col = 1, value = value),
                   allow_no_input_binding_ = TRUE)
  }
  # The skeleton and its warning are only drawn once the CRM parameters are
  # displayed, as when a user edits them.
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(display_crm = TRUE)
  edit_dose_2("0.5")
  expect_match(app$get_value(output = "trial_design-skeleton_warning"), "increase")
  edit_dose_2("0.15")
  expect_equal(app$get_value(output = "trial_design-skeleton_warning"), "")

  app$set_inputs(nav = "Simulation")
  app$set_inputs(`simulation-simulation_design_selection_input` = "CRM")
  wait_for_picker(app, "simulation-scen_output_input")
  app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
  wait_for_picker(app, "simulation-metric_selection_input")
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

test_that("BOIN p.saf and p.tox follow the target, reach the simulation, and are checked", {
  app <- AppDriver$new(test_path("..", ".."), name = "boin-inputs",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  boin_value <- function(id) app$get_value(input = paste0("trial_design-", id))
  run_boin <- function() {
    app$set_inputs(nav = "Simulation")
    app$set_inputs(`simulation-simulation_design_selection_input` = "BOIN")
    wait_for_picker(app, "simulation-scen_output_input")
    app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
    wait_for_picker(app, "simulation-metric_selection_input")
    app$set_inputs(`simulation-metric_selection_input` = "Accuracy")
    app$click("simulation-run_simulation")
    app$wait_for_idle(timeout = 10 * 60 * 1000)
  }

  app$set_inputs(nav = "Trial Design")
  app$set_inputs(display_boin = TRUE, display_input_all = TRUE)

  # A new target keeps the multiples (0.6 and 1.4), so p.saf, p.tox and the
  # boundaries move with it.
  app$set_inputs(`trial_design-ttl_inputt` = 0.4)
  app$wait_for_idle()
  expect_equal(boin_value("direct_phi_1"), 0.24, tolerance = 1e-6)
  expect_equal(boin_value("direct_phi_2"), 0.56, tolerance = 1e-6)
  expected <- boin_lambdas(0.4, 0.24, 0.56)
  expect_equal(boin_value("direct_lambda_e"), expected$lambda_e, tolerance = 1e-6)
  expect_equal(boin_value("direct_lambda_d"), expected$lambda_d, tolerance = 1e-6)

  # Entering a boundary sets the matching probability and multiple.
  lambda_e <- boin_lambdas(0.4, 0.3, 0.56)$lambda_e
  app$set_inputs(`trial_design-direct_lambda_e` = lambda_e)
  app$wait_for_idle()
  expect_equal(boin_value("direct_phi_1"), 0.3, tolerance = 1e-3)
  expect_equal(boin_value("ttl_multiple_phi_1"), 0.75, tolerance = 1e-3)
  expect_equal(app$get_value(output = "trial_design-boin_phi_warning"), "")

  # BOIN runs with a target of 0.4.
  run_boin()
  app$click(selector = "a[data-value='Simulation Output - Tables']")
  app$wait_for_idle()
  titles <- unlist(app$get_js(
    "Object.keys(document.getElementById('simulation-ind_tables').selectize.options)"
  ))
  expect_true(any(grepl("^BOIN", titles)), label = "BOIN results")
  expect_equal(count_errors(app), 0)

  # A p.tox too close to the target is flagged in Trial Design.
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(`trial_design-direct_phi_2` = 0.42)
  app$wait_for_idle()
  expect_match(app$get_value(output = "trial_design-boin_phi_warning"), "p.tox")
})
