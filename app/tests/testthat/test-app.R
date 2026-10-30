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

# The first row of an individual results table on the Tables tab, named by
# column. The table is picked by design, scenario and metric.
first_row <- function(app, design, scenario, metric) {
  titles <- unlist(app$get_js(
    "Object.keys(document.getElementById('simulation-ind_tables').selectize.options)"
  ))
  title <- grep(paste0("^", design, ".*", scenario, ".*", metric), titles, value = TRUE)
  expect_length(title, 1)
  app$set_inputs(`simulation-ind_tables` = title)
  app$wait_for_js(sprintf(
    "document.getElementById('simulation-selected_ind_title').textContent === %s",
    jsonlite::toJSON(title, auto_unbox = TRUE)
  ))
  cells <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('#simulation-selected_ind_table tr'))
       .slice(0, 2).map(tr => Array.from(tr.children).map(c => c.textContent.trim()).join('|'))"
  ))
  header <- strsplit(cells[1], "|", fixed = TRUE)[[1]][-1]
  values <- as.numeric(strsplit(cells[2], "|", fixed = TRUE)[[1]][-1])
  setNames(values, header)
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

test_that("the View Conduct Tab button opens the Conduct tab", {
  app <- AppDriver$new(test_path("..", ".."), name = "view-conduct",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(nav = "Simulation")
  app$click("simulation-view_conduct")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "nav"), "Conduct")
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

test_that("questionnaire answers reach Trial Design, and later edits there are kept", {
  app <- AppDriver$new(test_path("..", ".."), name = "questionnaire-transfer",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # The values Simulation and Conduct use (exported by Trial Design).
  used <- function(name) app$get_value(export = paste0("trial_design-", name))
  expect_equal(used("cohort_size"), default_design$cohort_size)
  expect_equal(used("ttl"), default_design$ttl)

  # Answer the general questions with non-default values, keep the rest.
  app$set_inputs(nav = "Questionnaire")
  for (i in 1:20) {
    if (isTRUE(app$get_js("!!document.getElementById('questionnaire-advanced_settings')"))) break
    question <- app$get_text("#questionnaire-question_counter")
    if (identical(question, "Question 2 of 9")) app$set_inputs(`questionnaire-ttl` = 0.3)
    if (identical(question, "Question 3 of 9")) app$set_inputs(`questionnaire-max_n` = 24)
    if (identical(question, "Question 5 of 9")) app$set_inputs(`questionnaire-cohort_size` = 2)
    app$click("questionnaire-next_button")
    app$wait_for_idle()
  }
  app$click("questionnaire-advanced_settings")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "nav"), "Trial Design")
  expect_equal(app$get_value(input = "trial_design-cohort_inputt"), 2)
  expect_equal(used("cohort_size"), 2)
  expect_equal(used("ttl"), 0.3)
  expect_equal(used("max_size"), 24)

  app$set_inputs(`trial_design-cohort_inputt` = 4)
  app$wait_for_idle()
  expect_equal(used("cohort_size"), 4)
})

test_that("Trial Design settings can be saved to a file and loaded again", {
  app <- AppDriver$new(test_path("..", ".."), name = "settings-file",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  td <- function(id) paste0("trial_design-", id)
  set_td <- function(values) {
    names(values) <- td(names(values))
    do.call(app$set_inputs, values)
    app$wait_for_idle()
  }
  app$set_inputs(nav = "Trial Design")

  # Non-default values, basic mode included; the target is set first, because a new target
  # resets the halfwidth.
  saved <- list(ttl_inputt = 0.3, cohort_inputt = 4, max_size_inputt = 24,
                halfwidth_input = 0.05, prior_var_input = 2,
                skip_esc_crm_input = "TRUE", ttl_multiple_phi_1 = 0.7,
                boin_stopping_rule = "FALSE", modes = "1",
                basic_prior_mtd_input = 2, basic_skip_esc_input = "TRUE")
  set_td(saved["ttl_inputt"])
  set_td(saved[names(saved) != "ttl_inputt"])

  file <- app$get_download(td("config_save_button"))
  settings <- read.csv(file)
  expect_setequal(settings$inputId, config_variables)

  # Change them all, then load the file.
  set_td(list(ttl_inputt = 0.25))
  set_td(list(cohort_inputt = 3, max_size_inputt = 30, halfwidth_input = 0.1,
              prior_var_input = 1, skip_esc_crm_input = "FALSE",
              ttl_multiple_phi_1 = 0.6, boin_stopping_rule = "TRUE", modes = "2",
              basic_prior_mtd_input = 3, basic_skip_esc_input = "FALSE"))
  app$upload_file(`trial_design-config_file_upload` = file)
  app$wait_for_idle()

  for (id in names(saved)) {
    value <- app$get_value(input = td(id))
    expect_equal(as.character(value), as.character(saved[[id]]), label = id)
  }
  # The BOIN probabilities follow the loaded multiple and target.
  expect_equal(app$get_value(input = td("direct_phi_1")), 0.7 * 0.3, tolerance = 1e-6)
})

test_that("each design's plots carry its own name when viewed individually", {
  # The names of the plots offered after one run, in a fresh app.
  plot_names <- function(designs) {
    app <- AppDriver$new(test_path("..", ".."), name = "plot-labels",
                         load_timeout = 60 * 1000, timeout = 20 * 1000)
    on.exit(app$stop(), add = TRUE)
    app$set_inputs(nav = "Simulation")
    app$set_inputs(`simulation-simulation_design_selection_input` = designs)
    wait_for_picker(app, "simulation-scen_output_input")
    app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
    wait_for_picker(app, "simulation-metric_selection_input")
    app$set_inputs(`simulation-metric_selection_input` = "% times dose was selected as MTD")
    app$set_inputs(`simulation-comparative_view` = "Individually")
    app$click("simulation-run_simulation")
    app$wait_for_idle(timeout = 10 * 60 * 1000)
    app$click(selector = "a[data-value='Simulation Output - Plots']")
    app$wait_for_idle()
    expect_equal(count_errors(app), 0)
    unlist(app$get_js(
      "Object.keys(document.getElementById('simulation-ind_graph').selectize.options)"
    ))
  }

  # Without 3+3, CRM's plot was labelled BOIN and BOIN's was missing.
  names <- plot_names(c("CRM", "BOIN"))
  expect_length(names, 2)
  expect_true(any(grepl("for CRM ", names)), label = "a CRM plot")
  expect_true(any(grepl("for BOIN ", names)), label = "a BOIN plot")

  # With one design there were no plots at all.
  expect_match(plot_names("CRM"), "for CRM ")
})

test_that("the cohort size and the BOIN stopping rule reach the simulation", {
  app <- AppDriver$new(test_path("..", ".."), name = "cohort-size",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # One cohort of 4 fills the trial, so every patient gets the starting dose.
  # The default cohort size of 3 would hide a cohort size that is not used.
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(display_crm = TRUE, display_boin = TRUE, display_input_all = TRUE)

  # 30 patients in cohorts of 4: the last cohort takes a trial to 32.
  app$set_inputs(`trial_design-cohort_inputt` = 4)
  app$wait_for_idle()
  expect_match(app$get_value(output = "trial_design-cohort_multiple_warning"), "reach 32 patients")

  app$set_inputs(`trial_design-max_size_inputt` = 4, `trial_design-cohort_inputt` = 4,
                 `trial_design-stop_n_mtd_input` = 4, `trial_design-stop_n_mtd_boin` = 4,
                 `trial_design-boin_cohorts` = 1, `trial_design-boin_stopping_rule` = "FALSE")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "trial_design-cohort_multiple_warning"), "")

  app$set_inputs(nav = "Simulation")
  app$set_inputs(`simulation-simulation_design_selection_input` = c("CRM", "BOIN"))
  wait_for_picker(app, "simulation-scen_output_input")
  app$set_inputs(`simulation-scen_output_input` = c("Scenario 2", "Scenario 3"))
  wait_for_picker(app, "simulation-metric_selection_input")
  app$set_inputs(`simulation-metric_selection_input` = c(
    "% participants treated at dose", "% times dose was selected as MTD"
  ))
  app$set_inputs(`simulation-comparative_view` = "Individually")
  app$click("simulation-run_simulation")
  app$wait_for_idle(timeout = 10 * 60 * 1000)
  app$click(selector = "a[data-value='Simulation Output - Tables']")
  app$wait_for_idle()
  expect_equal(count_errors(app), 0)

  for (design in c("CRM", "BOIN")) {
    treated <- first_row(app, design, "Scenario 2", "Treated")
    expect_equal(unname(treated[1]), 100, label = paste(design, "% treated at dose 1"))
  }
  # With the stopping rule off, BOIN never stops for toxicity, even when every
  # dose is too toxic.
  expect_equal(unname(first_row(app, "BOIN", "Scenario 3", "MTD")["NoDose"]), 0)
})

test_that("CRM and BOIN results do not depend on which other designs run", {
  # CRM and BOIN results for Scenario 1 after one run, in a fresh app.
  results <- function(designs) {
    app <- AppDriver$new(test_path("..", ".."), name = "seed",
                         load_timeout = 60 * 1000, timeout = 20 * 1000)
    on.exit(app$stop(), add = TRUE)
    app$set_inputs(nav = "Simulation")
    expect_match(app$get_text("body"), paste("seed", default_design$seed))
    app$set_inputs(`simulation-simulation_design_selection_input` = designs)
    wait_for_picker(app, "simulation-scen_output_input")
    app$set_inputs(`simulation-scen_output_input` = "Scenario 1")
    wait_for_picker(app, "simulation-metric_selection_input")
    app$set_inputs(`simulation-metric_selection_input` = "% times dose was selected as MTD")
    app$click("simulation-run_simulation")
    app$wait_for_idle(timeout = 10 * 60 * 1000)
    app$click(selector = "a[data-value='Simulation Output - Tables']")
    app$wait_for_idle()
    expect_equal(count_errors(app), 0)
    lapply(c(CRM = "CRM", BOIN = "BOIN"), function(design) {
      first_row(app, design, "Scenario 1", "MTD")
    })
  }

  # 3+3 used to set the seed for every design, and CRM and BOIN set none.
  alone <- results(c("CRM", "BOIN"))
  with_tpt <- results(c("3+3", "CRM", "BOIN"))
  expect_equal(with_tpt$CRM, alone$CRM)
  expect_equal(with_tpt$BOIN, alone$BOIN)
})

test_that("Conduct uses the same designs as Simulation", {
  app <- AppDriver$new(test_path("..", ".."), name = "conduct-designs",
                       load_timeout = 60 * 1000, timeout = 20 * 1000)
  on.exit(app$stop(), add = TRUE)

  # Non-default settings that the old Conduct designs ignored: with them, CRM
  # skips to dose 5 after no DLTs at dose 2 (old: 3), and BOIN stays at dose 2
  # after 1 DLT in 3 (old: 1).
  d <- default_design
  app$set_inputs(nav = "Trial Design")
  app$set_inputs(display_crm = TRUE, display_boin = TRUE, display_input_all = TRUE)
  app$set_inputs(`trial_design-start_dose_inputt` = 2, `trial_design-skip_esc_crm_input` = "TRUE",
                 `trial_design-prior_var_input` = 2.5, `trial_design-stop_n_mtd_input` = 6,
                 `trial_design-direct_phi_2` = 0.45, `trial_design-boin_stopping_rule` = "FALSE")
  app$wait_for_idle()

  skeleton <- dfcrm::getprior(halfwidth = d$crm$halfwidth, target = d$ttl,
                              nu = d$crm$prior_mtd, nlevel = d$n_doses)
  designs <- list(
    CRM = crm_design(skeleton, d$ttl, d$max_size, prior_var = 2.5, skip_esc = TRUE,
                     d$crm$skip_deesc, d$crm$stop_tox_x, d$crm$stop_tox_y, stop_n_mtd = 6),
    BOIN = boin_design(d$n_doses, d$ttl, d$max_size, d$boin$stop_n_mtd, p_tox = 0.45,
                       p_saf = d$boin$phi_1, use_stopping_rule = FALSE),
    `3+3` = tpt_design(d$n_doses, d$max_size)
  )
  outcomes <- c(CRM = "2NNN", BOIN = "2NNT", `3+3` = "2NNN")

  app$set_inputs(nav = "Conduct")
  for (design in names(designs)) {
    app$set_inputs(`conduct-choice` = design)
    app$click("conduct-update_design")
    app$wait_for_idle()
    if (design == "BOIN") {
      # A DLT for the third patient of the first cohort.
      app$set_inputs(`conduct-editable_table_cell_edit` = list(row = 3, col = 3, value = "1"),
                     allow_no_input_binding_ = TRUE)
      app$wait_for_idle()
    }
    expected <- recommended_dose(fit(designs[[design]], outcomes[[design]]))
    expect_equal(app$get_value(output = "conduct-recommended_dose"), as.character(expected),
                 label = paste(design, "recommended dose"))
  }
  expect_equal(count_errors(app), 0)
})
