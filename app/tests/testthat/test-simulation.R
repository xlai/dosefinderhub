# Simulation functions (pages/global.R). Some values differ from the default
# design on purpose, because some bugs only show away from the defaults.

d <- default_design
scenario <- d$scenarios[1, ] # target at dose 3
n_sims <- 20

expect_valid_sim <- function(sim, n_doses, n_sims) {
  expect_equal(dim(sim$selection_tab), c(2, n_doses + 1))
  expect_equal(sum(sim$selection_tab[1, ]), 100, tolerance = 1e-8)
  expect_equal(dim(sim$treatment_tab), c(2, n_doses))
  expect_equal(sum(sim$treatment_tab[1, ]), 100, tolerance = 1e-8)
  expect_gte(sim$mean_accuracy, 0)
  expect_lte(sim$mean_accuracy, 1)
  expect_length(sim$dist_overdose, n_sims)
  expect_true(all(sim$dist_overdose >= 0 & sim$dist_overdose <= 100))
  expect_length(sim$dist_length, n_sims)
}

test_that("the right dose is the highest dose at or below the target", {
  expect_equal(find_best_dose_level(d$scenarios[1, ], d$ttl), 3)
  expect_equal(find_best_dose_level(d$scenarios[2, ], d$ttl), 5)
  expect_true(is.na(find_best_dose_level(d$scenarios[3, ], d$ttl)))
})

test_that("3+3 simulates with the default design", {
  set.seed(d$seed)
  sim <- sim_tpt(d$n_doses, d$ttl, d$max_size, d$start_dose, n_sims, scenario)
  expect_valid_sim(sim, d$n_doses, n_sims)
})

test_that("CRM simulates with a non-default prior variance and stopping rule", {
  skeleton <- dfcrm::getprior(halfwidth = 0.25 * d$ttl, target = d$ttl,
                              nu = d$crm$prior_mtd, nlevel = d$n_doses)
  set.seed(d$seed)
  sim <- sim_crm(d$n_doses, d$ttl, d$max_size, d$start_dose, n_sims, scenario,
                 skeleton, prior_var = 2.5, skip_esc = d$crm$skip_esc,
                 skip_deesc = d$crm$skip_deesc, stop_tox_x = d$crm$stop_tox_x,
                 stop_tox_y = d$crm$stop_tox_y, stop_n_mtd = 6, cohort_size = 4)
  expect_valid_sim(sim, d$n_doses, n_sims)
})

test_that("BOIN simulates with a cohort size of 4", {
  set.seed(d$seed)
  sim <- sim_boin(d$n_doses, d$ttl, max_n = 32, d$start_dose, n_sims, scenario,
                  cohort_size = 4, stop_n_mtd = d$boin$stop_n_mtd,
                  p_tox = d$boin$phi_2, p_saf = d$boin$phi_1,
                  use_stopping_rule = d$boin$stopping_rule, n_cohorts = 8)
  expect_valid_sim(sim, d$n_doses, n_sims)
})

test_that("CRM and BOIN treat whole cohorts of the size they are given", {
  # With 4 patients at most, one cohort of 4 all get the starting dose; with
  # cohorts of 3, a second cohort can move on to dose 2.
  skeleton <- dfcrm::getprior(halfwidth = 0.25 * d$ttl, target = d$ttl,
                              nu = d$crm$prior_mtd, nlevel = d$n_doses)
  pct_at_start <- function(design, cohort_size) {
    set.seed(d$seed)
    sim <- if (design == "CRM") {
      sim_crm(d$n_doses, d$ttl, max_n = 4, d$start_dose, n_sims, d$scenarios[2, ],
              skeleton, d$crm$prior_var, d$crm$skip_esc, d$crm$skip_deesc,
              d$crm$stop_tox_x, d$crm$stop_tox_y, stop_n_mtd = 4, cohort_size = cohort_size)
    } else {
      sim_boin(d$n_doses, d$ttl, max_n = 4, d$start_dose, n_sims, d$scenarios[2, ],
               cohort_size = cohort_size, stop_n_mtd = 4, p_tox = d$boin$phi_2,
               p_saf = d$boin$phi_1, use_stopping_rule = TRUE, n_cohorts = 1)
    }
    sim$treatment_tab[1, d$start_dose]
  }
  for (design in c("CRM", "BOIN")) {
    expect_equal(pct_at_start(design, 4), 100, label = design)
    expect_lt(pct_at_start(design, 3), 100, label = design)
  }
})

test_that("BOIN stops for toxicity only when its stopping rule is on", {
  no_dose_pct <- function(use_stopping_rule) {
    set.seed(d$seed)
    sim <- sim_boin(d$n_doses, d$ttl, d$max_size, d$start_dose, n_sims, d$scenarios[3, ],
                    d$cohort_size, d$boin$stop_n_mtd, p_tox = d$boin$phi_2,
                    p_saf = d$boin$phi_1, use_stopping_rule = use_stopping_rule,
                    n_cohorts = d$boin$n_cohorts)
    sim$selection_tab[1, "NoDose"]
  }
  expect_gt(no_dose_pct(TRUE), 0)
  expect_equal(no_dose_pct(FALSE), 0)
})

test_that("Accuracy counts 'no dose' as right when every dose is too toxic", {
  set.seed(d$seed)
  sim <- sim_tpt(d$n_doses, d$ttl, d$max_size, d$start_dose, n_sims, d$scenarios[3, ])
  no_dose_pct <- sim$selection_tab[1, 1]
  expect_equal(sim$mean_accuracy * 100, no_dose_pct, tolerance = 1e-8)
})
