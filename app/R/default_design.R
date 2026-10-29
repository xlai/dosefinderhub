# Default trial design, used wherever the app sets a starting value.
# Values signed off for v0.1.0, see https://github.com/xlai/dosefinderhub/issues/91

default_design <- list(
  # General
  n_doses = 5,
  ttl = 0.25,
  max_size = 30,
  start_dose = 1,
  cohort_size = 3,

  # CRM (the skeleton is built from the target and prior MTD in Trial Design)
  crm = list(
    skip_esc = FALSE,
    skip_deesc = TRUE,
    above_target = TRUE,
    prior_var = 1.34,
    stop_n_mtd = 9,
    prior_mtd = 3,
    stop_tox_x = 0.09,
    stop_tox_y = 0.77
  ),

  # BOIN
  boin = list(
    stopping_rule = TRUE,
    n_cohorts = 3,
    stop_n_mtd = 9,
    phi_1_multiple = 0.6,
    phi_2_multiple = 1.4
  ),

  # 3+3
  tpt = list(
    allow_deescalate = FALSE
  ),

  # Simulation
  n_sims = 100,
  seed = 12345,
  scenarios = rbind(
    c(0.08, 0.14, 0.25, 0.40, 0.57), # target at dose 3
    c(0.02, 0.04, 0.08, 0.14, 0.25), # all doses safe
    c(0.40, 0.57, 0.73, 0.84, 0.91)  # all doses too toxic
  )
)

# BOIN toxicity thresholds and escalation boundaries implied by the defaults
default_design$boin$phi_1 <- default_design$boin$phi_1_multiple * default_design$ttl
default_design$boin$phi_2 <- default_design$boin$phi_2_multiple * default_design$ttl
default_boin_boundary <- BOIN::get.boundary(
  target = default_design$ttl,
  ncohort = default_design$max_size / default_design$cohort_size,
  cohortsize = default_design$cohort_size,
  p.saf = default_design$boin$phi_1,
  p.tox = default_design$boin$phi_2
)
default_design$boin$lambda_e <- round(default_boin_boundary$lambda_e, 3)
default_design$boin$lambda_d <- round(default_boin_boundary$lambda_d, 3)

# Names of the designs offered in the app
design_names <- c("CRM", "3+3", "BOIN")

# Settings saved and loaded by the Trial Design configuration file
config_variables <- c(
  "n_doses", "ttl", "max_n", "start_dose", "cohort_size",
  "skip_esc", "skip_deesc", "no_esc_if_observed_gt_target", "prior_var", "stop_n_mtd",
  "prior_ttp", "prior_mtd", "stop_tox_x", "stop_tox_y", "tpt_allow_deesc"
)
