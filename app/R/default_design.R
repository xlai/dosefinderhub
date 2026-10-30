# Default trial design, used wherever the app sets a starting value.
# Values signed off for v0.1.0, see https://github.com/xlai/dosefinderhub/issues/91

default_design <- list(
  # General
  n_doses = 5,
  ttl = 0.25,
  max_size = 30,
  start_dose = 1,
  cohort_size = 3,

  # CRM (the skeleton is built from the target, halfwidth and prior MTD in Trial Design)
  crm = list(
    skip_esc = FALSE,
    skip_deesc = TRUE,
    above_target = TRUE,
    prior_var = 1.34,
    stop_n_mtd = 9,
    prior_mtd = 3,
    halfwidth_multiple = 0.25,
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

# CRM skeleton halfwidth implied by the defaults
default_design$crm$halfwidth <- default_design$crm$halfwidth_multiple * default_design$ttl

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

# Trial Design inputs saved and loaded by the settings file. The BOIN
# probabilities and boundaries follow from the saved multiples of the target,
# and the CRM skeleton from the target, halfwidth and prior MTD guess.
config_numeric_inputs <- c(
  "n_doses_inputt", "ttl_inputt", "max_size_inputt", "start_dose_inputt", "cohort_inputt",
  "prior_var_input", "stop_n_mtd_input", "prior_mtd_input", "halfwidth_input",
  "stop_tox_x_input", "stop_tox_y_input",
  "boin_cohorts", "stop_n_mtd_boin", "ttl_multiple_phi_1", "ttl_multiple_phi_2"
)
config_choice_inputs <- c(
  "skip_esc_crm_input", "skip_deesc_crm_input", "above_target_input", "boin_stopping_rule"
)
config_variables <- c(config_numeric_inputs, config_choice_inputs)
