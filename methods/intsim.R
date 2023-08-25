`%>%` <- magrittr::`%>%`

# Function for model creation
create_model <- function(model_type, model_config, ...)
{
  config <- model_config[[model_type]]
  model <- switch(model_type,
  tpt = escalation::get_three_plus_three(num_doses = config$n_doses, allow_deescalate = config$tpt_allow_deesc),
  crm = escalation::get_dfcrm(skeleton = config$skeleton, target = config$ttl, scale = sqrt(config$prior_var)) %>% 
    dont_skip_doses(when_escalating = 1-config$skip_esc, when_deescalating = 1-config$skip_deesc) %>% 
    stop_when_too_toxic(dose = 1, config$stop_tox_x + config$ttl, confidence = config$stop_tox_y) %>%
    stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended")
  ) %>%
    stop_at_n(n = config$max_n)

  return(model)
}

process_sims <- function(model, n_sims, true_dlt_ss, ...){

sims <- model %>%
  escalation::simulate_trials(next_dose = start_dose, n_sims, true_dlt_ss, NULL)

# find selection probs

selection <- sims %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated <- colSums(sims %>% 
  escalation::n_at_dose())

treatedpct <- treated / sum(treated)

# coerce into table

selection_df <- data.frame(selection)
treatedpct_df <- data.frame(treatedpct)
selection_tab <- rbind(t(selection_df), t(c(NA,treatedpct_df), c(NA,true_dlt_ss))

rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose" "True Toxicity Probabilities")

# run model

model_tpt <- create_model('tpt',)

model_crm <- create_model('crm',)


# WIP


# trial conduct