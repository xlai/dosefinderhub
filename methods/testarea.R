# read in .rData file
design <- "boin"
data <- readRDS("dummy_data4.RData")
attach(data)
# the data can now be called directly with trial, method, ranking subsets

# setup piping. we want to avoid loading packages with library()
`%>%` <- magrittr::`%>%`

# create some sim data
create_dummy_sims <- function(n_doses) {

  n_sims <- sample(seq(20, 100, 10), 1)

  true_dlt_ss <- list()
  true_dlt_ss[[1]] <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss[[2]] <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss[[3]] <- sort(sample(seq(0, 1, 0.01), n_doses))

  value <- list(n_sims = n_sims, true_dlt_ss = true_dlt_ss)

  saveRDS(value, "dummy_sims_data")
  dummy_sims_data <- readRDS("dummy_sims_data")
  return(dummy_sims_data)
}

# function that processes config into machine-readable format
process_config <- function(model_config, model_type){
  # model_config is a list of three elements - trial, method and ranking
  # subset config that contains only model_type
  config_method <- model_config$method %>% dplyr::filter(design == model_type) %>% dplyr::select(q_variable, value, answer_type)
  # the trial object only contains non-specific info, but we still need to subset the config-specific info
  config_trial <- model_config$trial %>% dplyr::filter(config == "Y") %>% dplyr::select(q_variable, value, answer_type)

  config_processed <- data.frame(rbind(config_method, config_trial))
  
  # convert this dataframe into a list, where each element is q_variable
  config_processed_list <- as.list(config_processed$value)
  names(config_processed_list) <- as.list(config_processed$q_variable)

  # format all data
  for (i in 1:length(config_processed_list)){
      if (config_processed$answer_type[i] == "numeric"){
        config_processed_list[[i]] <- as.numeric(config_processed$value[i])}
      else if (config_processed$answer_type[i] == "numeric_bounded"){
        config_processed_list[[i]] <- as.numeric(config_processed$value[i])}
      else if (config_processed$value[i] == "No"){
        config_processed_list[[i]] <- FALSE}
      else if (config_processed$value[i] == "Yes"){
        config_processed_list[[i]] <- TRUE}
      else if (config_processed$answer_type[i] == "comma-separated list"){
        config_processed_list[[i]] <- as.numeric(unlist(strsplit(config_processed_list[[i]],",")))}
      }

return(config_processed_list)

}


# function for model creation
create_model <- function(model_config, model_type) {
  
  # pre-process config
  config <- process_config(model_config, model_type)

  #func_name <- get_function_from_package(config)
  model <- switch(model_type,
  tpt = escalation::get_three_plus_three(config$n_doses, config$tpt_allow_deesc) %>%
    escalation::stop_at_n(n = config$max_n),
  crm = escalation::get_dfcrm(skeleton = config$prior_ttp, target = config$ttl, scale = config$prior_var) %>% 
    escalation::dont_skip_doses(when_escalating = as.logical(1-config$skip_esc), when_deescalating = as.logical(1-config$skip_deesc)) %>% 
    escalation::stop_when_too_toxic(dose = 1, config$stop_tox_x + config$target, confidence = config$stop_tox_y) %>%
    escalation::stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n),
  boin = escalation::get_boin(num_doses = n_doses, target = ttl, use_stopping_rule = use_stopping_rule, p.saf = p_saf,
                              p.tox = p_tox, cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05) %>%
    escalation::stop_when_n_at_dose(n=14, dose = "recommended") %>%
    escalation::stop_at_n(n = max_n)
  )

  return(model)
}

# function for conducting sims and returning output


process_sims <- function(model_config, model_type, sim_data) {


  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  sim_data <- create_dummy_sims(config$n_doses)

# for each sim scenario
  i <- 1

  # pre-create list objects?

  sims <- list()
  selection <- list()
  treated <- list()
  treatedpct <- list()
  selection_df <- list()
  treatedpct_df <- list()
  selection_tab <- list()
  best_dose <- list()
  best_dose_level <- list()
  dist_accruacy <- list()
  mean_accruacy <- list()
  dist_overdose <- list()
  mean_overdose <- list()
  dist_length <- list()
  mean_length <- list()
  metrics <- list()
  

  sims[[i]] <- model %>%
  escalation::simulate_trials(next_dose = config$start_dose, num_sims = sim_data$n_sims, true_prob_tox = sim_data$true_dlt_ss[[i]], sample_patient_arrivals = patient_arrivals_func) %>%

  return(sims)
  }


## process_sims with cohort size specified:

process_sims_cohort_size <- function(model_config, model_type, sim_data) {


  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  sim_data <- create_dummy_sims(4)
  patient_arrivals_func <- function(current_data) cohorts_of_n(n = cohort_size)

# for each sim scenario
  i <- 1

  # pre-create list objects?

  sims <- list()
  selection <- list()
  treated <- list()
  treatedpct <- list()
  selection_df <- list()
  treatedpct_df <- list()
  selection_tab <- list()
  best_dose <- list()
  best_dose_level <- list()
  dist_accruacy <- list()
  mean_accruacy <- list()
  dist_overdose <- list()
  mean_overdose <- list()
  dist_length <- list()
  mean_length <- list()
  metrics <- list()
  

  sims[[i]] <- model %>%
    escalation::simulate_trials(next_dose = config$start_dose, num_sims = sim_data$n_sims, true_prob_tox = sim_data$true_dlt_ss[[i]],
                                sample_patient_arrivals = patient_arrivals_func)

  return(sims)
  }



o_sims <- process_sims(data, design, dummy_sims_data)
o_sims

# add error for number of doses in true_dlt_ss being different to num_doses!
# or programmatically make it impossible? reactibve table input?

