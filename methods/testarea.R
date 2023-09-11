# read in .rData file
design <- "crm"
data <- readRDS("dummy_data1.RData")
attach(data)
# the data can now be called directly with trial, method, ranking

# setup piping. we want to avoid loading packages with library()
`%>%` <- magrittr::`%>%`

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
    escalation::stop_at_n(n = config$max_n)
  )

  return(model)
}

# function for conducting sims and returning output

process_sims <- function(model_config, model_type, sim_config) {

  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  # IMPORTANT: sim_config <- process_config(sim_config)
  # sim_config is a list that consists scenarios, n_sims and display options. to be expanded upon with UI development

  # sim_scenarios <- as.list(sim_config$scenarios)
  # assume n_sims is a list (name of each element would be model_type)

  sims <- model %>%
  escalation::simulate_trials(next_dose = config$start_dose, num_sims = 100, true_prob_tox = c(0.05,0.1,0.3,0.4))
  # note: temp direct use of n_sims, true DLTs. this SHOULD be part of sim_config
  
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
  selection_tab <- rbind(t(selection_df), c(NA,t(treatedpct_df)), c(NA,c(0.2,0.55,0.6,0.99)))
  
  rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities") 

  # return both sims object and table

  pslist <- list(design, sims, selection_tab)
  return(pslist)
}

o_sims <- process_sims(data, design)
