# read in .rData file
design <- "crm"
data <- readRDS("dummy_data1.RData")
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
    escalation::stop_at_n(n = config$max_n)
  boin = escalation::get_boin(num_doses = config$n_doses, target = config$ttl, use_stopping_rule = use_stopping_rule, p.saf = config$p_saf, p.tox = config$p_tox,
    cutoff.eli = config$cutoff_eli, extrasafe = config$extrasafe, offset = config$offset) %>%
    escalation::stop_when_n_at_dose(n=config$n_earlystop, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n))

  return(model)
}

# function for conducting sims and returning output

process_sims <- function(model_config, model_type, sim_data) {


  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  sim_data <- create_dummy_sims(4)
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
  dist_accuracy <- list()
  mean_accuracy <- list()
  dist_overdose <- list()
  mean_overdose <- list()
  dist_length <- list()
  mean_length <- list()
  metrics <- list()
  pslist <- list()

  # for each sim scenario
  for (i in 1:length(sim_data$true_dlt_ss)){
  

  sims[[i]] <- model %>%
  escalation::simulate_trials(next_dose = config$start_dose, num_sims = sim_data$n_sims, true_prob_tox = sim_data$true_dlt_ss[[i]])
  
  # find selection probs
  selection[[i]] <- sims[[i]] %>% 
  escalation::prob_recommend()
  
  # find no. treated at dose
  treated[[i]] <- colSums(sims[[i]] %>% 
  escalation::n_at_dose())
  treatedpct[[i]] <- treated[[i]] / sum(treated[[i]])
  
  # coerce into table
  selection_df[[i]] <- data.frame(selection[[i]])
  treatedpct_df[[i]] <- data.frame(treatedpct[[i]])
  selection_tab[[i]] <- rbind(t(selection_df[[i]]), c(NA,t(treatedpct_df[[i]])), c(NA,sim_data$true_dlt_ss[[i]]))
  
  rownames(selection_tab[[i]]) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities") 

  # quick and ugly listing of variables
  o_config <- rbind(design,data.frame(unlist(config)))

  # metrics

  best_dose[[i]] <- max(sim_data$true_dlt_ss[[i]][sim_data$true_dlt_ss[[i]]<=config$ttl])
  best_dose_level[[i]] <- match(best_dose[[i]],sim_data$true_dlt_ss[[i]])

  # i) accuracy

  dist_accuracy[[i]] <- escalation::recommended_dose(sims[[i]])
  mean_accuracy[[i]] <- length(subset(dist_accuracy[[i]],dist_accuracy[[i]] == best_dose_level[[i]])) / sim_data$n_sims

  # ii) risk of overdosing

  dist_overdose[[i]] <- escalation::num_tox(sims[[i]])
  mean_overdose[[i]] <- mean(dist_overdose[[i]])

  # iii) trial length

  dist_length[[i]] <- sims[[i]] %>% escalation::trial_duration()
  mean_length[[i]] <- mean(dist_length[[i]])

  # metrics combined;

  metrics[[i]] <- data.frame("Accuracy" = mean_accuracy[[i]], "Risk of Overdose" = mean_overdose[[i]], "Trial Duration" = mean_length[[i]])

  pslist[[i]] <- list(Table = selection_tab[[i]], Metrics = metrics[[i]])
  }
  o_final <- list(Configurations = o_config, Output = pslist)
  return(o_final)
}

o_sims <- process_sims(data, design, sim_data)


# add error for number of doses in true_dlt_ss being different to num_doses!
# or programmatically make it impossible? reactibve table input?
