# read in .rData file
design <- "crm"
data <- readRDS("dummy_data.RData")
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

############### Testing Simulation Ouputs ###################

## Testing conditions approach:

observeEvent(input$submit, {
## List of possible selections - these will be TRUE or FALSE depending on the user's input
# Model
selected_tpt <- {"3+3" %in% input$simulation_design_selection_input}
selected_crm <- {"crm" %in% input$simulation_design_selection_input}

#Scenario (to do later)

# Metric
selected_participant <- {"% participants treated at dose" %in% input$metric_selection_input}
selected_mtd <- {"% times dose was selected as MTD" %in% input$metric_selection_input}
selected_accuracy <- {"Accuracy" %in% input$metric_selection_input}
selected_duration <- {"Duration" %in% input$metric_selection_input}
selected_overdose <- {"Overdosing" %in% input$metric_selection_input}

tpt_sim <- sim_tpt(5, 0.55, 86, 3, 1000, c(0.05, 0.15, 1/3, 0.5, 0.8), 12345)
# crm_sim <- sim_crm()

datasets <- list(tpt_participant = tpt_sim$treated_tab,
                 tpt_mtd = tpt_sim$selection_tab,
                 tpt_accuracy = tpt_sim$mean_accuracy,
                 tpt_duration = tpt_sim$mean_length,
                 tpt_overdose = tpt_sim$mean_overdose#,
                 # crm_participant = crm_sim$treated_tab,
                 # crm_mtd = crm_sim$selection_tab,
                 # crm_accuracy = crm_sim$mean_accuracy,
                 # crm_duration = crm_sim$mean_length,
                 # crm_overdose = crm_sim$mean_overdose
                 )

  model_options <- cbind(rep(selected_tpt, 5)#,
                         #rep(selected_crm, 5)
                         )
  # scenario_options <- c() # To be implemented later
  metric_options <- rep(c(selected_participant, selected_mtd, selected_accuracy, selected_duration, selected_overdose), 1) # replace with 2) when crm is added

  condition_map <- as.data.frame(cbind(model_options, metric_options, datasets))
  filtered_datasets <- df[condition_map$model_options == TRUE & condition_map$metric_options == TRUE, "datasets"]

  output$scen_sim_output <- lappy(list(filtered_datasets, renderDT()))
  })


  ########################### Example Simulation Outputs to Test other Functions ##############################

  tpt_sim <- sim_tpt(5, 1/3, 50, 1, 15, c(0.05, 0.15, 1/3, 0.5, 0.8), TRUE, 12345)
  crm_sim <- sim_crm(5, 1/3, 50, 1, 15, c(0.05, 0.15, 1/3, 0.5, 0.8), c(0.08,0.20,0.35,0.6,0.9), 0.8, FALSE, TRUE, 0.1, 0.7, 25)
  boin_sim <- sim_boin(5, 1/3, 50, 1, 15, c(0.05, 0.15, 1/3, 0.5, 0.8), 4, 25, 1.4/3, 0.6/3, TRUE, 10)

### PLOTS - To return to later. (Comments from Previous Version)
#par(mfrow = c(1,3))
#graph_accuracy <- hist(dist_accuracy$tpt,breaks=11) %>% abline(v=mean_accuracy$tpt, col = "red")
#graph_overdose <- hist(dist_overdose$tpt,breaks=11) %>% abline(v=mean_overdose$tpt, col = "red")
#graph_length <- hist(dist_length$tpt,breaks=11) %>% abline(v=mean_length$tpt, col = "red")

# df for graphs:

#a <- cbind("tpt",dist_accuracy$tpt)
#b <- cbind("crm",dist_accuracy$crm)

#c <- data.frame(rbind(a,b))

#ggplot2::ggplot(data = c, aes(x=X2, y=X1)) + 
#geom_bar(stat = 'identity') +
#scale_fill_manual(values=c("#69b3a2", "#404080")) 

## trial conduct WIP

# assuming we get the following:
# id
# dose level
# cohort size
# number in the cohort that had DLT