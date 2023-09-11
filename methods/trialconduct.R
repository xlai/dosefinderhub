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
  true_dlt_ss$S1 <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss$S2 <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss$S3 <- sort(sample(seq(0, 1, 0.01), n_doses))

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
  )

  return(model)
}

# quick define variables

design <- "tpt" # note: tpt does NOT support sampling (not bayesian, no posterior)

outcomes <- '1NNN 2NNT'

n_cohorts <- 2
cohort_size <- 3

# reading in the reactive table to find the outcome string (e.g. 1NN 2NNT)

# Make up some data
prev_outcomes <- data.frame(
  cohort = c(1, 1, 1, 2, 2, 2),
  dose_level = c(1, 1, 1, 2, 2, 2),
  dlt = c(0, 0, 0, 1, 0, 0),
  evaluable = c(1, 1, 1, 1, 1, 1)
)

prev_outcomes$dlt_nt <- ifelse(prev_outcomes$dlt == 1, "T", "N")

# Define the parameters
n_cohorts <- max(prev_outcomes$cohort)
cohort_size <- sum(prev_outcomes$cohort == 1)

# Initialize the outcome_string vector
outcome_string <- character(n_cohorts)

# Loop through the data
for (i in 1:nrow(prev_outcomes)) {
  cohort_number <- prev_outcomes$cohort[i]
  dose_level <- prev_outcomes$dose_level[i]
  dlt_nt <- prev_outcomes$dlt_nt[i]
  
  if (i %% cohort_size == 1) {
    outcome_string[cohort_number] <- paste(dose_level, dlt_nt, collapse = " ")
  } else {
    outcome_string[cohort_number] <- paste(outcome_string[cohort_number], dlt_nt, collapse = "")
  }
}

# Combine the cohort-specific strings
outcome_string <- paste(outcome_string, collapse = " ")

# Print the outcome_string
print(outcome_string)

# outputs

# the model

tc_model <- create_model(data, design)

# the fitted outcomes to the model

tc_model_fit <- tc_model %>% escalation::fit(outcomes)

# the should this trial continue T/F indicator

tc_o_continue <- tc_model_fit %>% escalation::continue()

# the model frame which should be the same as the user input in reactive table, minus the non-evaluables

tc_o_df <- tc_model_fit %>% escalation::model_frame()

# the posterior updates

# for this, there is a function called supports_sampling() in escalation that we can use as a switch to run this if it does

#tc_o_posterior <- lapply((tc_model_fit %>% escalation::prob_tox_samples())[,2:num_doses],median)

# the dose path object

tc_o_dose_paths <- tc_model %>% escalation::get_dose_paths(previous_outcomes = outcomes, cohort_sizes = rep(cohort_size,3))

# the dose path table

tc_o_dose_paths_table <- escalation::spread_paths(dose_finding_paths = dplyr::as_tibble(tc_o_dose_paths),max_depth = 3) %>%
dplyr::select(outcomes0, next_dose0, outcomes1, next_dose1, outcomes2, next_dose2)

# unspread tibble

tc_o_dose_paths_tibble <- dplyr::as_tibble(tc_o_dose_paths)

# maybe use this to warn user of oversized tables
n_nodes <- escalation::num_dose_path_nodes(2,c(3,3,3))

#tc_o_dose_paths_graph <- set_edge_direction(tc_o_dose_paths_graph, direction = "TB")




#xd <- data.tree::as.Node(tc_o_dose_paths_graph)