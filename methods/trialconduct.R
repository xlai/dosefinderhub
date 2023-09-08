# pre-load packages while testing

library(ape)
library(data.tree)
library(dplyr)
library(escalation)
library(RColorBrewer)
library(DiagrammeR)


# read in .rData file
data <- readRDS("dummy_data1.RData")
attach(data)
# the data can now be called directly with trial, method, ranking

# setup piping
`%>%` <- magrittr::`%>%`

# function that processes config into machine-readable format
process_config <- function(model_config, model_type){
  # model_config is a list of three elements - trial, method and ranking
  # subset config that contains only model_type
  config_method <- model_config$method %>% dplyr::filter(design == model_type) %>% dplyr::select(q_variable, value)
  # the trial object only contains non-specific info, but we still need to subset the config-specific info
  config_trial <- model_config$trial %>% dplyr::filter(config == "Y") %>% dplyr::select(q_variable, value)

  config_processed <- rbind(config_method, config_trial)
  # change No to 0, Yes to 1. Not T/F just so that I may convert all to numeric easily!
  config_processed[config_processed == "No"] <- 0
  config_processed[config_processed == "Yes"] <- 1

  #convert this dataframe into a list, where each element is q_variable
  config_processed_list <- as.list(as.numeric(config_processed$value)) # Note: this may need to be changed for future non-numeric config values
  names(config_processed_list) <- as.list(config_processed$q_variable)

return(config_processed_list)

}

# function for model creation
create_model <- function(model_config, model_type) {
  # pre-process config
  config <- process_config(model_config, model_type)

  # map the variables from config to the corresponding escalation functions
  # only those directly used as arguments in the model need to be mapped. not needed for e.g. stopping rules
  # the !! is to stop an error from occuring when the variable to be renamed does not exist
  
 # config_names <- names(config)

  #config_names[config_names == "n_doses"] <- "num_doses"
 # config_names[config_names == "tpt_allow_deesc"] <- "allow_deescalate"
 # config_names[config_names == "prior_ttp"] <- "skeleton"
 # config_names[config_names == "ttl"] <- "target"
 # config_names[config_names == "prior_var"] <- "scale"
  
 # names(config) <- config_names

 # oops, this isn't needed as it isn't being fed directly into args anymore since the .yaml code. will delete soon

  #func_name <- get_function_from_package(config)
  model <- switch(model_type,
  tpt = escalation::get_three_plus_three(config$n_doses, config$tpt_allow_deesc) %>%
    escalation::stop_at_n(n = config$max_n),
  crm = escalation::get_dfcrm(config$n_doses, config$prior_ttp, config$ttl, config$prior_var) %>% 
    escalation::dont_skip_doses(when_escalating = 1-config$skip_esc, when_deescalating = 1-config$skip_deesc) %>% 
    escalation::stop_when_too_toxic(dose = 1, config$stop_tox_x + config$target, confidence = config$stop_tox_y) %>%
    escalation::stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n)
  )

  return(model)
}

# quick define variables

design <- "tpt" # note: tpt does NOT support sampling (not bayesian, no posterior)

outcomes <- '3NNN 4NNT'

cohort_size <- 3

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

tc_o_posterior <- lapply((tc_model_fit %>% escalation::prob_tox_samples())[,2:num_doses],median)

# the dose path object

tc_o_dose_paths <- tc_model %>% escalation::get_dose_paths(previous_outcomes = outcomes, cohort_sizes = rep(cohort_size,2))

# the dose path table

tc_o_dose_paths_table <- escalation::spread_paths(dose_finding_paths = dplyr::as_tibble(tc_o_dose_paths),max_depth = 2) %>%
dplyr::select(outcomes0, next_dose0, outcomes1, next_dose1, outcomes2, next_dose2)

# unspread tibble

tc_o_dose_paths_tibble <- dplyr::as_tibble(tc_o_dose_paths)

#

#tc_o_dose_paths_graph <- set_edge_direction(tc_o_dose_paths_graph, direction = "TB")




#xd <- data.tree::as.Node(tc_o_dose_paths_graph)