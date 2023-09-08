# read in .rData file
design <- "tpt"
data <- readRDS("dummy_data1.RData")
attach(data)
# the data can now be called directly with trial, method, ranking

# setup piping. we want to avoid loading packages with library()
`%>%` <- magrittr::`%>%`

# Xiaoran's custom function for loading a function from a package
#get_function_from_package <- function(full_func_name) {
  #func_name <- unlist(strsplit(full_func_name, "::"))
 # package_name <- func_name[1]
 # function_name <- func_name[2]
 # func <- getExportedValue(package_name, function_name)
 # return(func)
#}

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

# function for conducting sims and returning output

process_sims <- function(model_config, model_type, sim_config) {

  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  # IMPORTANT: sim_config <- process_config(sim_config)
  # sim_config is a list that consists scenarios, n_sims and display options. to be expanded upon with UI development

  # sim_scenarios <- as.list(sim_config$scenarios)
  # assume n_sims is a list (name of each element would be model_type)

  sims <- model %>%
  escalation::simulate_trials(next_dose = config$start_dose, num_sims = 100, true_prob_tox = c(0.05,0.1,0.3,0.4)) # note: temp direct use of n_sims, true DLTs. this SHOULD be part of sim_config
  
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
  selection_tab <- rbind(t(selection_df), c(NA,t(treatedpct_df)), c(NA,c(0.05,0.1,0.3,0.4)))
  
  rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities") 

  # return both sims object and table

  pslist <- list(design, sims, selection_tab)
  return(pslist)
}

o_sims <- process_sims(data, design)

# add error for number of doses in true_dlt_ss being different to num_doses!
# or programmatically make it impossible? reactibve table input?
