# reads config from a .yaml file
#model_config <- yaml::yaml.load_file("~/dosefinderhub/methods/config.yaml")

# setup piping
`%>%` <- magrittr::`%>%`

# xiaoran's custom function for loading a function from a package
get_function_from_package <- function(full_func_name) {
  func_name <- unlist(strsplit(full_func_name, "::"))
  package_name <- func_name[1]
  function_name <- func_name[2]
  func <- getExportedValue(package_name, function_name)
  return(func)
}

# function that processes config into machine-readable format
process_config <- function(model_config, model_type){
  # assume model_config is a list of three elements - trial, method and ranking
  # subset config that contains only model_type
  # select only the q_vaiable and value (currently X (blank) and answer)
  config_method <- model_config %>% dplyr::filter(method == model_type) %>% dplyr::select(X, answer)
  config_trial <- model_config %>% dplyr::filter(method == "config") %>% dplyr::select(X, answer)
  config_processed <- rbind(config_method, config_trial)
  # change No to F, Yes to T
  config_processed[config_processed == "No"] <- FALSE 
  config_processed[config_processed == "Yes"] <- TRUE

  # convert this dataframe into a list, where each element is q_variable
  config_processed_list <- as.list(config_processed$answer)
  names(config_processed_list) <- as.list(config_processed$X)

  # convert to numeric
  config_processed_list <- sapply(config_processed_list, function(x) {
    if (is.na(as.numeric(x))){
      return(x)
      } else {
        return(as.numeric(x))
             }

return(config_processed_list)
})

}

test <- process_config(read.csv("methods/dummy_data1.csv"), "tpt")

# function for model creation
create_model <- function(model_config, model_type) {
  # pre-process config
  config <- process_config(model_config, model_type)
  # map the variables from config to the corresponding escalation functions
  func_name <- get_function_from_package(config$func)
  model <- do.call(func_name, config$args)
  model <- switch(model_type,
  tpt = escalation::get_three_plus_three(model_config) %>%
    escalation::stop_at_n(n = config$max_n),
  crm = escalation::get_dfcrm(model_config) %>% 
    escalation::dont_skip_doses(when_escalating = 1-config$skip_esc, when_deescalating = 1-config$skip_deesc) %>% 
    escalation::stop_when_too_toxic(dose = 1, config$stop_tox_x + config$target, confidence = config$stop_tox_y) %>%
    escalation::stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n)
  )

  return(model)
}

# function for conducting sims and returning output table

process_sims <- function(model, model_type, model_config, sim_config) {
  
  config <- process_config(model_config)
  # sim_config, a lists consists scenarios, n_sims

  # process scenarios into a list (check)
  sim_scenarios <- as.list(sim_config$scenarios)
  # assume n_sims is a list (name of each element would be model_type)

  sims <- model %>%
  escalation::simulate_trials(next_dose = config$start_dose, n_sims$model_type, config$true_dlt_ss, NULL)
  
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
  selection_tab <- rbind(t(selection_df), c(NA,t(treatedpct_df)), c(NA,true_dlt_ss))
  
  rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities") 

  # return both sims object and table

  pslist <- list(design, sims, selection_tab)
  return(pslist)
}

# example usage of create_model() and process_sims():

o_model <- create_model("crm", model_config) 

o_sims <- o_model %>% process_sims(start_dose = 1, n_sims = 10, true_dlt_ss = c(0.01,0.05,0.2,0.8))

# add error for number of doses in true_dlt_ss being different to num_doses!
# or programmatically make it impossible? reactibve table input?

