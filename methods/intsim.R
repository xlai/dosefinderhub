# reads config from a .yaml file
model_config <- yaml::yaml.load_file("~/dosefinderhub/methods/config.yaml")

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

# function for model creation
create_model <- function(model_type, model_config) {
  config <- model_config[[model_type]]
  func_name <- get_function_from_package(config$func)
  model <- do.call(func_name, config$args)
  model <- switch(model_type,
  tpt = model %>%
    stop_at_n(n = config$max_n),
  crm = model %>% 
    dont_skip_doses(when_escalating = 1-config$skip_esc, when_deescalating = 1-config$skip_deesc) %>% 
    stop_when_too_toxic(dose = 1, config$stop_tox_x + config$ttl, confidence = config$stop_tox_y) %>%
    stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended") %>%
    stop_at_n(n = config$max_n)
  )

  return(model)
}

# function for conducting sims and returning output table

process_sims <- function(model, start_dose, n_sims, true_dlt_ss, ...) {
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
  selection_tab <- rbind(t(selection_df), c(NA,t(treatedpct_df)), c(NA,true_dlt_ss))
  
  rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities")

  # return both sims object and table

  pslist <- list(sims, selection_tab)
  return(pslist)
}

# example usage of create_model() and process_sims():

# o_model <- create_model("tpt", model_config) 

# o_sims <- o_model %>% process_sims("tpt", start_dose = 1, n_sims = 100, true_dlt_ss = c(0.01,0.02,0.05,0.2,0.8))
