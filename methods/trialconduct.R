# pre-load packages while testing

library(ape)
library(data.tree)
library(dplyr)
library(escalation)
library(RColorBrewer)
library(DiagrammeR)

# reads config from a .yaml file
model_config <- yaml::yaml.load_file("~/dosefinderhub/methods/config.yaml")

# can't define these in the fucking stupid .yaml for some goddamn reason

max_n <- 24
skip_esc <- FALSE
skip_deesc <- TRUE
stop_tox_x <- 0.1 
stop_tox_y <- 0.7
stop_n_mtd <- 25
num_doses <- 4

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
    escalation::stop_at_n(n = max_n),
  crm = model %>% 
    escalation::dont_skip_doses(when_escalating = 1-skip_esc, when_deescalating = 1-skip_deesc) %>% 
    escalation::stop_when_too_toxic(dose = 1, stop_tox_x + config$ttl, confidence = stop_tox_y) %>%
    escalation::stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>%
    escalation::stop_at_n(n = max_n)
  )

  return(model)
}

# quick define variables

design <- "crm"

outcomes <- '3NNN 4NNT'

cohort_size <- 3

# outputs

# the model

tc_model <- create_model(design, model_config)

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