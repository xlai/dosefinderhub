################ Xiaoran's Code ################

library(shiny)
library(DT)
library(ggplot2)
library(here)

here::i_am("app/modules/trial_design/global.R")

# Data loading
data_env <- new.env()
load(here('app', 'data', 'dummy', 'dummy_data6.RData'), envir = data_env)
dummy_data <- data_env$dummy_data 

# Data processing
dummy_data_trial <- dummy_data$trial
n_doses <- dummy_data_trial[dummy_data_trial$q_variable == "n_doses", "value"]
dummy_data_method <- dummy_data$method
dummy_data_ranking <- dummy_data$ranking
ranking <- c(dummy_data_ranking$method)
ranking <- c("crm", "tpt", "other") # Your test ranking

# Functions
prettify_ranking <- function(ranking_argument) {
  pretty_ranking <- ranking_argument
  pretty_ranking[pretty_ranking == "crm"] <- "CRM"
  pretty_ranking[pretty_ranking == "tpt"] <- "3+3"
  pretty_ranking[pretty_ranking == "other"] <- "Other design (TEST)"
  return(pretty_ranking)
}

parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, '[', 1)
  sapply(param_list, '[', 2)
}

# Global variables
pretty_ranking <- prettify_ranking(ranking)
questions <- dummy_data
column_names <- sprintf("d(%d)", 1:n_doses)
