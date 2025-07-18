library(shiny)
library(DT)
library(ggplot2)
library(here)

#DUMMY DATA MANIPULATION
here::i_am("app/modules/trial_design/ui.R")
data_directory_dummy <- here('app','data','dummy') #define relative path to your questionnaire app directory
dummy_data <- readRDS(here(data_directory_dummy,"dummy_data1.RData"))
#View(dummy_data)

dummy_data_trial <- dummy_data$trial
#View(dummy_data_trial)
n_doses <- dummy_data_trial[dummy_data_trial$q_variable == "n_doses", "value"]
#n_doses

dummy_data_method <- dummy_data$method
#View(dummy_data_method)

dummy_data_ranking <- dummy_data$ranking
#View(dummy_data_ranking)
ranking <- c(dummy_data_ranking$method)
#ranking
ranking <- c("crm", "tpt", "other") #REMOVE/COMMENT OUT; JUST TO TEST TAB DYNAMICS
prettify_ranking <- function(ranking_argument) {
  pretty_ranking <- ranking_argument
  pretty_ranking[pretty_ranking == "crm"] <- "CRM"
  pretty_ranking[pretty_ranking == "tpt"] <- "3+3"
  pretty_ranking[pretty_ranking == "other"] <- "Other design (TEST)"
  return(pretty_ranking)
}
pretty_ranking <- prettify_ranking(ranking)
#THIS FUNCTION IS NOT FUTURE-PROOF!!!!! I HAVE SPOKEN WITH SIAN ABOUT ADDING "PRETTY" DESIGN NAMES TO DATABASE

##Function to change parameter strings in preparation for extracting UI specs
parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, '[', 1)
  sapply(param_list, '[', 2)
}

questions <- dummy_data

#column_names <- reactive({sprintf("d(%d)", 1:input$n_doses_inputt)})

############################################ Simulation Code ############################################

### This is a rewrite of the basesim.r file as a set of functions that can be used reactively to simulate data.

sim_tpt <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, current_seed) {

 ## Example inputs taken from the original basesim.r file
 # n_doses <- 5
 # ttl <- 0.55
 # max_n <- 86
 # start_dose <- 3
 # current_seed <- 12345
 # true_dlt_ss <- c(0.05,0.15,1/3,0.5,0.8) 

 best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
 best_dose_level <- match(best_dose,true_dlt_ss)
  
 ## lists

model <- list()
sims <- list()

selection <- list()
selection_df <- list()
selection_tab <- list()

treated <- list()
treatedpct <- list()
treatedpct_df <- list()
treatment_tab <- list()

dist_accuracy <- list()
mean_accuracy <- list()

dist_overdose <- list()
mean_overdose <- list()

dist_length <- list()
mean_length <- list()

# Initialize list
  model <- list()
  
  # Set parameters
  tpt_allow_deesc <- TRUE
  
  # Create the model
  model$tpt <- escalation::get_three_plus_three(num_doses = n_doses, 
    allow_deescalate = tpt_allow_deesc, 
    set.seed(current_seed)) %>% 
    stop_at_n(n=max_n)

  # run sims  

sims$tpt <- model$tpt %>% 
  escalation::simulate_trials(next_dose = start_dose, n_sims, true_dlt_ss, NULL)

# find selection probs  

selection$tpt <- sims$tpt %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated$tpt <- colSums(sims$tpt %>% 
  escalation::n_at_dose())

treatedpct$tpt <- treated$tpt / sum(treated$tpt)

# coerce into selection table

selection_df$tpt <- data.frame(selection$tpt)

selection_tab$tpt <- rbind(t(selection_df$tpt), c(NA,true_dlt_ss))

rownames(selection_tab$tpt) <- c("Proportion of Simulations Dose Selected by Model", "True Toxicity Probabilities")

# coerce into treatment table

treatedpct_df$tpt <- data.frame(treatedpct$tpt)

treatment_tab$tpt <- rbind(t(treatedpct_df$tpt),true_dlt_ss)

rownames(treatment_tab$tpt) <- c("Proportion of Patients Treated by Model", "True Toxicity Probabilities")

# i) accuracy

dist_accuracy$tpt <- recommended_dose(sims$tpt)
mean_accuracy$tpt <- length(subset(dist_accuracy$tpt,dist_accuracy$tpt == best_dose_level)) / n_sims
#hist(dist_accuracy$tpt,breaks=10)

# ii) risk of overdosing

dist_overdose$tpt <- num_tox(sims$tpt)
mean_overdose$tpt <- mean(dist_overdose$tpt)
#hist(dist_overdose$tpt,breaks=10)

# iii) trial length

dist_length$tpt <- sims$tpt %>% escalation::trial_duration()
mean_length$tpt <- mean(dist_length$tpt)
#hist(dist_length$tpt,breaks=10)

output <- list(
    selection_df = selection_df$tpt, # % Times dose was selected as MTD
    selection_tab = selection_tab$tpt, # % Times dose was selected as MTD
    treatedpct_df = treatedpct_df$tpt, # % Treated at each dose 
    treatment_tab = treatment_tab$tpt, # % Treated at each dose
    dist_accuracy = dist_accuracy$tpt, # Distribution of accuracy
    mean_accuracy = mean_accuracy$tpt, # Mean accuracy
    dist_overdose = dist_overdose$tpt, # Distribution of overdose
    mean_overdose = mean_overdose$tpt, # Mean overdose
    dist_length = dist_length$tpt, # Distribution of trial length
    mean_length = mean_length$tpt # Mean trial length
  )
  
  return(output)
}


