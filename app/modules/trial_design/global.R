library(shiny)
library(DT)
library(ggplot2)
library(here)
library(magrittr)
library(escalation)

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

sim_tpt <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, skip_deesc, current_seed) {

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
  tpt_allow_deesc <- skip_deesc
  
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

selection_df$tpt <- data.frame(selection$tpt*100) # Converting to %

selection_tab$tpt <- rbind(t(selection_df$tpt), c(NA,true_dlt_ss))

rownames(selection_tab$tpt) <- c("% of Simulations that Chose Dose as MTD", "True Toxicity Probabilities")

# coerce into treatment table

treatedpct_df$tpt <- data.frame(treatedpct$tpt*100) # Converting to %

treatment_tab$tpt <- rbind(t(treatedpct_df$tpt),true_dlt_ss)

rownames(treatment_tab$tpt) <- c("% of Patients Treated at Dose", "True Toxicity Probabilities")

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
    selection_tab = selection_tab$tpt, # % Times dose was selected as MTD
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

sim_crm <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, skeleton, prior_var,
                    skip_esc, skip_deesc, stop_tox_x, stop_tox_y, stop_n_mtd) {

  # Failsafe for when n_doses does not match the length of skeleton
  if (length(skeleton) != n_doses) {
    stop("The length of the skeleton must match the number of doses specified.")
  }

  # lists

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

# variables needed (UI)

#n_doses <- 5
#ttl <- 0.55
#max_n <- 86
#start_dose <- 3
#current_seed <- 12345
#true_dlt_ss <- c(0.05,0.15,1/3,0.5,0.8) # not dummy

best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
best_dose_level <- match(best_dose,true_dlt_ss)

# model crm

#n_sims$crm <- 200

#skeleton <- c(0.108321683015674,0.255548628279939,0.425089891767129,0.576775912195444,0.817103320499882)
#prior_var <- 0.01

#skip_esc <- FALSE
#skip_deesc <- FALSE
#stop_tox_x <- 0.11 
#stop_tox_y <- 0.06
#stop_n_mtd <- 45

model$crm <- escalation::get_dfcrm(skeleton = skeleton, target = ttl, scale = sqrt(prior_var)) %>% 
  dont_skip_doses(when_escalating = 1-skip_esc, when_deescalating = 1-skip_deesc) %>% 
  stop_when_too_toxic(dose = 1, stop_tox_x + ttl, confidence = stop_tox_y) %>%
  stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>%
  stop_at_n(n=max_n) 

########################################################################################################################################################


## crm sims

# run sims  

sims$crm <- model$crm %>% 
  escalation::simulate_trials(next_dose = start_dose, n_sims, true_dlt_ss, NULL)

# find selection probs  

selection$crm <- sims$crm %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated$crm <- colSums(sims$crm %>% 
  escalation::n_at_dose())

treatedpct$crm <- treated$crm / sum(treated$crm)

# coerce into table

selection_df$crm <- data.frame(selection$crm*100) # Converting to %
treatedpct_df$crm <- data.frame(treatedpct$crm*100) # Converting to %
selection_tab$crm <- rbind(t(selection_df$crm), c(NA,true_dlt_ss))

rownames(selection_tab$crm) <- c("% of Simulations that Chose Dose as MTD", "True Toxicity Probabilities")

# coerce into treatment table

treatedpct_df$crm <- data.frame(treatedpct$crm*100) # Converting to %

treatment_tab$crm <- rbind(t(treatedpct_df$crm),true_dlt_ss)

rownames(treatment_tab$crm) <- c("% of Patients Treated at Dose", "True Toxicity Probabilities")

# i) accuracy

dist_accuracy$crm <- recommended_dose(sims$crm)
mean_accuracy$crm <- length(subset(dist_accuracy$crm,dist_accuracy$crm == best_dose_level)) / n_sims
#hist(dist_accuracy$crm,breaks=10)

# ii) risk of overdosing

dist_overdose$crm <- num_tox(sims$crm)
mean_overdose$crm <- mean(dist_overdose$crm)
#hist(dist_overdose$crm,breaks=10)

# iii) trial length

dist_length$crm <- sims$crm %>% escalation::trial_duration()
mean_length$crm <- mean(dist_length$crm)
#hist(dist_length$crm,breaks=10)

################################################################################################################################################################

# current outputs:
output <- list(
    selection_tab = selection_tab$crm, # % Times dose was selected as MTD
    treatment_tab = treatment_tab$crm, # % Treated at each dose
    dist_accuracy = dist_accuracy$crm, # Distribution of accuracy
    mean_accuracy = mean_accuracy$crm, # Mean accuracy
    dist_overdose = dist_overdose$crm, # Distribution of overdose
    mean_overdose = mean_overdose$crm, # Mean overdose
    dist_length = dist_length$crm, # Distribution of trial length
    mean_length = mean_length$crm # Mean trial length
  )
  
  return(output)

### PLOTS - To return to later.
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
}

