library(shiny)
library(DT)
library(ggplot2)
library(here)
library(magrittr)
library(escalation)
library(rlang)

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
ranking <- c("crm", "tpt", "boin") #REMOVE/COMMENT OUT; JUST TO TEST TAB DYNAMICS
prettify_ranking <- function(ranking_argument) {
  pretty_ranking <- ranking_argument
  pretty_ranking[pretty_ranking == "crm"] <- "CRM"
  pretty_ranking[pretty_ranking == "tpt"] <- "3+3"
  pretty_ranking[pretty_ranking == "boin"] <- "BOIN"
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

######################## Validation Functions ########################

# Reusable validation function
validate_numeric_input <- function(value, min_val = NULL, max_val = NULL, integer_only = FALSE) {

  if (!is.null(value) && value != "" && !is.na(value)) {
    if (!is.numeric(value)) {
      return("Please enter a valid number")
    }
    
    # Check if integer is required
    if (integer_only) {
      if (value != as.integer(value)) {
        return("Please enter a whole number (integer)")
      }
    }
    
    # Check for positive values when min_val is set to greater than 0
    if (!is.null(min_val) && value < min_val) {
      if (min_val > 0) {
        return(paste("Value must be positive (greater than", min_val - 1, ")"))
      } else {
        return(paste("Value must be at least", min_val))
      }
    }
    
    if (!is.null(max_val) && value > max_val) {
      return(paste("Value must be at most", max_val))
    }
  }
  
  return(NULL) # No error
}

# Reusable UI component for numeric input with validation using bslib


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
}

sim_boin <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, cohort_size, stop_n_mtd,
                    p_tox, p_saf, use_stopping_rule, n_cohorts) {

# variables needed (UI)

#n_doses <- 5
#ttl <- 1/3
#max_n <- 24
#start_dose <- 1
#current_seed <- 12345
#n_sims$boin <- 100
#true_dlt_ss <- c(0.05, 0.15, 1/3, 0.5, 0.8)
#cohort_size <- 3
#stop_n_mtd <- 15

## NOT YET COLLECTED IN Q_DATABASE
#p_tox <- 0.6
#p_saf <- 0.1
#use_stopping_rule <- TRUE
#n_cohorts <- 10

best_dose <- max(true_dlt_ss[true_dlt_ss <= ttl])
best_dose_level <- match(best_dose, true_dlt_ss)

model <- get_boin(num_doses = n_doses, target = ttl, use_stopping_rule = use_stopping_rule, p.saf = p_saf, p.tox = p_tox,
                       cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05) %>%
  escalation::stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>% 
  escalation::stop_at_n(n = max_n)
  # Might be worth looking at https://www.rdocumentation.org/packages/BOIN/versions/2.7.2/topics/get.boundary to refine this.

# run sims
patient_arrivals_func <- function(current_data) cohorts_of_n(n = cohort_size)
sims <- model %>%
  escalation::simulate_trials(
    next_dose = start_dose,
    num_sims = n_sims,
    true_prob_tox = true_dlt_ss,
    true_prob_eff = NULL,
    sample_patient_arrivals = patient_arrivals_func
  )

# find selection probs

selection <- sims %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated <- colSums(sims %>% 
  escalation::n_at_dose())

treatedpct <- treated / sum(treated)
# coerce into selection table

selection_df <- data.frame(selection*100)


selection_tab <- rbind(t(selection_df), c(NA, true_dlt_ss))

rownames(selection_tab) <- c("% of Simulations that Chose Dose as MTD", "True Toxicity Probabilities")
#selection_tab
# coerce into treatment table

treatedpct_df <- data.frame(treatedpct*100)

treatment_tab <- rbind(t(treatedpct_df), true_dlt_ss)

rownames(treatment_tab) <- c("% of Patients Treated at Dose", "True Toxicity Probabilities")
#treatment_tab

# i) accuracy

dist_accuracy <- recommended_dose(sims)
mean_accuracy <- length(subset(dist_accuracy, dist_accuracy == best_dose_level)) / n_sims
#hist(dist_accuracy$boin,breaks=10)

# ii) risk of overdosing

dist_overdose <- num_tox(sims)
mean_overdose <- mean(dist_overdose)
#hist(dist_overdose,breaks=10)

# iii) trial length

dist_length <- escalation::trial_duration(sims)
mean_length <- mean(dist_length)
#hist(dist_length,breaks=10)
output <- list(
    selection_tab = selection_tab, # % Times dose was selected as MTD
    treatment_tab = treatment_tab, # % Treated at each dose
    dist_accuracy = dist_accuracy, # Distribution of accuracy
    mean_accuracy = mean_accuracy, # Mean accuracy
    dist_overdose = dist_overdose, # Distribution of overdose
    mean_overdose = mean_overdose, # Mean overdose
    dist_length = dist_length, # Distribution of trial length
    mean_length = mean_length # Mean trial length
  )
  return(output)
                    }

sim_plots <- function(sim, ttl, col1, col2, col3) {
  true_dlts <- sim$treatment_tab[2,] # True DLTs from treatment table
  best_dose <- max(true_dlts[true_dlts <= ttl])
  mtd <- match(best_dose, true_dlts)

  selection <- as.vector(sim$selection_tab[1,])
  treatment <- as.vector(sim$treatment_tab[1,])
  Dose_Level <- seq(1, length(treatment))
  no_dose <- "No Dose"
  Dose <- c(no_dose, Dose_Level)

 # % selected as MTD and % Treaed at each dose as a bar plot
  data <- as.data.frame(selection)
  data$Dose <- factor(Dose, levels = c(no_dose, as.character(Dose_Level)))
  data$selection <- selection
  data_treatment <- as.data.frame(cbind(Dose_Level, treatment))

  # Highlighting the MTD
  data$highlight <- ifelse(data$Dose == mtd, "MTD", "Other")
  data_treatment$highlight <- ifelse(Dose_Level == mtd, "MTD", "Other")

  plot_mtd <- ggplot(data, aes(x=Dose, y=selection, color = highlight)) +
  geom_bar(stat="identity", fill = col1) +
  scale_color_manual(values=c("MTD" = col2, "Other" = col1)) +
  theme_minimal() +
  ggtitle("Percentage of Trials Selecting Dose as MTD") +
  xlab("Dose Level") +
  ylab("Percentage of Trials Selecting Dose as MTD") +
  labs(color = "Is the Dose the True MTD?")

  plot_treated <- ggplot(data_treatment, aes(x=Dose_Level, y=treatment, color = highlight)) +
  geom_bar(stat="identity", fill = col1) +
  scale_color_manual(values=c("MTD" = col2, "Other" = col1)) +
  theme_minimal() +
  ggtitle("Percentage of Patients Treated at Each Dose") +
  xlab("Dose Level") +
  ylab("Percentage of Patients Treated") +
  labs(color = "Is the Dose the True MTD?")

  # Distrubtion of accuracy, overdose, and trial length as histograms
  plot_accuracy <- ggplot(data.frame(sim$dist_accuracy), aes(x=sim$dist_accuracy)) +
    geom_histogram(binwidth = 1, fill = col1, color = "black") +
    geom_vline(aes(xintercept = sim$mean_accuracy), color = col3, linetype = "dashed") +
    labs(title = "Distribution of Accuracy", x = "Dose Level", y = "Frequency") +
    theme_minimal() 

  plot_overdose <- ggplot(data.frame(sim$dist_overdose), aes(x=sim$dist_overdose)) +
    geom_histogram(binwidth = 1, fill = col1, color = "black") +
    geom_vline(aes(xintercept = sim$mean_overdose), color = col3, linetype = "dashed") +
    labs(title = "Distribution of Overdose", x = "Number of Overdoses", y = "Frequency") +
    theme_minimal()

  plot_length <- ggplot(data.frame(sim$dist_length), aes(x=sim$dist_length)) +
    geom_histogram(binwidth = 1, fill = col1, color = "black") +
    geom_vline(aes(xintercept = sim$mean_length), color = col3, linetype = "dashed") +
    labs(title = "Distribution of Trial Length", x = "Trial Length", y = "Frequency") +
    theme_minimal()

  # Return a list of plots
  output <- list(
    plot_mtd = plot_mtd,
    plot_treated = plot_treated,
    plot_accuracy = plot_accuracy,
    plot_overdose = plot_overdose,
    plot_length = plot_length
  )
  return(output)
}

data_for_plotting <- function(sim, ttl) {
  true_dlts <- sim$treatment_tab[2,] # True DLTs from treatment table
  best_dose <- max(true_dlts[true_dlts <= ttl])
  mtd <- match(best_dose, true_dlts)

  selection <- as.vector(sim$selection_tab[1,])
  treatment <- as.vector(sim$treatment_tab[1,])
  Dose_Level <- seq(1, length(treatment))
  no_dose <- "No Dose"
  Dose <- c(no_dose, Dose_Level)

 # % selected as MTD and % Treaed at each dose as a bar plot
  data_selection <- as.data.frame(selection)
  data_selection$Dose <- factor(Dose, levels = c(no_dose, as.character(Dose_Level)))
  data_selection$selection <- selection
  data_treatment <- as.data.frame(cbind(Dose_Level, treatment))

  # Highlighting the MTD
  data_selection$highlight <- ifelse(data_selection$Dose == mtd, "MTD", "Other")
  data_treatment$highlight <- ifelse(Dose_Level == mtd, "MTD", "Other")
  
  output <- list(
    data_selection = data_selection,
    data_treatment = data_treatment,
    accuracy = data.frame(accuracy = sim$dist_accuracy),
    #mean_accuracy = as.numeric(sim$mean_accuracy),
    overdose = data.frame(overdose = sim$dist_overdose),
    #mean_overdose = as.numeric(sim$mean_overdose),
    length = data.frame(length = sim$dist_length)
    #mean_length = as.numeric(sim$mean_length)
  )
}

plot_bar <- function(data, category, value, title, y_title, col) {
  valid_data <- Filter(Negate(is.null), data)

  model <- c("3+3", "CRM", "BOIN")
  updated_model <- model

  # scenario <- c("Scenario 1", "Scenario 2", "Scenario 3") # For later when "by scenario" is implemented

  # Isolating the models we have:
  for (j in 1:3) {
    if (is.null(data[[j]])) {
        updated_model <- updated_model[-j] 
    }
  }

  if (length(valid_data) == 0) {
    return(NULL)
  } else {
  
  named_data <- lapply(seq_along(valid_data), function(i) {
    df <- valid_data[[i]]
    df$Model <- paste0(updated_model[i])
    return(df)

  })

  combined_data <- do.call(rbind, named_data)

  plot <- ggplot(combined_data, aes(x = {{category}}, y = {{value}}, fill = Model, color = highlight)) +
     geom_bar(stat = "identity", position = position_dodge()) +
     scale_color_manual(values=c("MTD" = col, "Other" = NULL)) +
     labs(title = title, x = "Dose Level", y = y_title, color = "Is the Dose the True MTD?") +
    theme_minimal()

  #plot <- ggplot(combined_data, aes(x = category, y = value, color = highlight)) +
   # geom_bar(stat = "identity", position = position_dodge(), fill = col1) +
    #scale_color_manual(values=c("MTD" = col2, "Other" = col1)) +
    #theme_minimal() +
    #ggtitle(Title) +
    #xlab("Dose Level") +
    #ylab(y_title) +
    #labs(color = "Is the Dose the True MTD?")

    return(plot)
}}

plot_dist <- function(data, category, mean, title, x_title, col) {
  valid_data <- Filter(Negate(is.null), data)

  model <- c("3+3", "CRM", "BOIN")
  updated_model <- model

  # scenario <- c("Scenario 1", "Scenario 2", "Scenario 3") # For later when "by scenario" is implemented

  # Isolating the models we have:
  for (j in 1:3) {
    if (is.null(data[[j]])) {
        updated_model <- updated_model[-j] 
    }
  }

  if (length(valid_data) == 0) {
    return(NULL)
  } else {
  
  named_data <- lapply(seq_along(valid_data), function(i) {
    df <- valid_data[[i]]
    df$Model <- paste0(updated_model[i])
    return(df)

  })

  combined_data <- do.call(rbind, named_data)

  plot <- ggplot(combined_data, aes(x = {{category}}, fill = Model)) +
     geom_histogram(binwidth = 1, position = position_dodge(), color = "black") +
     geom_vline(aes(xintercept = mean), color = col, linetype = "dashed") +
     labs(title = title, x = x_title, y = "Frequency") +
    theme_minimal()

    return(plot)
}}

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


