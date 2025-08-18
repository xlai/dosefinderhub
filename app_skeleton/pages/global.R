library(shiny)
library(DT)
library(ggplot2)
library(here)
library(magrittr)
library(escalation)
library(rlang)
library(ggpattern)
library(BOIN)

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

###################################### BOIN Boundaries ######################################
boin_probs <- function(ttl, lambda, type) { # Reverse engineering the BOIN probabilities from lambda and ttl
  if (type == 1) {
    f <- function(x) lambda - log((1-x)/(1-ttl))/log((ttl*(1-x))/(x*(1-ttl)))
  } else if (type == 2) {
    f <- function(x) lambda - log((1-ttl)/(1-x))/log((x*(1-ttl))/(ttl*(1-x)))
  }

  result <- uniroot(f, c(0,0.999))
  return(result$root)
}

boin_boundaries <- function(target, ncohort, cohortsize, n.earlystop, p.saf, p.tox) { # Generating boundaries
 if (0 < p.saf && p.saf < 0.95*target && 1.05*target < p.tox && p.tox < 1 && !is.na(p.saf) && !is.na(p.tox)) {
  lambda <- BOIN::get.boundary(target = target, ncohort = ncohort, 
                               cohortsize = cohortsize, n.earlystop = n.earlystop, 
                               p.saf = p.saf, p.tox = p.tox, 0.95, FALSE, 0.05)
  
  lambda_e <- lambda$lambda_e
  lambda_d <- lambda$lambda_d

  text_e <-  paste("lambda_e = ", lambda_e) 
  text_d <-  paste("lambda_d = ", lambda_d)
 } else {
  text_e <- "Probabilities must be less than and greater than the ttl respectively, and cannot be too close to the ttl."
  text_d <- NULL

  lambda_e <- NULL
  lambda_d <- NULL
 }
 return(list(
    lambda_e = lambda_e,
    lambda_d = lambda_d,
    text_e = text_e,
    text_d = text_d
  ))
}

############################################ Simulation Code ############################################

### This is a rewrite of the basesim.r file as a set of functions that can be used reactively to simulate data.

########################### 3+3 Simulation Function ###########################
sim_tpt <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, current_seed) {

# Best dose and level
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
  
  # Create the model
  model$tpt <- escalation::get_three_plus_three(num_doses = n_doses, 
    allow_deescalate = FALSE,
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

# ii) risk of overdosing

dist_overdose$tpt <- num_tox(sims$tpt)
mean_overdose$tpt <- mean(dist_overdose$tpt)

# iii) trial length

dist_length$tpt <- sims$tpt %>% escalation::trial_duration()
mean_length$tpt <- mean(dist_length$tpt)

output <- list(
    selection_tab = selection_tab$tpt, # % Times dose was selected as MTD
    treatment_tab = treatment_tab$tpt, # % Treated at each dose
    mean_accuracy = mean_accuracy$tpt, # Mean accuracy
    dist_overdose = dist_overdose$tpt, # Distribution of overdose
    mean_overdose = mean_overdose$tpt, # Mean overdose
    dist_length = dist_length$tpt, # Distribution of trial length
    mean_length = mean_length$tpt # Mean trial length
  )
  
  return(output)
}

########################### CRM Simulation Function ###########################

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

# Best dose and level

best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
best_dose_level <- match(best_dose,true_dlt_ss)

# Model itself

model$crm <- escalation::get_dfcrm(skeleton = skeleton, target = ttl, scale = sqrt(prior_var)) %>% 
  dont_skip_doses(when_escalating = 1-skip_esc, when_deescalating = 1-skip_deesc) %>% 
  stop_when_too_toxic(dose = 1, stop_tox_x + ttl, confidence = stop_tox_y) %>%
  stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>%
  stop_at_n(n=max_n) 

########################################################################################################################################################
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

# i) accuracy - needs to be revisited

dist_accuracy$crm <- recommended_dose(sims$crm)
mean_accuracy$crm <- length(subset(dist_accuracy$crm,dist_accuracy$crm == best_dose_level)) / n_sims

# ii) risk of overdosing

dist_overdose$crm <- num_tox(sims$crm)
mean_overdose$crm <- mean(dist_overdose$crm)

# iii) trial length

dist_length$crm <- sims$crm %>% escalation::trial_duration()
mean_length$crm <- mean(dist_length$crm)

################################################################################################################################################################

output <- list(
    selection_tab = selection_tab$crm, # % Times dose was selected as MTD
    treatment_tab = treatment_tab$crm, # % Treated at each dose
    mean_accuracy = mean_accuracy$crm, # Mean accuracy
    dist_overdose = dist_overdose$crm, # Distribution of overdose
    mean_overdose = mean_overdose$crm, # Mean overdose
    dist_length = dist_length$crm, # Distribution of trial length
    mean_length = mean_length$crm # Mean trial length
  )
  
  return(output)
}

########################### BOIN Simulation Function ###########################
sim_boin <- function(n_doses, ttl, max_n, start_dose, n_sims, true_dlt_ss, cohort_size, stop_n_mtd,
                    p_tox, p_saf, use_stopping_rule, n_cohorts) {



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

# coerce into treatment table

treatedpct_df <- data.frame(treatedpct*100)

treatment_tab <- rbind(t(treatedpct_df), true_dlt_ss)

rownames(treatment_tab) <- c("% of Patients Treated at Dose", "True Toxicity Probabilities")

# i) accuracy - needs to be revisited

dist_accuracy <- recommended_dose(sims)
mean_accuracy <- length(subset(dist_accuracy, dist_accuracy == best_dose_level)) / n_sims

# ii) risk of overdosing

dist_overdose <- num_tox(sims)
mean_overdose <- mean(dist_overdose)

# iii) trial length

dist_length <- escalation::trial_duration(sims)
mean_length <- mean(dist_length)

output <- list(
    selection_tab = selection_tab, # % Times dose was selected as MTD
    treatment_tab = treatment_tab, # % Treated at each dose
    mean_accuracy = mean_accuracy, # Mean accuracy
    dist_overdose = dist_overdose, # Distribution of overdose
    mean_overdose = mean_overdose, # Mean overdose
    dist_length = dist_length, # Distribution of trial length
    mean_length = mean_length # Mean trial length
  )
  return(output)
                    }

########################### Function to Plot Simulation Bar Charts ###########################
plot_bar <- function(data, category, value, title, y_title, col, model_picked, models, scenarios) {
  valid_data <- Filter(Negate(is.null), data)
  
  model <- c("3+3", "CRM", "BOIN")
  updated_model <- model[!sapply(models, identical, FALSE)]

  scenario <- c("Scenario 1", "Scenario 2", "Scenario 3") 
  updated_scenarios <- scenario[!sapply(scenarios, identical, FALSE)]

  if (length(valid_data) == 0) {
    return(NULL)
  } else {
  
  named_data <- lapply(seq_along(valid_data), function(i) {
    df <- valid_data[[i]]
    df$Design <- paste0(updated_model[i])

    updated_scenarios_2 <- vector()

    y <- 1
    z <- 1
    while (y < 4) {
    if (scenarios[[y]] == FALSE) {
      updated_scenarios_2[y] <- "Not selected"
    } else {
      updated_scenarios_2[y] <- paste0(updated_scenarios[z])
      z <- z+1
    }
    y <- y + 1
    }

    df$Scenario <- paste0(updated_scenarios_2[i])
    return(df)
  })

  combined_data <- do.call(rbind, named_data)
  
  if (model_picked == 1) {
    fill_col <- combined_data$Design
    fill_title <- "Design"
  } else {
    fill_col <- combined_data$Scenario
    fill_title <- "Scenario"
  }

 plot <- ggplot(combined_data, aes(x = {{category}}, y = {{value}}, fill = fill_col, pattern = highlight)) +
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge(),
    pattern_density = 0.2,
    pattern_spacing = 0.05,
    pattern_fill = "white",
    color = "black"
  ) +
  scale_pattern_manual(values = c("MTD" = "crosshatch", "Other" = "none")) +
  labs(
    title = title,
    x = "Dose Level",
    y = y_title,
    pattern = "Is the Dose Level the True MTD?",
    fill = fill_title
  ) +
  theme_minimal()

    return(plot)
}} # end of plot_bar function


########################### Function to Format Data for Plot Simulation Histograms ###########################
plot_dist <- function(data, category, median_vector, title, x_title, col, model_picked, models, scenarios) {
  
  valid_data <- Filter(Negate(is.null), data)

  model <- c("3+3", "CRM", "BOIN")
  updated_model <- model[!sapply(models, identical, FALSE)]

  scenario <- c("Scenario 1", "Scenario 2", "Scenario 3") # For later when "by scenario" is implemented
  updated_scenarios <- scenario[!sapply(scenarios, identical, FALSE)]

  Median <- median_vector

  median <- data.frame(Median)

  if (length(valid_data) == 0) {
    return(NULL)
  } else {
  
  named_data <- lapply(seq_along(valid_data), function(i) {
    df <- valid_data[[i]]
    df$Design <- paste0(updated_model[i])

    updated_scenarios_2 <- vector()

    y <- 1
    z <- 1
    while (y < 4) {
    if (scenarios[[y]] == FALSE) {
      updated_scenarios_2[y] <- "Not selected"
    } else {
      updated_scenarios_2[y] <- paste0(updated_scenarios[z])
      z <- z+1
    }
    y <- y + 1
    }

    df$Scenario <- paste0(updated_scenarios_2[i])
    return(df)
  })
  
  combined_data <- do.call(rbind, named_data)

   if (model_picked == 1) {
    median$model <- updated_model

    fill_col <- combined_data$Design
    colour <- median$model
    fill_title <- "Design"
  } else {
     median$scenarios <- updated_scenarios

    fill_col <- combined_data$Scenario
    colour <- median$scenarios
    fill_title <- "Scenario"
  }

  plot <- ggplot(combined_data, aes(x = {{category}}, fill = fill_col)) +
     geom_density(alpha=0.3) +
     geom_vline(data = median, aes(xintercept = Median, color = colour), linetype = "dashed") + 
     labs(title = title, x = x_title, y = "Density", color = "Median Values", fill = fill_title) +
    theme_minimal()

    return(plot)
}}

########################### Functions to Format Data for Plotting###########################
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
    #mean_accuracy = as.numeric(sim$mean_accuracy),
    overdose = data.frame(overdose = sim$dist_overdose),
    #mean_overdose = as.numeric(sim$mean_overdose),
    length = data.frame(length = sim$dist_length)
    #mean_length = as.numeric(sim$mean_length)
  )
}
 
 plot_by_scenario <- function(list1) { # Might be worth rewriting this to look more like mean_for_scen in future
  list2 <- vector("list", length = 5)

  for (j in 1:5) {
   list2[[j]] <- vector("list", length = 3)
  }
 
  for (i in 1:3) {
    for (j in 1:4) {
    if (is.null(list1[[i]][[j]])) {
      list2[[j]][[i]] <- list(NULL)
    } else {
    list2[[j]][[i]] <- list1[[i]][[j]]
  } } }

  return(list2)
}

  median_for_scen <- function(median) {
  median_scen <- lapply(seq_along(median[[1]]), function(j) sapply(median, `[`, j))
  return(median_scen)
  }

  ############### Functions to Output Example Scenarios ################

  # For now, I will take a fixed delta (0.05). This can be refined later.
  example_scenarios <- function (ttl, mtd, n_doses) {
    p <- rep(NA, n_doses)
    p[mtd] <- ttl

    if (ttl < 0.5) {
      lower <- 0.05
      upper <- 2*(ttl-0.05)
    } else if (ttl > 0.5) {
      lower <- 2*ttl - 0.95
      upper <- 0.95
    } else {
      lower <- 0.2
      upper <- 0.8
    }
   

    upper_doses <- n_doses - mtd + 1
    lower_doses <- mtd 

    if (upper_doses > 0) {
      p[mtd:n_doses] <- seq(from = ttl, to = upper, length.out = upper_doses)
    }
    if (lower_doses > 0) {
      p[1:mtd] <- seq(from = lower, to = ttl, length.out = lower_doses)
    }

    q <- round(p, 2)
    return(q)
  }
 