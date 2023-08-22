# required packages

library(magrittr)
library(escalation)
library(ggplot2)

# lists

n_sims <- list()

model <- list()
sims <- list()

selection <- list()
selection_df <- list()

treated <- list()
treatedpct <- list()
treatedpct_df <- list()

dist_accuracy <- list()
mean_accuracy <- list()

dist_overdose <- list()
mean_overdose <- list()

dist_length <- list()
mean_length <- list()

# variables needed (UI)

n_doses <- 5
ttl <- 1/3
max_n <- 30
start_dose <- 1
current_seed <- 12345
tpt_allow_deesc <- TRUE
n_sims$tpt <- 100
true_dlt_ss <- c(0.05,0.15,1/3,0.5,0.8) 

best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
best_dose_level <- match(best_dose,true_dlt_ss)

# model

model$tpt <- escalation::get_three_plus_three(num_doses = n_doses, 
  allow_deescalate = tpt_allow_deesc, 
  set.seed(current_seed)) %>% 
  stop_at_n(n=max_n)

# run sims  

sims$tpt <- model$tpt %>% 
  escalation::simulate_trials(next_dose = start_dose, n_sims$tpt, true_dlt_ss, NULL)

# find selection probs  

selection$tpt <- sims$tpt %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated$tpt <- colSums(sims$tpt %>% 
  escalation::n_at_dose())

treatedpct$tpt <- treated$tpt / sum(treated$tpt)

# coerce into selection table

selection_df$tpt <- data.frame(selection$tpt)

selection_tab <- rbind(t(selection_df$tpt), c(NA,true_dlt_ss))

rownames(selection_tab) <- c("Dose Selected by Model", "True Toxicity Probabilities")

# coerce into treatment table

treated_df$tpt <- data.frame(treated$tpt)

treatment_tab <- rbind(t(treated_df$tpt),true_dlt_ss)

rownames(treatment_tab) <- c("Patients Treated by Model", "True Toxicity Probabilities")

# spider diagram?

# arms of spider:
# i) Accuracy. Defined as 'correct' MTD selection %
# ii) Risk of overdosing. sum of (tox prob * % patients treated * # patients)
# iii) Length of trial. trial_duration from escalation.
# iv) Risk of returning no dose?
# v) clinical benefit / underdosing?
# return distribution rather than single point?

# i) accuracy

dist_accuracy$tpt <- recommended_dose(sims$tpt)
mean_accuracy$tpt <- length(subset(dist_accuracy$tpt,dist_accuracy$tpt == best_dose_level)) / n_sims$tpt
hist(dist_accuracy$tpt,breaks=10)

# ii) risk of overdosing

dist_overdose$tpt <- num_tox(sims$tpt)
mean_overdose$tpt <- mean(dist_overdose$tpt)
#hist(dist_overdose$tpt,breaks=10)

# iii) trial length

dist_length$tpt <- sims$tpt %>% escalation::trial_duration()
mean_length$tpt <- mean(dist_length$tpt)
#hist(dist_length$tpt,breaks=10)




