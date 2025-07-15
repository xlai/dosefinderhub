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
max_n <- 24
start_dose <- 1
current_seed <- 12345
n_sims$crm <- 100
true_dlt_ss <- c(0.05,0.15,1/3,0.5,0.8) 
skeleton <- c(0.08,0.20,0.35,0.6,0.9)
prior_var <- 0.8

skip_esc <- FALSE
skip_deesc <- TRUE
stop_tox_x <- 0.1 
stop_tox_y <- 0.7
stop_n_mtd <- 25

best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
best_dose_level <- match(best_dose,true_dlt_ss)

# model

model$crm <- escalation::get_dfcrm(skeleton = skeleton, target = ttl, scale = sqrt(prior_var)) %>% 
  dont_skip_doses(when_escalating = 1-skip_esc, when_deescalating = 1-skip_deesc) %>% 
  stop_when_too_toxic(dose = 1, stop_tox_x + ttl, confidence = stop_tox_y) %>%
  stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>%
  stop_at_n(n=max_n) 

# run sims  

sims$crm <- model$crm %>% 
  escalation::simulate_trials(next_dose = start_dose, n_sims$crm, true_dlt_ss, NULL)

# find selection probs  

selection$crm <- sims$crm %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated$crm <- colSums(sims$crm %>% 
  escalation::n_at_dose())

treatedpct$crm <- treated$crm / sum(treated$crm)

# coerce into selection table

selection_df$crm <- data.frame(selection$crm)

selection_tab <- rbind(t(selection_df$crm), c(NA,true_dlt_ss))

rownames(selection_tab) <- c("Dose Selected by Model", "True Toxicity Probabilities")

# coerce into treatment table

treated_df$crm <- data.frame(treated$crm)

treatment_tab <- rbind(t(treated_df$crm),true_dlt_ss)

rownames(treatment_tab) <- c("Patients Treated by Model", "True Toxicity Probabilities")

# spider diagram?

# arms of spider:
# i) Accuracy. Defined as 'correct' MTD selection %
# ii) Risk of overdosing. sum of (tox prob * % patients treated * # patients)
# iii) Length of trial. trial_duration from escalation.
# iv) Risk of returning no dose?
# v) clinical benefit / underdosing?

# i) accuracy

dist_accuracy$crm <- recommended_dose(sims$crm)
mean_accuracy$crm <- length(subset(dist_accuracy$crm,dist_accuracy$crm == best_dose_level)) / n_sims$crm
#hist(dist_accuracy$crm,breaks=10)

# ii) risk of overdosing

dist_overdose$crm <- num_tox(sims$crm)
mean_overdose$crm <- mean(dist_overdose$crm)
#hist(dist_overdose$crm,breaks=10)

# iii) trial length

dist_length$crm <- sims$crm %>% escalation::trial_duration()
mean_length$crm <- mean(dist_length$crm)
#hist(dist_length$crm,breaks=10)

