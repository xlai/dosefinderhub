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
n_sims$boin <- 100
true_dlt_ss <- c(0.05, 0.15, 1/3, 0.5, 0.8)
cohort_size <- 3
stop_n_mtd <- 15

## NOT YET COLLECTED IN Q_DATABASE
p_tox <- 0.6
p_saf <- 0.1
use_stopping_rule <- TRUE
n_cohorts <- 10

best_dose <- max(true_dlt_ss[true_dlt_ss <= ttl])
best_dose_level <- match(best_dose, true_dlt_ss)

model$boin <- get_boin(num_doses = n_doses, target = ttl, use_stopping_rule = use_stopping_rule, p.saf = p_saf, p.tox = p_tox,
                       cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05) %>%
  escalation::stop_when_n_at_dose(n = 14, dose = "recommended") %>%
  escalation::stop_at_n(n = max_n)

# run sims
patient_arrivals_func <- function(current_data) cohorts_of_n(n = cohort_size)
sims$boin <- model$boin %>%
  escalation::simulate_trials(
    next_dose = start_dose,
    num_sims = n_sims$boin,
    true_prob_tox = true_dlt_ss,
    true_prob_eff = NULL,
    sample_patient_arrivals = patient_arrivals_func
  )

# find selection probs

selection$boin <- sims$boin %>% 
  escalation::prob_recommend()

# find no. treated at dose

treated$boin <- colSums(sims$boin %>% 
  escalation::n_at_dose())

treatedpct$boin <- treated$boin / sum(treated$boin)
treatedpct$boin
# coerce into selection table

selection_df$boin <- data.frame(selection$boin)


selection_tab <- rbind(t(selection_df$boin), c(NA, true_dlt_ss))

rownames(selection_tab) <- c("Dose Selected by Model", "True Toxicity Probabilities")
selection_tab
# coerce into treatment table

treatedpct_df$boin <- data.frame(treatedpct$boin)

treatment_tab <- rbind(t(treatedpct_df$boin), true_dlt_ss)

rownames(treatment_tab) <- c("% Patients Treated by Model", "True Toxicity Probabilities")
treatment_tab
# spider diagram?

# arms of spider:
# i) Accuracy. Defined as 'correct' MTD selection %
# ii) Risk of overdosing. sum of (tox prob * % patients treated * # patients)
# iii) Length of trial. trial_duration from escalation.
# iv) Risk of returning no dose?
# v) clinical benefit / underdosing?

# i) accuracy

dist_accuracy$boin <- recommended_dose(sims$boin)
mean_accuracy$boin <- length(subset(dist_accuracy$boin, dist_accuracy$boin == best_dose_level)) / n_sims$boin
#hist(dist_accuracy$boin,breaks=10)

# ii) risk of overdosing

dist_overdose$boin <- num_tox(sims$boin)
mean_overdose$boin <- mean(dist_overdose$boin)
hist(dist_overdose$boin,breaks=10)

# iii) trial length

dist_length$boin <- sims$boin %>% escalation::trial_duration()
mean_length$boin <- mean(dist_length$boin)
#hist(dist_length$boin,breaks=10)
