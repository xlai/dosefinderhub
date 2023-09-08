# required packages

library(escalation)
library(ggplot2)
`%>%` <- magrittr::`%>%`

# lists

n_sims <- list()

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

n_doses <- 5
ttl <- 0.55
max_n <- 86
start_dose <- 3
current_seed <- 12345
true_dlt_ss <- c(0.05,0.15,1/3,0.5,0.8) # not dummy

best_dose <- max(true_dlt_ss[true_dlt_ss<=ttl])
best_dose_level <- match(best_dose,true_dlt_ss)

# model tpt

n_sims$tpt <- 500

tpt_allow_deesc <- TRUE

model$tpt <- escalation::get_three_plus_three(num_doses = n_doses, 
  allow_deescalate = tpt_allow_deesc, 
  set.seed(current_seed)) %>% 
  stop_at_n(n=max_n)

# model crm

n_sims$crm <- 200

skeleton <- c(0.108321683015674,0.255548628279939,0.425089891767129,0.576775912195444,0.817103320499882)
prior_var <- 0.01

skip_esc <- FALSE
skip_deesc <- FALSE
stop_tox_x <- 0.11 
stop_tox_y <- 0.06
stop_n_mtd <- 45

model$crm <- escalation::get_dfcrm(skeleton = skeleton, target = ttl, scale = sqrt(prior_var)) %>% 
  dont_skip_doses(when_escalating = 1-skip_esc, when_deescalating = 1-skip_deesc) %>% 
  stop_when_too_toxic(dose = 1, stop_tox_x + ttl, confidence = stop_tox_y) %>%
  stop_when_n_at_dose(n = stop_n_mtd, dose = "recommended") %>%
  stop_at_n(n=max_n) 

########################################################################################################################################################

## tpt sims

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

selection_tab$tpt <- rbind(t(selection_df$tpt), c(NA,true_dlt_ss))

rownames(selection_tab$tpt) <- c("Dose Selected by Model", "True Toxicity Probabilities")

# coerce into treatment table

treatedpct_df$tpt <- data.frame(treatedpct$tpt)

treatment_tab$tpt <- rbind(t(treatedpct_df$tpt),true_dlt_ss)

rownames(treatment_tab$tpt) <- c("Patients Treated by Model", "True Toxicity Probabilities")

# i) accuracy

dist_accuracy$tpt <- recommended_dose(sims$tpt)
mean_accuracy$tpt <- length(subset(dist_accuracy$tpt,dist_accuracy$tpt == best_dose_level)) / n_sims$tpt
#hist(dist_accuracy$tpt,breaks=10)

# ii) risk of overdosing

dist_overdose$tpt <- num_tox(sims$tpt)
mean_overdose$tpt <- mean(dist_overdose$tpt)
#hist(dist_overdose$tpt,breaks=10)

# iii) trial length

dist_length$tpt <- sims$tpt %>% escalation::trial_duration()
mean_length$tpt <- mean(dist_length$tpt)
#hist(dist_length$tpt,breaks=10)

#############################################################################################################################################################

## crm sims

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

# coerce into table

selection_df$crm <- data.frame(selection$crm)
treatedpct_df$crm <- data.frame(treatedpct$crm)
selection_tab$crm <- rbind(t(selection_df$crm), c(NA,true_dlt_ss))

rownames(selection_tab$crm) <- c("Dose Selected by Model", "True Toxicity Probabilities")

# coerce into treatment table

treatedpct_df$crm <- data.frame(treatedpct$crm)

treatment_tab$crm <- rbind(t(treatedpct_df$crm),true_dlt_ss)

rownames(treatment_tab$crm) <- c("Patients Treated by Model", "True Toxicity Probabilities")

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

################################################################################################################################################################

# current outputs:
metrics <- list(accuracy = mean_accuracy, overdose = mean_overdose, duration = mean_length)
print(selection_tab)
print(treatment_tab) #combine? was using to create comparative scores, but have moved away from this approach. Using this purely for visual outputs.
print(metrics)

#par(mfrow = c(1,3))
graph_accuracy <- hist(dist_accuracy$tpt,breaks=11) %>% abline(v=mean_accuracy$tpt, col = "red")
graph_overdose <- hist(dist_overdose$tpt,breaks=11) %>% abline(v=mean_overdose$tpt, col = "red")
graph_length <- hist(dist_length$tpt,breaks=11) %>% abline(v=mean_length$tpt, col = "red")

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
