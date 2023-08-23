library(shiny)





#1.0 DEFINING VARIABLES TO BE INPUTTED FROM QUESTIONNAIRE UI

#Most variable names were taken from the master_variables spreadsheet published by Sian and reviewed by Jake, added to main branch on 22Aug23
#Variable names not taken from spreadsheet are signalled below
#Variable values are arbitrary since we have not integrated them from the Questionnaire UI yet; I tried to stick to the defaults specified in the spreadsheet

##user_profile variables:
ranking <- c("3p3", "crm", "other") #Not taken from spreadhseet; first ranking possibility. Commen/uncomment and re-run script
#ranking <- c("crm", "3p3", "other") #Second ranking possibility. Commen/uncomment and re-run script
#ranking <- c("other", "3p3", "crm") #Third ranking possibility. Commen/uncomment and re-run script

##non_spec variables (non design-specific):
n_doses <- 5
start_dose <- 1
cohort_size <- 3
ttl <- 0.2
max_n <- 24
curent_seed <- 1234
#Maybe the other simulation parameters (n_sims, true_dlt_ss) should be here

##spec_3p3 variables (trial-design-specific, particularly 3 plus3):
tpt_allow_deesc <- FALSE
true_dlt_ss_3p3_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
true_dlt_ss_3p3_2 <- c(0, 0.1, 0.2, 0.3, 0.4) #Not taken from spreadsheet
true_dlt_ss_3p3_3 <- c(0, 0, 0, 0.1, 0.2) #Not taken from spreadsheet
n_sims_3p3 <- 20 #Not taken from spreadsheet

##spec_crm variables (trial-design-specific, particularly CRM):
prior_mtd <- 3
prior_ttp <- c(0.02, 0.08, 0.20, 0.36, 0.52)
prior_var <- 0.5
skip_esc <- FALSE
skip_deesc <- TRUE
no_esc_if_observed_gt_target <- TRUE
stop_tox_x <- 0.1
stop_tox_y <- 0.7
stop_n_mtd <- 12 #12 default not taken from spreadsheet because 25 seemed too high
true_dlt_ss_crm_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
true_dlt_ss_crm_2 <- c(0, 0.1, 0.2, 0.3, 0.4) #Not taken from spreadsheet
true_dlt_ss_crm_3 <- c(0, 0, 0, 0.1, 0.2) #Not taken from spreadsheet
n_sims_crm <- 20 #Not taken from spreadsheet

##spec_other variables (trial-design-specific, particularly another design):
mock_parameter <- 1 #Not taken from spreadsheet
true_dlt_ss_other_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
true_dlt_ss_other_2 <- c(0, 0.1, 0.2, 0.3, 0.4) #Not taken from spreadsheet
true_dlt_ss_other_3 <- c(0, 0, 0, 0.1, 0.2) #Not taken from spreadsheet
n_sims_other <- 20 #Not taken from spreadsheet

# #Joining some of the parameters into lists; this may be useful to send to modelling backend (Jake)

# true_dlt_ss_3p3_list <- list(
#   true_dlt_ss_3p3_1,
#   true_dlt_ss_3p3_2,
#   true_dlt_ss_3p3_3
# )
# true_dlt_ss_crm_list <- list(
#   true_dlt_ss_crm_1,
#   true_dlt_ss_crm_2,
#   true_dlt_ss_crm_3
# )
# true_dlt_ss_other_list <- list(
#   true_dlt_ss_other_1,
#   true_dlt_ss_other_2,
#   true_dlt_ss_other_3
# )
# true_dlt_ss_all_list <- list(
#   true_dlt_ss_3p3_list,
#   true_dlt_ss_crm_list,
#   true_dlt_ss_other_list
# )
# names(true_dlt_ss_all_list) <- c("3p3", "crm", "other")
# true_dlt_ss_all_list
# true_dlt_ss_all_list$'3p3'
# true_dlt_ss_all_list$crm
# true_dlt_ss_all_list$other

# n_sims_all_list <- list(
#   n_sims_3p3,
#   n_sims_crm,
#   n_sims_other
# )
# names(n_sims_all_list) <- c("3p3", "crm", "other")
# n_sims_all_list
# n_sims_all_list$'3p3'
# n_sims_all_list$crm
# n_sims_all_list$other





#2.0 CODING THE MAIN UI: 4 DESIGN TABS (CONFIGURATIONS, DESIGN 1-3, COMPARISON) PLUS 1 CONDUCT TAB

#For each possible recommended design, defining a function containing its design-specific and simulation parameters
#Depending on the ranking vector, these will be selected and then be called inside fluidPage()
#This is so adding new designs beyond 3 plus 3 and CRM and new parameters is easier as the project advances

input_func_3p3 <- function(input) {
  message <-
    "3+3-specific parameters"

  tpt_allow_deesc_input <- checkboxInput("tpt_allow_deesc_input",
    "Allow any de-escalation for 3+3", value = tpt_allow_deesc)

  true_dlt_ss_3p3_1_input <- textInput("true_dlt_ss_3p3_1_input",
    "Simulation scenario 1 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_1)
  true_dlt_ss_3p3_2_input <- textInput("true_dlt_ss_3p3_2_input",
    "Simulation scenario 2 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_2)
  true_dlt_ss_3p3_3_input <- textInput("true_dlt_ss_3p3_3_input",
    "Simulation scenario 3 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_3)

  n_sims_3p3_input <- numericInput("n_sims_3p3_input",
    "Number of simulations per scenario (3+3)", value = n_sims_3p3)

  output <- list(message,
    tpt_allow_deesc_input,
    true_dlt_ss_3p3_1_input, true_dlt_ss_3p3_2_input, true_dlt_ss_3p3_3_input,
    n_sims_3p3_input)

  return(output)
}

input_func_crm <- function(input) {
  message <-
    "CRM-specific parameters"

  prior_mtd_input <- numericInput("prior_mtd_input",
    "Your guess of which doese level is the MTD", value = prior_mtd)
  prior_ttp_input <- textInput("prior_ttp_input",
    "Prior DLT rates vector", value = prior_ttp)
  prior_var_input <- numericInput("prior_var_input",
    "Prior variance", value = prior_var)

  skip_esc_input <- checkboxInput("skip_esc_input",
    "Allow skipping doses while escalating", value = skip_esc)
  skip_deesc_input <- checkboxInput("skip_deesc_input",
    "Allow skipping doses while de-escalating", value = skip_deesc)
  no_esc_if_observed_gt_target_input <- checkboxInput("no_esc_if_observed_gt_target_input",
    "No escalation if current dose level's observed DLT rate > TTL", value = no_esc_if_observed_gt_target)

  formula <-
    "Formula for safety stopping: p(true DLT rate at dose > TTL + x | observed data) > y"
  stop_tox_x_input <- sliderInput("stop_tox_x_input",
    "Toxicity exceeding TTL (x)", 0, ttl, value = stop_tox_x)
  stop_tox_y_input <- numericInput("stop_tox_y_input",
    "Confidence (y)", 0, 1, value = stop_tox_y)
  stop_n_mtd_input <- numericInput("stop_n_mtd_input",
    "Min n at MTD for stopping", value = stop_n_mtd)

  true_dlt_ss_crm_1_input <- textInput("true_dlt_ss_crm_1_input",
    "Simulation scenario 1 true DLT rates vector (CRM)", value = true_dlt_ss_crm_1)
  true_dlt_ss_crm_2_input <- textInput("true_dlt_ss_crm_2_input",
    "Simulation scenario 2 true DLT rates vector (CRM)", value = true_dlt_ss_crm_2)
  true_dlt_ss_crm_3_input <- textInput("true_dlt_ss_crm_3_input",
    "Simulation scenario 3 true DLT rates vector (CRM)", value = true_dlt_ss_crm_3)

  n_sims_crm_input <- numericInput("n_sims_crm_input",
    "Number of simulations per scenario (CRM)", value = n_sims_crm)

  output <- list(
    message,
    prior_mtd_input, prior_ttp_input, prior_var_input,
    skip_esc_input, skip_deesc_input, no_esc_if_observed_gt_target_input,
    formula, stop_tox_x_input, stop_tox_y_input,
    stop_n_mtd_input,
    true_dlt_ss_crm_1_input, true_dlt_ss_crm_2_input, true_dlt_ss_crm_3_input,
    n_sims_crm_input)
  
  return(output)
}

input_func_other <- function(input) {
  message <-
    "[Design Name Here]-specific parameters"

  mock_parameter_input = sliderInput("mock_parameter_input",
    "Mock parameter", 0, 2, value = mock_parameter)

  true_dlt_ss_other_1_input = textInput("true_dlt_ss_other_1_input",
    "Simulation scenario 1 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_1)
  true_dlt_ss_other_2_input = textInput("true_dlt_ss_other_2_input",
    "Simulation scenario 2 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_2)
  true_dlt_ss_other_3_input = textInput("true_dlt_ss_other_3_input",
    "Simulation scenario 3 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_3)

  n_sims_other_input = numericInput("n_sims_other_input",
    "Number of simulations per scenario ([Design Name Here])", value = n_sims_other)

  output <- list(message,
    mock_parameter_input,
    true_dlt_ss_other_1_input, true_dlt_ss_other_2_input, true_dlt_ss_other_3_input,
    n_sims_other_input)

  return(output)
}

#Selecting which functions to be called within UI based on ranking vector
func_list <- list(input_func_3p3(), input_func_crm(), input_func_other())
names(func_list) <- c("3p3", "crm", "other")
func_1 <- func_list[ranking[1]]
func_2 <- func_list[ranking[2]]
func_3 <- func_list[ranking[3]]

#fluidPage() - backbone of UI
main_ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Parameter Configurations",
      fluidRow(
        column(1, width = 3, func_1),
        column(2, width = 3, func_2),
        column(3, width = 3, func_3)
      )
    ),
    tabPanel(
      ranking[1]
    ),
    tabPanel(
      ranking[2]
    ),
    tabPanel(
      ranking[3]
    ),
    tabPanel(
      "Cross-Method Comparison"
    ),
    tabPanel(
      "Conduct Tab"
    )
  )
)
main_server <- function(input, output, session) {}
shinyApp(main_ui, main_server)