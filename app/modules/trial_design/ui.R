library(shiny)





#1.0 DEFINING VARIABLES TO BE INPUTTED FROM QUESTIONNAIRE UI

#Most variable names were taken from the master_variables spreadsheet published by Sian and reviewed by Jake, added to main branch on 22Aug23
#Variable names not taken from spreadsheet are signalled below
#Variable values are arbitrary since we have not integrated them from the Questionnaire UI yet; I tried to stick to the defaults specified in the spreadsheet

##user_profile variables:
ranking <- c("3p3", "crm", "other") #Not taken from spreadhseet; first ranking possibility. Commen/uncomment and re-run script
#ranking <- c("crm", "3plus3", "other") #Second ranking possibility. Commen/uncomment and re-run script
#ranking <- c("other", "3plus3", "crm") #Third ranking possibility. Commen/uncomment and re-run script
design_1 <- ranking[1]
design_2 <- ranking[2]
design_3 <- ranking[3]

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





#2.0 CODING THE MAIN UI (4 DESIGN TABS - CONFIGURATIONS, DESIGN 1-3, COMPARISON - AND 1 CONDUCT TAB)

#For each possible recommended design, defining a function containing its design-specific and simulation parameters
#These will then be called inside fluidPage()
#This is so adding new designs (beyond 3 plus 3 and CRM) and new parameters is easier

input_func_3p3 <- function(input) {
  tpt_allow_deesc_input <- checkboxInput("tpt_allow_deesc_input", "Allow any de-escalation for 3+3?", value = tpt_allow_deesc)
  true_dlt_ss_3p3_1_input <- textInput("true_dlt_ss_3p3_1_input", "Simulation scenario 1 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_1)
  true_dlt_ss_3p3_2_input <- textInput("true_dlt_ss_3p3_2_input", "Simulation scenario 2 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_2)
  true_dlt_ss_3p3_3_input <- textInput("true_dlt_ss_3p3_3_input", "Simulation scenario 3 true DLT rates vector (3+3)", value = true_dlt_ss_3p3_3)
  n_sims_3p3_input <- numericInput("n_sims_3p3_input", "Number of simulations per scenario (3+3)", value = n_sims_3p3)
  message <- "These were 3+3-specific parameters!"
  output <- list(tpt_allow_deesc_input, true_dlt_ss_3p3_1_input, true_dlt_ss_3p3_2_input, true_dlt_ss_3p3_3_input, n_sims_3p3_input, message)
  return(output)
}

input_fuc_crm <- function(input) {
  UI_input_1 <- numericInput("n_sims_crm_input", "Number of Simulations (CRM)", value = n_sims_crm)
  UI_input_2 <- textInput("true_dlt_ss_crm_1_input", "Simulation Scenario 1 True DTLs (CRM)", value = true_dlt_ss_crm_1)
  UI_input_3 <- textInput("prior_ttp_input", "Prior DLTs", value = prior_ttp)
  UI_input_4 <- "These were CRM-specific parameters!"
  output <- list(UI_input_1, UI_input_2, UI_input_3, UI_input_4)
  return(output)
}

input_func_other <- function(input) {
  UI_input_1 <- numericInput("n_sims_other_input", "Number of Simulations ([Design Name Here])", value = n_sims_other)
  UI_input_2 <- textInput("true_dlt_ss_other_1_input", "Simulation Scenario 1 True DTLs ([Design Name Here])", value = true_dlt_ss_other_1)
  UI_input_3 <- sliderInput("mock_parameter", "Mock Parameter", 0, 2, value = mock_parameter)
  UI_input_4 <- "These were [Design Name Here]-specific parameters!"
  output <- list(UI_input_1, UI_input_2, UI_input_3, UI_input_4)
  return(output)
}

func_list <- list(input_func_3p3(), input_fuc_crm(), input_func_other())
names(func_list) <- c("3plus3", "crm", "other")
func_1 <- func_list[design_1]
func_2 <- func_list[design_2]
func_3 <- func_list[design_3]

main_ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Parameter Configuration",
      fluidRow(
        column(1, width = 2, design_1,
          func_1),
        column(2, width = 2, design_2,
          func_2),
        column(3, width = 2, design_3,
          func_3)
      )
    ),
    tabPanel(
      design_1
    ),
    tabPanel(
      design_2
    ),
    tabPanel(
      design_3
    ),
    tabPanel(
      "Cross-Method Comparison"
    )
  )
)
main_server <- function(input, output, session) {}
shinyApp(main_ui, main_server)