library(shiny)





#1.0 VARIABLE DEFINITION
#Most variable names taken from master_variables spreadsheet published by Sian on Github issues page
#Variable names not taken from spreadsheet (weren't defined yet) are signalled below
#Variable values are arbitrary since we have not integrated them yet; I tried to stick to the defaults determined in the spreadsheet
#user_profile variables:
ranking = c("3plus3", "crm", "other") #Not taken from spreadhseet; first ranking possibility
#ranking = c("crm", "3plus3", "other") #Second ranking possibility
#ranking = c("other", "3plus3", "crm") #Third ranking possibility
design_1 = ranking[1]
design_2 = ranking[2]
design_3 = ranking[3]
#non_spec variables (non trial-design-specific):
n_doses = 5
ttl = 0.2
max_n = 24
start_dose = 1
curent_seed = 1234
cohort_size = 3
#spec_3plus3 variables (trial-design-specific, particularly 3+3):
n_sims_3plus3 = 20 #Not taken from spreadsheet
true_dlt_ss_3plus3_1 = c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
true_dlt_ss_3plus3_2 = c(0, 0.1, 0.2, 0.3, 0.4) #Not taken from spreadsheet
true_dlt_ss_3plus3_3 = c(0, 0, 0, 0.1, 0.2)
#spec_crm variables (trial-design-specific, particularly CRM):
n_sims_crm = 20 #Not taken from spreadsheet; spreadsheet defines n_sims as being CRM-specific
skip_esc = FALSE
skip_deesc = TRUE
no_esc_if_observed_gt_target = TRUE
prior_var = 0.5
stop_tox_x = 0.1
stop_tox_y = 0.7
stop_n_mtd = 12 #"mtd" is capitalized on spredsheet; default value not taken from spreadsheet because it seemed too high
prior_ttp = c(0, 0.1, 0.2, 0.3, 0.4) #Default value not taken from spreadsheet because it wasn't defined
true_dlt_ss_crm_1 = c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
true_dlt_ss_crm_2 = c(0, 0.1, 0.2, 0.3, 0.4) #Not taken from spreadsheet
true_dlt_ss_crm_3 = c(0, 0, 0, 0.1, 0.2) #Not taken from spreadsheet
prior_mtd = 3
#spec_other variables (trial-design-specific, particularly another design):
n_sims_other = 20 #Not taken from spreadsheet
true_dlt_ss_other_1 = c(0.2, 0.3, 0.4, 0.5, 0.6) #Not taken from spreadsheet
mock_parameter = 1





#2.0 UI CODING
func_3plus3_inputs = function(input) {
  UI_input_1 = numericInput("n_sims_3plus3_input", "Number of Simulations (3 plus 3)", value = n_sims_3plus3)
  UI_input_2 = textInput("true_dlt_ss_3plus3_1_input", "Simulation Scenario 1 True DTLs (3 plus 3)", value = true_dlt_ss_3plus3_1)
  UI_input_3 = "These were 3+3-specific parameters!"
  output = list(UI_input_1, UI_input_2, UI_input_3)
  return(output)
}
func_crm_inputs = function(input) {
  UI_input_1 = numericInput("n_sims_crm_input", "Number of Simulations (CRM)", value = n_sims_crm)
  UI_input_2 = textInput("true_dlt_ss_crm_1_input", "Simulation Scenario 1 True DTLs (CRM)", value = true_dlt_ss_crm_1)
  UI_input_3 = textInput("prior_ttp_input", "Prior DLTs", value = prior_ttp)
  UI_input_4 = "These were CRM-specific parameters!"
  output = list(UI_input_1, UI_input_2, UI_input_3, UI_input_4)
  return(output)
}
func_other_inputs = function(input) {
  UI_input_1 = numericInput("n_sims_other_input", "Number of Simulations ([Design Name Here])", value = n_sims_other)
  UI_input_2 = textInput("true_dlt_ss_other_1_input", "Simulation Scenario 1 True DTLs ([Design Name Here])", value = true_dlt_ss_other_1)
  UI_input_3 = sliderInput("mock_parameter", "Mock Parameter", 0, 2, value = mock_parameter)
  UI_input_4 = "These were [Design Name Here]-specific parameters!"
  output = list(UI_input_1, UI_input_2, UI_input_3, UI_input_4)
  return(output)
}

func_list = list(func_3plus3_inputs(), func_crm_inputs(), func_other_inputs())
names(func_list) = c("3plus3", "crm", "other")
func_1 = func_list[design_1]
func_2 = func_list[design_2]
func_3 = func_list[design_3]

main_ui = fluidPage(
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
#main_server = function(input, output, session) {}
shinyApp(main_ui, main_server)