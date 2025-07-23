library(shiny)
library(shiny.semantic)


#################################### From Configuarations Tab ####################################

##Defining non-design-specific + simulation parameters column inputs function
non_specific_column_func <- function() {
  n_doses_output <- numericInput("n_doses_inputt", "How many dose levels are being tested?", min = 1, value = "5")
  ttl_output <- numericInput("ttl_inputt", "What is the target toxicity level for this trial, as a decimal?", min = 0, max = 1, value = "0.3")
  max_size_output <- numericInput("max_size_inputt", "What is the maximum sample size for this trial?", min = 1, value = "30")
  start_dose_output <- numericInput("start_dose_inputt", "What is the starting dose level?", min = 1, value = "1")
  cohort_output <- numericInput("cohort_inputt", "What size will the cohorts be?", min = 1, value = "5")
  non_specific_ui_inputts <- tagList(
    n_doses_output,
    ttl_output,
    max_size_output,
    start_dose_output,
    cohort_output
  )

  
  separator <- "___________________________________________"
  title <- "GENERAL TRIAL PARAMETERS"
  display_button <- checkboxInput("display_input_all", "Display parameters", value = F)
  conditional_non_specific_ui_inputs <- conditionalPanel(condition = "input.display_input_all==1", non_specific_ui_inputts)
  separator <- "___________________________________________"
  text <- "SIMULATION PARAMETERS"
  n_sims_input <- numericInput("n_sims_input", "How many simulations would you like to run per design per scenario?", value = 10)
  n_scenarios_input <- numericInput("n_scenarios_input", "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)
  text2 <- "Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities in the table below:"
  table_output <- DT::DTOutput("table_output") # This is to test the table output used for the simulations tab.
  test_df_table <- DT::DTOutput("test_df")
  #plot_button <- actionButton("plot_button", label = "Test plot")
  #plot <- plotOutput("plot")
  return <- list(upload_button, download_button, separator, title, display_button, conditional_non_specific_ui_inputs,
  separator, text, n_sims_input, n_scenarios_input, text2, #table_output, 
  test_df_table)
}

##Defining Configurations tab columns
display_input_id <- list()
display_condition <- list()
specific_columns <- list()

select_specific_columns <- function() {
skip_esc_crm_input <- radioButtons("skip_esc_crm_input","Would you like to be able to skip doses when escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE)
skip_deesc_crm_input <- radioButtons("skip_deesc_crm_input","Would you like to be able to skip doses when de-escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)
above_target_input <- radioButtons("above_target_input",
"Do you want to prevent escalation of doses if the overall observed DLT rate at the current dose level is above the target DLT rate?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)
prior_var_input <- numericInput("prior_var_input", "What is the estimate of the prior variance?", min = 0, value = 0.1)
stop_n_mtd_input <- numericInput("stop_n_mtd_input", "What is the minimum number of patients required at recommended dose before early stopping?", min = 1, value = 24)
skeleton_input <- textInput("skeleton_input", "What are the prior estimates of the DLT rates at each dose? Please make this an increasing list and separate each value with a comma.", value = "0.108321683015674,0.255548628279939,0.425089891767129,0.576775912195444,0.817103320499882")
prior_mtd_input <- numericInput("prior_mtd_input", "What is your prior guess of the MTD?", min = 1, value = 8)
stop_tox_x_input <- numericInput("stop_tox_x_input", "When using the this Bayesian safety early criterion: p(true DLT rate at lowest dose > target DLT rate + x | data) > y, what would you like x to be? This is the excess toxicity above the target DLT.", min = 0, value = 0.09)
stop_tox_y_input <- numericInput("stop_tox_y_input", "What would you like y to be? This is the confidence level for safety stopping.", min = 0, max = 1, value = 0.77)
specific_ui_inputs_crm <- tagList(
  skip_esc_crm_input,
  skip_deesc_crm_input,
  above_target_input,
  prior_var_input,
  stop_n_mtd_input,
  skeleton_input,
  prior_mtd_input,
  stop_tox_x_input,
  stop_tox_y_input
)

skip_tpt_input <- radioButtons("skip_tpt_input","Would you like to be able to skip doses when de-escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)

specific_ui_inputs_other <- radioButtons("specific_ui_inputs_other","This is a placeholder. Clicking this button will do nothing.",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)


title1 <- "1: CRM"
display_button_crm <- checkboxInput("display_crm", "Display parameters", value = F)
conditional_crm <- conditionalPanel(condition = "input.display_crm==1", specific_ui_inputs_crm, width = 4)

title2 <- "2: 3+3"
display_button_tpt <- checkboxInput("display_tpt", "Display parameters", value = F)
conditional_tpt <- conditionalPanel(condition = "input.display_tpt==1", skip_tpt_input, width = 4)

title3 <- "3: Other (TEST)"
display_button_other <- checkboxInput("display_other", "Display parameters", value = F)
conditional_other <- conditionalPanel(condition = "input.display_other==1", specific_ui_inputs_other, width = 4)

fluidRow(column(4,title1, display_button_crm, conditional_crm),
  column(4,title2, display_button_tpt, conditional_tpt),
  column(4,title3, display_button_other, conditional_other)
)
}

########################################### Running the UI ###########################################

trial_design_ui <- function(id) {
  ns <- NS(id)
  page_sidebar( 
    card(

    ),
    sidebar = sidebar(
      h3("Trial Design"),
      
      fileInput("config_file_upload", 
      "Saved a Trial Design file? Input it here to retrieve your configurations.", 
      accept = c(".csv", ".rds")),

      p("Want to save your configurations for later? Download a them as a file by clicking the button below."),
      downloadButton("config_save_button", "Download file with all configurations"),

      p("Done filling out the configurations? Click the button below to run simulations."),
      actionButton(ns("view_simulation"), "View Simulation")
    )
  )
}

trial_design_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}