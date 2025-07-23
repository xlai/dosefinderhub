

# Helper function to generate UI element
generate_ui_element <- function(question) {
  params <- parse_params(question$params)
  switch(question$q_type,
    radioButtons = radioButtons(inputId = question$q_variable,
                                label = question$q_text,
                                choices = strsplit(params[["choices"]], ",")[[1]],
                                width = 500,
                                selected = question$value),
    numeric = numericInput(inputId = question$q_variable,
                           label = question$q_text,
                           min = as.numeric(params[["min"]]),
                           value = question$value,
                           width = 500),
    slider = sliderInput(inputId = question$q_variable,
                         label = question$q_text,
                         min = as.numeric(params[["min"]]),
                         max = as.numeric(params[["max"]]),
                         value = question$value,
                         width = 500),
    text = textInput(inputId = question$q_variable,
                     label = question$q_text,
                     placeholder = "Enter your hint here",
                     value = question$value, width = 500)
  )
}

# Extracting non-design-specific UI specs
non_specific_ui_inputs <- tagList(
  lapply(dummy_data_trial$q_number, function(i) {
    question <- subset(dummy_data_trial, q_number == i)
    generate_ui_element(question)
  })
)

# Extracting design-specific UI specs
specific_ui_inputs <- lapply(ranking, function(method_current) {
  dummy_data_method_current <- subset(dummy_data_method, design == method_current)
  tagList(
    lapply(dummy_data_method_current$q_number, function(i) {
      question <- subset(dummy_data_method_current, q_number == i)
      generate_ui_element(question)
    })
  )
})

# Trying a different method

#CONFIGURATIONS TAB

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

  #label <- "Input file with all configurations"
  upload_button <- fileInput("config_file_upload", "Input file with all configurations", accept = c(".csv", ".rds"))
  #label_2 <- "Download file with all configurations"
  download_button <- downloadButton("config_save_button", "Download file with all configurations")
  separator <- "___________________________________________"
  title <- "GENERAL TRIAL PARAMETERS"
  display_button <- checkboxInput("display_input_all", "Display parameters", value = F)
  conditional_non_specific_ui_inputs <- conditionalPanel(condition = "input.display_input_all==1", non_specific_ui_inputts)
  separator <- "___________________________________________"
  text <- "SIMULATION PARAMETERS"
  n_sims_input <- numericInput("n_sims_input", "How many simulations would you like to run per design per scenario?", value = 10)
  n_scenarios_input <- numericInput("n_scenarios_input", "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)
  text2 <- "Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities in the table below:"
  table_output <- DT::DTOutput("table_output")
  test_df_table <- DT::DTOutput("test_df")
  #plot_button <- actionButton("plot_button", label = "Test plot")
  #plot <- plotOutput("plot")
  return <- list(upload_button, download_button, separator, title, display_button, conditional_non_specific_ui_inputs,
  separator, text, n_sims_input, n_scenarios_input, text2, table_output, test_df_table)
}

##Defining Configurations tab columns
display_input_id <- list()
display_condition <- list()
specific_columns <- list()

select_specific_columns <- function() {
  
  #for(n in 1:(length(ranking))) {
   # display_input_id[[n]] <- paste0("display_input_", ranking[n], sep="")
   # display_condition[[n]] <- paste0("input.", display_input_id[[n]], "==1", sep="")
   # specific_columns[[n]] <- column(
   #   n,
    #  paste0(": ", pretty_ranking[n]),
    #  checkboxInput(display_input_id[[n]], "Display parameters", value = F),
    #  conditionalPanel(condition = display_condition[[n]], specific_ui_inputs[[n]]),
    #  width = 4)
  # }
  # return(specific_columns)

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



##Defining Configurations tab input function
config_tab_input_func <- function() {
  sidebarLayout(
    sidebarPanel(non_specific_column_func(), width = 4),
    mainPanel(select_specific_columns()),
  )
}


#SIMULATIONS TAB

##Simulations tab UI function
sim_tab_input_func <- function() {
     sidebarLayout(
     sidebarPanel(

      selectizeInput("simulation_design_selection_input", "Select which designs' simulation outputs to see",
        choices = pretty_ranking,
        multiple = TRUE,
        options = list(plugins = list('remove_button'))),
      
      uiOutput("scen_output_question"),
      
      selectizeInput("metric_selection_input", "Select outputs/metrics",
        choices = c("% participants treated at dose",
          "% times dose was selected as MTD",
          "Accuracy",
          "Duration",
          "Overdosing"),
        multiple = TRUE,
        list(plugins = list('remove_button'))),
      
      #selectizeInput("visual_selection_input", "Select type of output",
        #choices = c("Table", "Plot"),
        #multiple = TRUE,
        #list(plugins = list('remove_button'))),

      actionButton("submit", "Submit"),

      width = 5

     ),

     mainPanel(width = 7, uiOutput("tables_ui") # Ouputting the simulation results - for now, this is just the tpt tables.
     )
  )}
       # Ouputting the simulation results - for now, this is just the tpt table.
 
#CONDUCT TAB

##Conduct tab UI function
cond_tab_input_func <- function() {
  sidebarLayout(
    
    sidebarPanel(
        radioButtons("conduct_design_selection_input", "Select which design to update during trial conduct",
          choices = c(unique(pretty_ranking)),
          width = 500),
        "Observed DLTs input table:",
        treated_participants_input <- numericInput("treated_participants_input", "How many participants have been treated & observed for DTL?", min = 1, value = dummy_data_trial[dummy_data_trial$q_variable == "cohort_size", "value"]),
        conduct_table_output <- DT::DTOutput("conduct_table_output"),
        #conduct_plot_button <- actionButton("conduct_plot_button", label = "Test plot"),
        #conduct_plot <- plotOutput("conduct_plot"),
        width = 5
    ),
    mainPanel("INSERT DYNAMIC CONDUCT OUTPUTS HERE")
  )
}



#MAIN UI

##Defining UI tabs
ui_tabs <- list()
ui_tabs[[1]] <- tabPanel("Configurations",
  config_tab_input_func()
)
ui_tabs[[2]] <- tabPanel("Simulations",
  sim_tab_input_func()
)
ui_tabs[[3]] <- tabPanel("Conduct",
  cond_tab_input_func()
)

##Running app
ui <- fluidPage(
  do.call(tabsetPanel, c(ui_tabs))
)




#shinyApp(ui, server_all)
