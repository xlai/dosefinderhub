library(shiny)
library(shiny.semantic)

sim_ui <- function(id) {
  ns <- NS(id)

  # Simulation-Specific Inputs
    n_sims_input <- numericInput(ns("n_sims_input"), "How many simulations would you like to run per design per scenario?", value = 10)
    n_scenarios_input <- numericInput(ns("n_scenarios_input"), "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)

  n_sims_warning_text <- textOutput(ns("n_sims_warning"))
  n_scenarios_warning_text <- textOutput(ns("n_scenarios_warning"))
    #table_output <- DT::DTOutput(ns("table_output")) # This is to test the table output used for the simulations tab.
  simulation_inputs <- tagList(
    n_sims_input,
    n_sims_warning_text,
    n_scenarios_input,
    n_scenarios_warning_text
    #table_output,
  )

  test_df_table <- DT::DTOutput(ns("test_df")) # The reactive table for the true DLT probabilities
  # The 'Refresh Dimensions' button doesn't work, so the table changes dimensions when the number of scenarios changes.

  # Running the tab itself
  page_sidebar(
    card(height = 100,
      card_header("Simulation Inputs"),
      card_body(
      simulation_inputs,
      p("Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities 
      in the table below. If the dimensions do not match, change the number of scenarios and doses and press 
      'Refresh Dimensions'."),
      test_df_table,
      input_task_button(ns("refresh_table_input"), "Refresh Table Dimensions")
      )),

    sidebar = sidebar(
      h3("What Do You Want to Simulate?"),
            ################################ Simulation tab UI ################################
      # This code is copied from the Simulation tab UI in trial_design/ui.R
      selectizeInput("simulation_design_selection_input", "Select which designs' simulation outputs to see",
        choices = pretty_ranking,
        multiple = TRUE,
        options = list(plugins = list('remove_button'))),
      
      uiOutput(ns("scen_output_question")),
      
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
        #list(plugins = list('remove_button')))

      ##### Run Simulation Button and Dowload Results Button #####
      tags$hr(), # Separator line
      h3("Run Simulation"),
      p("Please fill out the Simulation Inputs and click 'Run Simulation' to see the results."),
      actionButton(ns("run_simulation"), "Run Simulation"),
      tags$hr(), # Separator line
      h3("Download Results"),
      p("Want to save your simulation results? Click a button below to download them as a CSV file."),
      downloadButton(ns("download_simulation_results"), "Download Simulation Results")
    )
  ) 
}

sim_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

     ######################################## Configuration tab's simulation scenarios table code ########################################

  #### Simulation Variables from Configurations Tab
  n_sims <- reactive({as.numeric(input$n_sims_input)})
  n_scenarios <- reactive({as.numeric(input$n_scenarios_input)})
  

  ######################### Validation ############################
  validation_state <- reactiveValues(
    n_sims_val = NULL,
    n_scenarios_val = NULL
  )
  
  # Define base validation rules for each input
  base_validation_rules <- list(
    n_sims_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    n_scenarios_val = list(min_val = 1, max_val = 3, integer_only = TRUE)
  )
  
  validation_rules <- reactive({base_validation_rules})
  
  # Function to update validation for a specific input
  update_validation <- function(input_id, value) {
    rules <- validation_rules()[[input_id]]
    error_msg <- validate_numeric_input(
      value, 
      min_val = rules$min_val, 
      max_val = rules$max_val, 
      integer_only = rules$integer_only
    )
    
    validation_state[[input_id]] <- error_msg
    
    # Update the warning message next to the input
    warning_id <- paste0(input_id, "_warning")
    if (is.null(error_msg)) {
      validation_state[[warning_id]] <- ""
    } else {
      validation_state[[warning_id]] <- paste("⚠️", error_msg)
    }
  }

  # Observe changes in each input and validate
  # General Inputs
  observe({
    update_validation("n_sims_val", input$n_sims_input)
  })
  
  observe({
    update_validation("n_scenarios_val", input$n_scenarios_input)
  })

  # Render individual warning messages next to each input
  output$n_sims_warning <- renderText({
    validation_state$n_sims_val_warning %||% ""
  })

  output$n_scenarios_warning <- renderText({
    validation_state$n_scenarios_val_warning %||% ""
  })
  
  ############## Reactive True DLT Probabilities Table ##############

  reactive_df <- reactiveVal() # initalising a reactive value to store the data frame

  observeEvent({input$refresh_table_input}, {

  dimensions <- matrix(0, nrow = n_scenarios(), ncol = shared$n_dosess())
  colnames(dimensions) <- paste("d", 1:shared$n_dosess(), sep = "")
  dataframe <- data.frame(dimensions) # What was previously doses_table

  Scenario <- matrix(as.numeric(1:n_scenarios()), nrow = n_scenarios(), ncol = 1)

  dataframe_row_1 <- data.frame(Scenario) # What was previously scenarios_table

  cbind <- cbind(dataframe_row_1, dataframe)
  reactive_df(cbind) # Updating the reactive value with the new data frame
  })
  
  output$test_df <- renderDT({
    datatable(reactive_df(), editable = TRUE, rownames = FALSE) #, scrollX = TRUE, scrollX="250px", paging = FALSE
  })
  
  # Observe the cell edits in the datatable
  observeEvent(input$test_df_cell_edit, {
    info <- input$test_df_cell_edit

    modified_data <- reactive_df()
    modified_data[info$row, info$col + 1] <- DT::coerceValue(info$value, modified_data[info$row, info$col]) # +1 is here to counterract the movement of edited data.
    reactive_df(modified_data)
    #print(str(reactive_df()))
  })

  true_dlts <- reactive({
    reactive_df()[, -1] # Exclude the first column (Scenario)
  })

  ######################################## Simulation Tab's Server Code ######################################

  ##### Scenarios Question ######
   new_n_scen <- reactive(as.numeric(input$n_scenarios_input))
  updated_scen_choices <- reactive(paste0("Scenario ", 1:new_n_scen()))

  output$scen_output_question <- renderUI({
    tagList(
      selectizeInput("scen_output_input", "Select scenarios", choices = updated_scen_choices(),
        multiple = TRUE, list(plugins = list('remove_button')))
    )
  })


  }) # End of moduleServer
} # End of sever function