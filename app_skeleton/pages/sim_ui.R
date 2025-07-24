library(shiny)
library(shiny.semantic)

sim_ui <- function(id) {
  ns <- NS(id)

  # Simulation-Specific Inputs
    n_sims_input <- numericInput(ns("n_sims_input"), "How many simulations would you like to run per design per scenario?", value = 10)
    n_scenarios_input <- numericInput(ns("n_scenarios_input"), "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)
    text2 <- "Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities in the table below:"
    table_output <- DT::DTOutput(ns("table_output")) # This is to test the table output used for the simulations tab.
    test_df_table <- DT::DTOutput(ns("test_df"))
  simulation_inputs <- tagList(
    n_sims_input,
    n_scenarios_input,
    text2,
    table_output,
    test_df_table
  )


  # Running the tab itself
  page_sidebar(
    card(
      h3("Testing the movement of n_doses from Trial Design to Simulation"),
      p("Next to this text should be n_doses:"),
    ),
    sidebar = sidebar(
      h3("Simulation Inputs"),
      p("Please fill out the following inputs and click 'Run Simulation' to see the results."),
      actionButton(ns("run_simulation"), "Run Simulation"),
      tags$hr(), # Separator line
      simulation_inputs
    )
  )
}

sim_server <- function(id) {
  moduleServer(id, function(input, output, session) {

     ######################################## Configuration tab's simulation scenarios table code ########################################

  #### Simulation Variables from Configurations Tab
  n_sims <- reactive({as.numeric(input$n_sims_input)})
  n_scenarios <- reactive({as.numeric(input$n_scenarios_input)})

  ## Writing code such that the start dose cannot be greater than the number of doses and the number of doses cannot be less than the start dose

  # start_dose cannot be greater than the n_doses
   observe({
    updateNumericInput(session, "start_dose_inputt", max = input$n_doses_inputt)
  })
  
  # n_doses cannot be less than the start_dose
  observe({
    # Update the min value of the maxValue input based on minValue
    updateNumericInput(session, "n_doses_inputt", min = input$start_dose_inputt)
  })


  ############## Reactive True DLT Probabilities Table ##############

  reactive_df <- reactiveVal() # initalising a reactive value to store the data frame

  observeEvent({input$n_scenarios_input; input$n_doses_inputt}, {
   
  dimensions <- matrix(0, nrow = input$n_scenarios_input, ncol = input$n_doses_inputt)
  colnames(dimensions) <- paste("d", 1:input$n_doses_inputt, sep = "")
  dataframe <- data.frame(dimensions) # What was previously doses_table

  Scenario <- matrix(as.numeric(1:input$n_scenarios_input), nrow = input$n_scenarios_input, ncol = 1)
  
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
  })
}