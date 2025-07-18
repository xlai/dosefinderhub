

server_all <- function(input, output, session) {


  ######################################## Configuration tab's file upload/download ########################################

  #Upload
  observe({
    
    in_file <- input$config_file_upload

    if (is.null(in_file)) {
      return(NULL)
    }

    ext <- tools::file_ext(in_file$datapath)

    if (ext == "csv") {
      user_responses <- read.csv(in_file$datapath)
    } else if (ext == "rds") {
      user_responses <- readRDS(in_file$datapath)
    }

    for (i in seq_len(nrow(user_responses))) {
      updateNumericInput(session, inputId = user_responses$inputId[i], value = user_responses$value[i])
    }

  })

  #Download

  output$config_save_button <- shiny::downloadHandler(
    filename = function() {
      paste("user_responses-", Sys.Date(), ".csv", sep = "")
    },

    content = function(file) {
        input_ids_for_df <- c((questions$trial)$q_variable, (questions$method)$q_variable, (questions$ranking)$q_variable)

        inputs_for_df <- c()
        for (input.i in 1:length(input_ids_for_df)){
          inputs_for_df <- append(inputs_for_df, input[[input_ids_for_df[input.i]]])
        }
  
        inputs_data_frame <- data.frame(inputId=c(input_ids_for_df), value=c(inputs_for_df))
        write.csv(inputs_data_frame, file)
    }
  )

  ################################ Configuration tab's sidebar code ################################
   
  n_dosess <- reactive({as.numeric(input$n_doses_inputt)}) # Using double ending letters to avoid mixing up with other input (for now)
  ttl <- reactive({as.numeric(input$ttl_inputt)})
  max_size <- reactive({as.numeric(input$max_size_inputt)})
  start_dose <- reactive({as.numeric(input$start_dose_inputt)}) 
  cohort_size <- reactive({as.numeric(input$cohort_inputt)})
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

  ######################################## Configuration tab's simulation scenarios table code ########################################

  
  # Create a data frame with the specified number of rows and columns

  doses_table <- reactive({
    dimensions <- matrix(0, nrow = input$n_scenarios_input, ncol = input$n_doses_inputt)
    dataframe <- data.frame(dimensions)
    
  })

  scenario_table <- reactive({
    Scenario <- matrix(1:input$n_scenarios_input, nrow = input$n_scenarios_input, ncol = 1)
    dataframe_row_1 <- data.frame(Scenario)
  })

  reactive_df <- reactive({cbind(scenario_table(), doses_table())})


  output$test_df <- renderDT({
    datatable(reactive_df(), editable = list(target = "cell", columns = c(2:(input$n_doses_inputt + 1))), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE, colnames = c("Scenario", paste(rep("d", input$n_doses_inputt), as.list(as.character(1:input$n_doses_inputt)), sep = ""))) #, scrollX = TRUE, scrollX="250px", paging = FALSE
  })

  
 # observeEvent(input$plot_button, {
    #Capture current data and transform for plotting
  #  current_data <- reactive_table_data()
  #  plot_data <- tidyr::gather(current_data[, -1, drop = FALSE], key = "Dose Level", value = "Value")
   # plot_data$Scenario <- rep(current_data$Scenario, each = ncol(current_data) - 1)
    
    #Create the plot
   # p <- ggplot(plot_data, aes(x = `Dose Level`, y = Value)) +
    #  geom_line() +
   #   geom_point() +
    #  labs(x = "Dose Levels",
    #       y = "'True' DLT Rates") +
    #  theme_minimal()
    
    # Render the plot
    #output$plot <- renderPlot({
   #   p
   # })
 # })
  
  ######################################## Simulation tab server code ########################################

  new_n_scen <- reactive(as.numeric(input$n_scenarios_input))
  updated_scen_choices <- reactive(paste0("Scenario ", 1:new_n_scen()))
  
  output$scen_output_question <- renderUI({
    tagList(
      selectizeInput("scen_output_input", "Select scenarios", choices = updated_scen_choices(),
        multiple = TRUE, list(plugins = list('remove_button')))
    )
  })

 # Simulation outputs

  tpt_sim <- sim_tpt(n_doses = 5, ttl = 1/3, max_n = 30, start_dose = 1, n_sims = 100, true_dlt_ss = c(0.05,0.15,1/3,0.5,0.8), current_seed = 12345)
  # Using the hardcoded values for now to make sure the table displays correctly. In the next commit, these values will be replaced with the desired ones.
  tpt_sim_table <- tpt_sim$treatment_tab

  output$scen_sim_output <-renderDT(tpt_sim_table, 
    options = list(
      pageLength = 5,
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )

  ######################################## Conduct tab table code ########################################

  conduct_reactive_table_data <- reactiveVal(data.frame(matrix(ncol = 3, nrow = 0, dimnames = list(NULL, c("Cohort number", "Dose level", "DLT?")))))
  observe({
    conduct_current_data <- conduct_reactive_table_data()
    conduct_target_rows <- as.numeric(input$treated_participants_input)
    conduct_current_rows <- nrow(conduct_current_data)
    conduct_rows_to_add <- conduct_target_rows - conduct_current_rows
    if (conduct_rows_to_add > 0) {
      conduct_new_rows <- data.frame(
        Participants = seq_len(conduct_rows_to_add) + conduct_current_rows,
        matrix(0, ncol = 3, nrow = conduct_rows_to_add, dimnames = list(NULL, c("Cohort number", "Dose level", "DLT?")))
      )
      conduct_updated_data <- rbind(conduct_current_data, conduct_new_rows)
    } else {
      conduct_updated_data <- head(conduct_current_data, conduct_target_rows)
    }
    conduct_reactive_table_data(conduct_updated_data)
  })
  output$conduct_editable_table <- renderDT({
    datable(conduct_reactive_table_data(), editable = TRUE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
      list(targets = 0, className = "not-editable")
    )))
  }, server = FALSE)
  output$conduct_table_output <- renderDT({
    datatable(conduct_reactive_table_data(), editable = TRUE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = F)
  })
  observeEvent(input$conduct_table_output_cell_edit, {
    conduct_info <- input$conduct_table_output_cell_edit
    conduct_modified_data <- conduct_reactive_table_data()
    conduct_modified_data[conduct_info$row, (conduct_info$col + 1)] <- as.numeric(conduct_info$value)
    conduct_reactive_table_data(conduct_modified_data)
  })


}



#shinyApp(ui, server_all)
