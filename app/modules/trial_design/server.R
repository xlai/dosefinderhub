questions <- dummy_data

column_names <- sprintf("d(%d)", 1:n_doses)

server_all <- function(input, output, session) {


  ######################################## Configuration tab's file upload/download ########################################

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

  output$config_save_button <- shiny::downloadHandler(
    filename = function() {
      paste("user_responses-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      inputs_to_save <- c((questions$trial)$q_variable, (questions$method)$q_variable, (questions$ranking)$q_variable)
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for (input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      write.csv(inputs_data_frame, file)
    }
  
  )


  ######################################## Configuration tab's simulation scenarios table code ########################################

  #Initialize empty data frame with specified columns
  reactive_table_data <- reactiveVal(data.frame(matrix(ncol = n_doses, nrow = 0, dimnames = list(NULL, column_names))))
  
  observe({
    #Capture current data
    current_data <- reactive_table_data()
    
    #Calculate rows to add or remove
    target_rows <- as.numeric(input$n_scenarios_input)
    current_rows <- nrow(current_data)
    rows_to_add <- target_rows - current_rows
    
    #Update data based on the difference
    if (rows_to_add > 0) {
      new_rows <- data.frame(
        Scenario = seq_len(rows_to_add) + current_rows,
        matrix(0, ncol = n_doses, nrow = rows_to_add, dimnames = list(NULL, column_names))
        )
      updated_data <- rbind(current_data, new_rows)
    } else {
      updated_data <- head(current_data, target_rows)
    }
    
    #Update reactive data frame
    reactive_table_data(updated_data)
  })
  
  output$editable_table <- renderDT({
    datatable(reactive_table_data(), editable = TRUE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"), list(targets = 0, className = "not-editable"))), rownames = FALSE)
      #scrollX = TRUE, scrollX="250px", paging = FALSE
    #options = list(scrollX = TRUE, scrollX="250px", paging = FALSE) #Did not work
  }, server = FALSE)
  
  output$table_output <- renderDT({
    datatable(reactive_table_data(), editable = TRUE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
      #scrollX = TRUE, scrollX="250px", paging = FALSE
    #options = list(scrollX = TRUE, scrollX="250px", paging = FALSE) #Did not work
  })
  
  observeEvent(input$table_output_cell_edit, {
    info <- input$table_output_cell_edit
    modified_data <- reactive_table_data()
    modified_data[info$row, (info$col + 1)] <- as.numeric(info$value)
    reactive_table_data(modified_data)
  })
  
  observeEvent(input$plot_button, {
    #Capture current data and transform for plotting
    current_data <- reactive_table_data()
    plot_data <- tidyr::gather(current_data[, -1, drop = FALSE], key = "Dose Level", value = "Value")
    plot_data$Scenario <- rep(current_data$Scenario, each = ncol(current_data) - 1)
    
    #Create the plot
    p <- ggplot(plot_data, aes(x = `Dose Level`, y = Value)) +
      geom_line() +
      geom_point() +
      labs(x = "Dose Levels",
           y = "'True' DLT Rates") +
      theme_minimal()
    
    # Render the plot
    output$plot <- renderPlot({
      p
    })
  })
  

  ######################################## Simulation tab server code ########################################

  new_n_scen <- reactive(as.numeric(input$n_scenarios_input))
  updated_scen_choices <- reactive(paste0("Scenario ", 1:new_n_scen()))
  
  output$scen_output_question <- renderUI({
    tagList(
      selectizeInput("scen_output_input", "Select scenarios", choices = updated_scen_choices(),
        multiple = TRUE, list(plugins = list('remove_button')))
    )
  })


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



shinyApp(ui, server_all)
