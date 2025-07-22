

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

  reactive_df <- reactiveVal() # initalising a reactive value to store the data frame

  observeEvent({input$n_scenarios_input; input$n_doses_inputt}, {
   
  dimensions <- matrix(0, nrow = input$n_scenarios_input, ncol = input$n_doses_inputt)
  colnames(dimensions) <- paste("d", 1:input$n_doses_inputt, sep = "")
  dataframe <- data.frame(dimensions) # What was previously doses_table

  Scenario <- matrix(1:input$n_scenarios_input, nrow = input$n_scenarios_input, ncol = 1)
  
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
  })

  true_dlts <- reactive({
    reactive_df()[, -1] # Exclude the first column (Scenario)
  })
  

  output$table_output <- DT::renderDT({
    datatable(true_dlts(), editable = TRUE, rownames = FALSE, options = list(scrollX = TRUE, scrollY = "250px", paging = FALSE))
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

  observeEvent(input$submit, {

    # Adding in Scenarios. I am going to cap the possible number of Scenarios to 3 (this can be changed later).

  selected_scenarios <- cbind(
    scen1 <- {"Scenario 1" %in% input$scen_output_input},
    scen2 <- {"Scenario 2" %in% input$scen_output_input},
    scen3 <- {"Scenario 3" %in% input$scen_output_input}
  )
  #print(true_dlts())
  used_true_dlts <- true_dlts()[selected_scenarios, ] # Scenarios are rows!
  #print(used_true_dlts)
  n_scen <- nrow(used_true_dlts)
  #print(n_scen)
  combined_list <- vector("list", n_scen) # initialising for use later

   # Metric - putting it outside of the for loop so only one list is created.
  selected_metric <- cbind(
  selected_participant <- {"% participants treated at dose" %in% input$metric_selection_input},
  selected_mtd <- {"% times dose was selected as MTD" %in% input$metric_selection_input},
  selected_accuracy <- {"Accuracy" %in% input$metric_selection_input},
  selected_duration <- {"Duration" %in% input$metric_selection_input},
  selected_overdose <- {"Overdosing" %in% input$metric_selection_input})

  if (n_scen == 0) {tables_ui <- NULL} else { for (j in 1:n_scen) {

  # Design - only running simulations that are necessary to save time.
  if ("3+3" %in% input$simulation_design_selection_input)
      { print(unlist(used_true_dlts[j, ]))
        tpt_sim <- sim_tpt(n_dosess(), ttl(), max_size(), start_dose(), n_sims(), unlist(used_true_dlts[j, ]), 12345)
       tpt_modified_tab <- tpt_sim[-c(3,5,7)]
      } else {tpt_modified_tab <- NULL}
  if ("CRM" %in% input$simulation_design_selection_input)
      {crm_sim <- sim_crm(3, 0.3, 10, 1, 10, c(0,1,0.2,0.3), c(0.11, 0.22, 0.56), 0.1, FALSE, FALSE, 0.11, 0.06, 45)
       crm_modified_tab <- crm_sim[-c(3,5,7)]
      } else {crm_modified_tab <- NULL}

  tpt_to_display <- tpt_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  tpt_data_frames <- lapply(tpt_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes
  
  crm_to_display <- crm_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  crm_data_frames <- lapply(crm_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes

  combined_list[[j]]  <- cbind(tpt_data_frames, crm_data_frames)
  
  } # for loop end

  combined_data_frames <- do.call(c, combined_list) 
  n_data_frames <- length(combined_data_frames)

  if (n_data_frames == 0) {output$tables_ui <- NULL  # The case where nothing is entered
  } else {
  table_names <- c(paste(rep("Table", n_data_frames), as.list(as.character(1:n_data_frames)), sep = " "))
  names(combined_data_frames) <- table_names

  ## Using the names of the tables to render a UI with all the tables in it.
   output$tables_ui <- renderUI({
    lapply(names(combined_data_frames), function(table_name) {
      tagList(
        h3(table_name), # Table title
        tableOutput(outputId = paste0("table_", table_name)) # Table output
      )
    })
  })
  
  # Rendering each table
  lapply(names(combined_data_frames), function(table_name) {
    output[[paste0("table_", table_name)]] <- renderTable({
      combined_data_frames[[table_name]]
    }) 
  }) # lapply
  } # else (after for loop)
  } # else (before for loop)
  }) # observe function
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
