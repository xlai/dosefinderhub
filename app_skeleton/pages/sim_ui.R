sim_ui <- function(id) {
  ns <- NS(id)

  # Simulation-Specific Inputs
    n_sims_input <- numericInput(ns("n_sims_input"), "How many simulations would you like to run per design per scenario?", value = 10)
    n_scenarios_input <- numericInput(ns("n_scenarios_input"), "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)

  n_sims_warning_text <- textOutput(ns("sims_warning"))
  n_scenarios_warning_text <- textOutput(ns("scen_warning"))
  #table_output <- DT::DTOutput(ns("table_output")) # This is to test the table output used for the simulations tab.
  simulation_inputs <- tagList(
    n_sims_input,
    n_sims_warning_text,
    n_scenarios_input,
    n_scenarios_warning_text
    #table_output,
  )

  test_df_table <- DT::DTOutput(ns("test_df")) # The reactive table for the true DLT probabilities

  # Running the tab itself
  fluidPage(
  page_sidebar(
      div( 
        navset_tab(
          nav_panel(
            "Simulation Inputs",
            h3("Simulation Inputs"),
            p("Please fill out the following inputs before running the simulation."),
            tags$hr(),
            simulation_inputs,
            p("Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities 
            in the table below. If the dimensions do not match, change the number of scenarios and doses and press 
            'Refresh Dimensions'."),
            test_df_table,
            input_task_button(ns("refresh_table_input"), "Refresh Table Dimensions"),
            textOutput(ns("table_warning"))
            ),
          nav_panel(
            "Simulation Output - Tables",
            h3("Simulation Output - Tables"),
            tags$hr(),
            uiOutput(ns("tables_ui")), # Individual view dropdown
            textOutput(ns("titles1"), container = h4), # Comparative view - titles of table 1
            tableOutput(ns("tables1")), # Comparative view - table 1
            uiOutput(ns("buttons1")), # Comparative view - buttons for table 1
            tags$hr(),
            textOutput(ns("selected_ind_title"), container = h4), # Individual table titles
            tableOutput(ns("selected_ind_table")), # Individual table output
            textOutput(ns("titles2"), container = h4), # Comparative view - titles of table 2
            tableOutput(ns("tables2")), # Comparative view - table 2
            uiOutput(ns("buttons2")) # Comparative view - buttons for table 2
          ),
          nav_panel("Simulation Output - Plots",
          h3("Simulation Output - Plots"),
          uiOutput(ns("generate_graphs_ui")), # Graph selection
          tags$hr(),
          plotOutput(ns("selected_graph")), # Comparative view 
          )
        )
      ),

    sidebar = sidebar(
      h3("What Do You Want to Simulate?"),
            ################################ Simulation tab UI ################################
      # This code is copied from the Simulation tab UI in trial_design/ui.R
      selectizeInput(ns("simulation_design_selection_input"), "Select which designs' simulation outputs to see",
        choices = pretty_ranking,
        multiple = TRUE,
        options = list(plugins = list('remove_button'))),
      
      uiOutput(ns("scen_output_question")),
      
      selectizeInput(ns("metric_selection_input"), "Select outputs/metrics",
        choices = c("% participants treated at dose",
          "% times dose was selected as MTD",
          "Accuracy",
          "Duration",
          "Overdosing"),
        multiple = TRUE,
        list(plugins = list('remove_button'))),

        tags$hr(), # Separator line
  
        radioButtons(ns("comparative_view") , "How would you like to view the simulation results?",
          choices = c("Individually", "Comparatively by Design", "Comparatively by Scenario"), selected = "Individually", inline = TRUE),

      #selectizeInput("visual_selection_input", "Select type of output",
        #choices = c("Table", "Plot"),
        #multiple = TRUE,
        #list(plugins = list('remove_button')))

      ##### Run Simulation Button and Dowload Results Button #####
      tags$hr(), # Separator line
      h3("Run Simulation"),
      p("Please fill out the Simulation Inputs and click 'Run Simulation' to see the results."),
      input_task_button(ns("run_simulation"), "Run Simulation"),
      tags$hr(), # Separator line
      h3("Download Results"),
      p("Want to save your simulation results? Click a button below to download them as a CSV file."),
      downloadButton(ns("download_simulation_results"), "Download Simulation Results")
    )
  ) 
  )
}

sim_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
ns <- session$ns
     ######################################## Configuration tab's simulation scenarios table code ########################################

  #### Simulation Variables from Configurations Tab
  n_sims <- reactive({as.numeric(input$n_sims_input)})
  n_scenarios <- reactive({as.numeric(input$n_scenarios_input)})

  ############## Reactive True DLT Probabilities Table ##############
 
  scen_1_init <- c(1, example_scenarios(0.3, 3, 5))
  scen_2_init <- c(2, example_scenarios(0.3, 2, 5))
  scen_3_init <- c(3, example_scenarios(0.3, 4, 5))
  matrix <- rbind(scen_1_init, scen_2_init, scen_3_init)
  colnames(matrix) <- list("Scenario", "d1", "d2", "d3", "d4", "d5")
  matrix_df <- as.data.frame(matrix)

  reactive_df <- reactiveVal(matrix_df) # initalising a reactive value to store the data frame

  observeEvent({input$refresh_table_input}, {
   doses <- shared$n_dosess()
   scen <- n_scenarios()
  if (doses < 1 || floor(doses) != doses) {
    showNotification("Please set a valid number of doses in the Trial Design tab before refreshing the table.", type = "error")
    return(NULL)
  } else if (scen < 1 ||  floor(scen) != scen || scen > 3) {
    showNotification("Please select a number of Scenarios between 1 and 3 in the Simulation Inputs before refreshing the table.", type = "error")
    return(NULL)
  } else {

  ex_scen_1 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm(), shared$n_dosess())
  ex_scen_2 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm() - 1, shared$n_dosess())
  ex_scen_3 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm() + 1, shared$n_dosess())

  if (n_scenarios() == 1) {
    dimensions <- t(ex_scen_1)
  } else if (n_scenarios() == 2) {
    dimensions <- rbind(ex_scen_1, ex_scen_2)
  } else if (n_scenarios() == 3) {
    dimensions <- rbind(ex_scen_1, ex_scen_2, ex_scen_3)
  }

  dataframe <- data.frame(dimensions) # What was previously doses_table
  colnames(dataframe) <- paste("d", 1:shared$n_dosess(), sep = "")

  Scenario <- matrix(as.numeric(1:n_scenarios()), nrow = n_scenarios(), ncol = 1)

  dataframe_row_1 <- data.frame(Scenario) # What was previously scenarios_table

  cbind <- cbind(dataframe_row_1, dataframe)
  reactive_df(cbind) # Updating the reactive value with the new data frame
  } # end of else
  })
  
  
  output$test_df <- renderDT({
    datatable(reactive_df(), editable = TRUE, rownames = FALSE, options = list(scrollX = TRUE)) 
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

  ######################################## Simulation Tab's Server Code ######################################

  ##### Scenarios Question ######
   new_n_scen <- reactive(as.numeric(input$n_scenarios_input))
  updated_scen_choices <- reactive(paste0("Scenario ", 1:new_n_scen()))

  output$scen_output_question <- renderUI({
    tagList(
      selectizeInput(ns("scen_output_input"), "Select scenarios", choices = updated_scen_choices(),
        multiple = TRUE, list(plugins = list('remove_button')))
    )
  })

  ######## Validation ########

## n_sims and n_scenarios

validation_state <- reactiveValues(
    sim_val = NULL,
    scen_val = NULL,
    table_val = NULL,
  )
  
  # Define base validation rules for each input
    base_validation_rules <- list(
    sim_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    scen_val = list(min_val = 1, max_val = 3, integer_only = TRUE)
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

  observe({
    update_validation("sim_val", input$n_sims_input)
  })

  observe({
    update_validation("scen_val", input$n_scenarios_input)
  })
  
  
  # Render individual warning messages next to each input
  output$sims_warning <- renderText({
    validation_state$sim_val_warning %||% ""
  })

   output$scen_warning <- renderText({
    validation_state$scen_val_warning %||% ""
  })

  # Function to validate table data
  validate_table_data <- function() {
    data <- reactive_df()
    
    if (any(is.na(data))) {
      return("⚠️ Please ensure all cells in the True DLT probabilities table are filled out before running the simulation.")
    } else if (any(data[, -1] < 0 | data[, -1] > 1)) {
      return("⚠️ Please ensure all True DLT probabilities are between 0 and 1.")
    } else {
      incr_val <- rep(FALSE, nrow(data))
      for (i in 1:nrow(data)) {
        if (is.unsorted(data[i, -1])) {
          incr_val[i] <- TRUE
        } 
      }
      if (any(incr_val)) {
        return("⚠️ Please ensure all rows of the table contain an increasing sequence of true DLT probabilities.")
      } else {
        return(NULL)
      }
    }
  }
  
  # Update table validation function
  update_table_validation <- function() {
    error_msg <- validate_table_data()
    validation_state$table_val <- error_msg
    
    if (is.null(error_msg)) {
      output$table_warning <- renderText({NULL})
    } else {
      output$table_warning <- renderText({error_msg})
    }
  }
  
  ## The reactive table - validate on cell edit
  observeEvent(input$test_df_cell_edit, {
    update_table_validation()
  })
  
  ## Also validate when table is refreshed
  observeEvent(reactive_df(), {
    update_table_validation()
  })

 ######################################### Simulation Outputs #########################################
 # Code copied directly from the trial_design tab's server code
 # Simulation outputs

 sim_df <- reactiveVal(NULL) # initialising
 sim_titles <- reactiveVal(NULL) # initialising
 sim_graphs <- reactiveVal(NULL) # initialising

  observeEvent(input$run_simulation, {

    validation_errors <- sapply(names(validation_state), function(x) validation_state[[x]])
    validation_errors <- validation_errors[!grepl("warning", names(validation_errors))]
    validation_errors <- validation_errors[!grepl("basic", names(validation_errors))]
    validation_errors <- Filter(Negate(is.null), validation_errors)

  if (shared$td_warnings > 0) {
    showNotification(
      "Please resolve the warnings in the Trial Design tab before running the simulation.",
      type = "error",
      duration = 5
    )
    return(NULL)
  } else if (length(validation_errors) > 0) {
    showNotification(
      "Please resolve the warnings in the Simulation inputs before running the simulation.",
      type = "error",
      duration = 5
    )
    return(NULL)
    } else if (!is.null(validation_state$table_val)) {
    showNotification(
      "Please resolve the warnings in the True DLT probabilities table before running the simulation.",
      type = "error",
      duration = 5
    )
    return(NULL)
    } else if (shared$n_dosess() != ncol(true_dlts()) || n_scenarios() > nrow(true_dlts())) {
    showNotification(
      "Please click Refresh Table Dimensions and fill in the True DLT probabilities table before running the simulation.",
      type = "error",
      duration = 5
    )
  } else {
    # Adding in Scenarios. I am going to cap the possible number of Scenarios to 3 (this can be changed later).

  scenarios <- c("Scenario 1", "Scenario 2", "Scenario 3") 
  selected_scenarios <- cbind(
    scen1 <- {"Scenario 1" %in% input$scen_output_input},
    scen2 <- {"Scenario 2" %in% input$scen_output_input},
    scen3 <- {"Scenario 3" %in% input$scen_output_input}
  )
  updated_scenarios <- scenarios[!sapply(selected_scenarios, identical, FALSE)] 

  model <- c("3+3", "CRM", "BOIN")
    selected_models <- c(("3+3" %in% input$simulation_design_selection_input),
                        ("CRM" %in% input$simulation_design_selection_input),
                        ("BOIN" %in% input$simulation_design_selection_input))
    n_models <- sum(selected_models)

  used_true_dlts <- true_dlts()[selected_scenarios, ] # Scenarios are rows!
  
  n_scen <- sum(selected_scenarios)
 
  combined_list <- vector("list", n_scen) # initialising for use later
  title_list <- vector("list", n_scen) # initialising for use later
  plot_list <- vector("list", n_scen) # initialising for use later
  plot_tpt <- vector("list", n_scen) # initialising for use later
  plot_crm <- vector("list", n_scen) # initialising for use later
  plot_boin <- vector("list", n_scen) # initialising for use later
  median_accuracy <- vector("list", n_scen) # initialising for use later
  median_overdose <- vector("list", n_scen) # initialising for use later
  median_length <- vector("list", n_scen) # initialising for use later

   # Metric - putting it outside of the for loop so only one list is created.
  selected_metric <- cbind(
  selected_mtd <- {"% times dose was selected as MTD" %in% input$metric_selection_input},
  selected_participant <- {"% participants treated at dose" %in% input$metric_selection_input},
  selected_accuracy <- {"Accuracy" %in% input$metric_selection_input},
  selected_overdose <- {"Overdosing" %in% input$metric_selection_input},
  selected_duration <- {"Duration" %in% input$metric_selection_input})
 
  if (n_scen == 0) {
    tables_ui <- NULL
  } else {
    # Use the new utility function to process all simulations
    simulation_results <- process_multiple_simulations(
      selected_methods = model[selected_models],
      selected_scenarios = selected_scenarios,
      scenarios = scenarios,
      shared = shared,
      n_sims = n_sims(),
      true_dlts = used_true_dlts,
      selected_metric = selected_metric
    )
    
    combined_list <- simulation_results$combined_list
    title_list <- simulation_results$title_list
    plot_list <- simulation_results$plot_list
    median_overdose <- simulation_results$median_overdose
    median_length <- simulation_results$median_length

  } # end of simulation processing

## Tables
  combined_data_frames <- do.call(c, combined_list) 
  combined_titles <- do.call(c, title_list) 

  n_data_frames <- length(combined_data_frames)
 
  # Using generic table names to render the UI with all the tables in it.
  if (n_data_frames == 0) { # The case where nothing is entered
    output$tables_ui <- NULL
    output$selected_ind_table <- NULL
    output$selected_ind_title <- NULL
    output$titles1 <- NULL
    output$titles2 <- NULL
    output$tables1 <- NULL
    output$tables2 <- NULL
    output$buttons1 <- NULL
    output$buttons2 <- NULL
  } else {

  if ("Individually" %in% input$comparative_view | n_data_frames == 1) {
    output$tables_ui <- renderUI({selectInput(
      ns("ind_tables"), "Select a table to view",
      choices = combined_titles, selected = combined_titles[1], multiple = FALSE, width = "100%"
    )})

    output$titles1 <- NULL
    output$titles2 <- NULL
    output$tables1 <- NULL
    output$tables2 <- NULL
    output$buttons1 <- NULL
    output$buttons2 <- NULL

   sim_df <- sim_df(combined_data_frames) 
   sim_titles <- sim_titles(combined_titles)

  } else {output$tables_ui <- NULL
  output$selected_ind_table <- NULL
  output$selected_ind_title <- NULL

  output$titles1 <- renderText({combined_titles[[1]]})
  output$titles2 <- renderText({combined_titles[[2]]})
  output$tables1 <- renderTable({combined_data_frames[[1]]}, rownames = TRUE, colnames = TRUE)
  output$tables2 <- renderTable({combined_data_frames[[2]]}, rownames = TRUE, colnames = TRUE)

  output$buttons1 <- renderUI({
    if (n_data_frames > 0) {
      tagList(
        actionButton(ns("prev1"), "Previous", class = "btn btn-secondary"),
        actionButton(ns("next1"), "Next", class = "btn btn-secondary")
      )
    }
  })
  output$buttons2 <- renderUI({
    if (n_data_frames > 1) {
      tagList(
        actionButton(ns("prev2"), "Previous", class = "btn btn-secondary"),
        actionButton(ns("next2"), "Next", class = "btn btn-secondary")
      )
    }
  })

  sim_df <- sim_df(combined_data_frames) 
  sim_titles <- sim_titles(combined_titles) # Updating the reactive values with the new data frames and titles
  }
  ########################## Plots #####################################
  
  # Use the new plotting utility function
  plot_results <- generate_simulation_plots(
    view_type = input$comparative_view,
    plot_data = plot_list,
    selected_metric = selected_metric,
    selected_scenarios = selected_scenarios,
    selected_models = selected_models,
    scenarios = scenarios,
    models = model,
    median_overdose = median_overdose,
    median_length = median_length,
    ns = ns
  )
  
  # Update reactive values and UI
  sim_graphs(plot_results$filtered_graphs)
  output$generate_graphs_ui <- plot_results$ui_element

  } # else (after for loop)
  } # else (for validation)
  }) # observe function

 current_table1 <- reactiveVal(1)
 current_table2 <- reactiveVal(2)

  observeEvent(input$next1, {
    if (current_table1() < length(sim_df())) {
      current_table1(current_table1() + 1)
      output$tables1 <- renderTable({
        sim_df()[[current_table1()]]
      }, rownames = TRUE, colnames = TRUE)
      output$titles1 <- renderText({
        sim_titles()[[current_table1()]]
      })
    } 
  })
  observeEvent(input$prev1, {
    if (current_table1() > 1) {
      current_table1(current_table1() - 1)
      output$tables1 <- renderTable({
        sim_df()[[current_table1()]]
      }, rownames = TRUE, colnames = TRUE)
      output$titles1 <- renderText({
        sim_titles()[[current_table1()]]
      })
    } 
  })
  observeEvent(input$next2, {
    if (current_table2() < length(sim_df())) {
      current_table2(current_table2() + 1)
      output$tables2 <- renderTable({
        sim_df()[[current_table2()]]
      }, rownames = TRUE, colnames = TRUE)
      output$titles2 <- renderText({
        sim_titles()[[current_table2()]]
      })
    } 
  })
  observeEvent(input$prev2, {
    if (current_table2() > 1) {
      current_table2(current_table2() - 1)
      output$tables2 <- renderTable({
        sim_df()[[current_table2()]]
      }, rownames = TRUE, colnames = TRUE)
      output$titles2 <- renderText({
        sim_titles()[[current_table2()]]
      })
    } 
  })

  # Individual table outputs
  observeEvent(input$ind_tables, {
    selected_table <- input$ind_tables
    table_index <- which(sim_titles() == selected_table)
  
    output$selected_ind_table <- renderTable({
      sim_df()[[table_index]]
    }, rownames = TRUE, colnames = TRUE)
    
    output$selected_ind_title <- renderText({
      selected_table
    })
  })

  # Comparative plots outputs
  observeEvent(input$m_graph, {
    selected_plot <- input$m_graph
    
    output$selected_graph <- renderPlot({
      sim_graphs()[[selected_plot]]
    })
  })

  observeEvent(input$s_graph, {
    selected_plot <- input$s_graph
    
    output$selected_graph <- renderPlot({
      sim_graphs()[[selected_plot]]
    })
  })

  observeEvent(input$ind_graph, {
    selected_plot <- input$ind_graph

    output$selected_graph <- renderPlot({
      sim_graphs()[[selected_plot]]
    })
  })




  }) # End of moduleServer
} # End of sever function