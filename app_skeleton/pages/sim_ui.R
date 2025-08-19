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
            textOutput(ns("selected_ind_title"), container = h4), # Individual table titles
            tableOutput(ns("selected_ind_table")), # Individual table output
            textOutput(ns("selected_title"), container = h3), # Scenario/Model title (Comparative view)
            textOutput(ns("true_dlts_title"), container = h4), # True DLT probabilities table title
            uiOutput(ns("true_dlts_table")), # True DLT probabilities table (Comparison by model)
            tags$hr(),
            textOutput(ns("treatment_title"), container = h4),
            tableOutput(ns("treatment_table")), # Comparative view - treatment
            textOutput(ns("mtd_title"), container = h4), 
            tableOutput(ns("mtd_table")), # Comparative view - mtd
            textOutput(ns("mean_title"), container = h4), 
            tableOutput(ns("mean_table")) # Comparative view - mean values

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
 scenario_list <- reactiveVal(NULL) # initialising


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
  scenario_list <- scenario_list(updated_scenarios)

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
 
  if (n_scen == 0) {tables_ui <- NULL} else { for (j in 1:n_scen) {

  # Design - only running simulations that are necessary to save time.
  if ("3+3" %in% input$simulation_design_selection_input)
      { tpt_sim <- sim_tpt(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), 12345)
       tpt_modified_tab <- tpt_sim[-c(4,6)]

      tpt_modified_tab$mean_accuracy <- as.data.frame(tpt_modified_tab$mean_accuracy, row.names = "Mean Accuracy")
      tpt_modified_tab$mean_overdose <- as.data.frame(tpt_modified_tab$mean_overdose, row.names = "Mean Overdose")
      tpt_modified_tab$mean_length <- as.data.frame(tpt_modified_tab$mean_length, row.names = "Mean Trial Length")
      colnames(tpt_modified_tab$mean_accuracy) <- ""
      colnames(tpt_modified_tab$mean_length)<- ""
      colnames(tpt_modified_tab$mean_overdose) <- ""

      tpt_mean_accuracy <- tpt_sim$mean_accuracy
      tpt_mean_overdose <- tpt_sim$mean_overdose
      tpt_mean_length <- tpt_sim$mean_length

      tpt_median_overdose <- median(tpt_sim$dist_overdose)
      tpt_median_length <- median(tpt_sim$dist_length)

      tpt_for_plots <- data_for_plotting(tpt_sim, shared$ttl())
      } else {tpt_modified_tab <- NULL
      tpt_for_plots <- rep(list(NULL), 5)
      tpt_mean_accuracy <- NULL
      tpt_mean_overdose <- NULL
      tpt_mean_length <- NULL
      tpt_median_overdose <- NULL
      tpt_median_length <- NULL
      }

  if ("CRM" %in% input$simulation_design_selection_input)
      { crm_sim <- sim_crm(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), shared$skeleton_crm(), shared$prior_var_crm(), shared$skip_esc_crm(), shared$skip_deesc_crm(), shared$stop_tox_x_crm(), shared$stop_tox_y_crm(), shared$stop_n_mtd_crm())
      crm_modified_tab <- crm_sim[-c(4,6)]
  

      crm_modified_tab$mean_accuracy <- as.data.frame(crm_modified_tab$mean_accuracy, row.names = "Mean Accuracy", col.names = FALSE)
      crm_modified_tab$mean_overdose <- as.data.frame(crm_modified_tab$mean_overdose, row.names = "Mean Overdose", col.names = FALSE)
      crm_modified_tab$mean_length <- as.data.frame(crm_modified_tab$mean_length, row.names = "Mean Trial Length", col.names = FALSE)
      colnames(crm_modified_tab$mean_accuracy) <- ""
      colnames(crm_modified_tab$mean_length)<- ""
      colnames(crm_modified_tab$mean_overdose) <- ""

      crm_mean_accuracy <- crm_sim$mean_accuracy 
      crm_mean_overdose <- crm_sim$mean_overdose
      crm_mean_length <- crm_sim$mean_length

      crm_median_overdose <- median(crm_sim$dist_overdose)
      crm_median_length <- median(crm_sim$dist_length)

      crm_for_plots <- data_for_plotting(crm_sim, shared$ttl())
      } else {crm_modified_tab <- NULL
      crm_for_plots <- rep(list(NULL), 5)
      crm_mean_accuracy <- NULL
      crm_mean_overdose <- NULL
      crm_mean_length <- NULL
      crm_median_overdose <- NULL
      crm_median_length <- NULL
      }

   if ("BOIN" %in% input$simulation_design_selection_input)
      { boin_sim <- sim_boin(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), shared$boin_cohorts(), shared$stop_n_mtd_boin(), p_tox =  shared$phi_2, p_saf =  shared$phi_1, TRUE, 10) # testing with hard-coded values for now
       boin_modified_tab <- boin_sim[-c(4,6)]

      boin_modified_tab$mean_accuracy <- as.data.frame(boin_modified_tab$mean_accuracy, row.names = "Mean Accuracy")
      boin_modified_tab$mean_overdose <- as.data.frame(boin_modified_tab$mean_overdose, row.names = "Mean Overdose")
      boin_modified_tab$mean_length <- as.data.frame(boin_modified_tab$mean_length, row.names = "Mean Trial Length")
      colnames(boin_modified_tab$mean_accuracy) <- ""
      colnames(boin_modified_tab$mean_length)<- ""
      colnames(boin_modified_tab$mean_overdose) <- ""

      boin_mean_accuracy <- boin_sim$mean_accuracy
      boin_mean_overdose <- boin_sim$mean_overdose
      boin_mean_length <- boin_sim$mean_length

      boin_median_overdose <- median(boin_sim$dist_overdose)
      boin_median_length <- median(boin_sim$dist_length)

      boin_for_plots <- data_for_plotting(boin_sim, shared$ttl())
      } else {boin_modified_tab <- NULL
      boin_for_plots <- rep(list(NULL), 5)
      boin_mean_accuracy <- NULL
      boin_mean_overdose <- NULL
      boin_mean_length <- NULL
      boin_median_overdose <- NULL
      boin_median_length <- NULL
      }

  # Giving Titles to Single Value Outputs

  tpt_to_display <- tpt_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  crm_to_display <- crm_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  boin_to_display <- boin_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display 

  tpt_title <- as.character(rep("3+3 Simulation for ", 5))
  crm_title <- as.character(rep("CRM Simulation for ", 5))
  boin_title <- as.character(rep("BOIN Simulation for ", 5))
  scenario_number <- as.character(rep(updated_scenarios[[j]], 5))
  metric_names <- as.character(c(" - % Times Dose Was Selected as MTD", "- % Treated at Each Dose",  " - Mean Accuracy", " - Mean Overdose", " - Mean Trial Length"))

  sim_list <- vector("list", 4) # initialising for use later

  # Organising plot lists by metric
  for (k in 1:4) {
    sim_list[[k]] <- list(tpt_for_plots[[k]], crm_for_plots[[k]], boin_for_plots[[k]])
  }

  if ("3+3" %in% input$simulation_design_selection_input) {
  full_tpt_titles <- paste(as.character(tpt_title), as.character(scenario_number), as.character(metric_names))
  } else {full_tpt_titles <- NULL}

  if("CRM" %in% input$simulation_design_selection_input) {
  full_crm_titles <- paste(as.character(crm_title), as.character(scenario_number), as.character(metric_names))
  } else {full_crm_titles <- NULL}

   if("BOIN" %in% input$simulation_design_selection_input) {
  full_boin_titles <- paste(as.character(boin_title), as.character(scenario_number), as.character(metric_names))
  } else {full_boin_titles <- NULL}

 
  used_tpt_titles <- full_tpt_titles[c(which(selected_metric == TRUE))] # A list of titles we want to display
  used_crm_titles <- full_crm_titles[c(which(selected_metric == TRUE))] # A list of titles we want to display
  used_boin_titles <- full_boin_titles[c(which(selected_metric == TRUE))] # A list of titles we want to display

  tpt_data_frames <- lapply(tpt_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes
  crm_data_frames <- lapply(crm_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes
  boin_data_frames <- lapply(boin_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes

  combined_list[[j]]  <- cbind(tpt_data_frames, crm_data_frames, boin_data_frames)
  cbind_titles <- cbind(used_tpt_titles, used_crm_titles, used_boin_titles)
  title_list[[j]] <- unname(unlist(cbind_titles))
  plot_list[[j]] <- sim_list

  plot_tpt[[j]] <- tpt_for_plots
  plot_crm[[j]] <- crm_for_plots
  plot_boin[[j]] <- boin_for_plots

  #mean_accuracy[[j]] <- c(tpt_mean_accuracy, crm_mean_accuracy, boin_mean_accuracy)
  #mean_overdose[[j]] <- c(tpt_mean_overdose, crm_mean_overdose, boin_mean_overdose)
  #mean_length[[j]] <- c(tpt_mean_length, crm_mean_length, boin_mean_length)
  median_overdose[[j]] <- c(tpt_median_overdose, crm_median_overdose, boin_median_overdose)
  median_length[[j]] <- c(tpt_median_length, crm_median_length, boin_median_length)
  } # for loop end

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
  } else {

  if ("Individually" %in% input$comparative_view | n_data_frames == 1) {
    output$selected_title <- NULL
    output$true_dlts_title <- NULL
    output$treatment_title <- NULL
    output$mtd_title <- NULL
    output$mean_title <- NULL
    output$true_dlts_table <- NULL
    output$treatment_table <- NULL
    output$mtd_table <- NULL
    output$mean_table <- NULL

    output$tables_ui <- renderUI({selectInput(
      ns("ind_tables"), "Select a table to view",
      choices = combined_titles, selected = combined_titles[1], multiple = FALSE, width = "100%"
    )})

   sim_df <- sim_df(combined_data_frames) 
   sim_titles <- sim_titles(combined_titles)

  } else if ("Comparatively by Design" %in% input$comparative_view) {
      output$tables_ui <- renderUI({selectInput(
      ns("scenario_tables"), "Select a Scenario to view",
      choices = updated_scenarios, selected = updated_scenarios[1], multiple = FALSE, width = "100%"
    )})

  output$selected_ind_table <- NULL
  output$selected_ind_title <- NULL

  names(combined_data_frames) <- combined_titles # Naming the data frames with the titles

  sim_df <- sim_df(combined_data_frames) 
  sim_titles <- sim_titles(combined_titles) # Updating the reactive values with the new data frames and titles
  } else {
    output$tables_ui <- renderUI({selectInput(
      ns("model_tables"), "Select a Design to view",
      choices = updated_model, selected = updated_model[1], multiple = FALSE, width = "100%"
    )})

  output$selected_ind_table <- NULL
  output$selected_ind_title <- NULL

    scen_numbers <- as.numeric(gsub("Scenario ", "", scenario_list())) # Extracting the scenario number from the selected scenario string

    true_dlts_wide <- reactive_df()[scen_numbers, ] 
    true_dlts <- as.data.frame(true_dlts_wide[, -1]) 
    row.names(true_dlts) <- paste("Scenario ", as.integer(true_dlts_wide$Scenario))


    output$true_dlts_title <- renderText({"True DLT Probabilities Per Scenario"})
    output$true_dlts_table <- renderTable({
      true_dlts
    }, rownames = TRUE, colnames = TRUE)

  names(combined_data_frames) <- combined_titles # Naming the data frames with the titles

  sim_df <- sim_df(combined_data_frames) 
  sim_titles <- sim_titles(combined_titles) # Updating the reactive values with the new data frames and titles
  }
  ########################## Plots #####################################

   
  metric_no_accuracy <- selected_metric[-3] # Removing accuracy from the list of selected metrics
  if  ("Individually" %in% input$comparative_view) {
    graphs <- vector("list", 4*n_scen*n_models) # initialising for use later
     updated_model <- model[!sapply(selected_models, identical, FALSE)] 

    for (j in 1:n_scen) {
      data_scen <- plot_list[[j]]
 

      data_mod <- plot_by_scenario(data_scen, 4, 4, n_models) # Reordering data such that we have scenario, model, metric.
      
      used_data <- data_mod[!sapply(data_mod, identical, FALSE)] 

      for (i in 1:n_models) {
        data <- used_data[[i]]

        mo <- median_overdose[[j]][i]
        ml <- median_length[[j]][i]
      
       for (k in 1:4) {
        met <- as.data.frame(data[[k]])

       if (metric_no_accuracy[k] == FALSE) { next
      } else if (!is.null(met$selection)) {
      graphs[[4*n_models*(j-1) + 4*(i-1) + k]] <- plot_bar_ind(met, Dose, selection, title = paste("% Times Dose Was Selected as MTD for", updated_model[[i]], updated_scenarios[[j]]), y_title = "% Times Dose Was Selected as MTD", col = "blue") # Using blue for MTD
      } else if (!is.null(met$treatment)) {
      graphs[[4*n_models*(j-1) + 4*(i-1) + k]] <- plot_bar_ind(met, Dose_Level, treatment, title = paste("% Treated at Dose for", updated_model[[i]], updated_scenarios[[j]]), y_title = "% Treated at Dose", col = "blue") # Using blue for MTD
      } else if (!is.null(met$overdose)) {
      graphs[[4*n_models*(j-1) + 4*(i-1) + k]] <- plot_dist_ind(met, overdose, mo, title = paste("Distribution of Overdoses for", updated_model[[i]], updated_scenarios[[j]]), x_title = "Overdose", col = "blue") # Using blue for median
      } else if (!is.null(met$length)) {
      graphs[[4*n_models*(j-1) + 4*(i-1) + k]] <- plot_dist_ind(met, length, ml, title = paste("Distribution of Trial Duration for", updated_model[[i]], updated_scenarios[[j]]), x_title = "Trial Duration", col = "blue") # Using blue for median
      } else {
        graphs[[4*n_models*(j-1) + 4*(i-1) + k]] <- NULL
      }
    }
      }
    }

     # Removing NULL values from the graphs list
  filtered_graphs <- Filter(Negate(is.null), graphs)


  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)


  output$generate_graphs_ui <- renderUI({selectInput(
      ns("ind_graph"), "Select a plot to view",
      choices = names(filtered_graphs), selected = names(filtered_graphs)[1], multiple = FALSE, width = "100%"
    )})

   sim_graphs <- sim_graphs(filtered_graphs) # Updating the reactive value with the new plots

  } else if ("Comparatively by Design" %in% input$comparative_view) { # Focusing on "by model"
  graphs <- vector("list", 4*n_scen*n_models) # initialising for use later

   for (j in 1:n_scen) {
    data <- plot_list[[j]]   #plot_list[[j]][[k]] = Scenario j, Metric k.
    mo <- median_overdose[[j]]
    ml <- median_length[[j]]

    for (k in 1:4) {
      met <- data[[k]]

      if(is.null(met)) 
      { next } else if (metric_no_accuracy[k] == FALSE) { next
      } else if (!is.null(met[[1]]$selection) | !is.null(met[[2]]$selection) | !is.null(met[[3]]$selection)) {
      graphs[[4*(j-1) + k]] <- plot_bar(met, Dose, selection, title = paste("% Times Dose Was Selected as MTD for", updated_scenarios[[j]]), y_title = "% Times Dose Was Selected as MTD", col = "blue", model_picked = 1, models = selected_models, scenarios = selected_scenarios) # Using blue for MTD
      } else if (!is.null(met[[1]]$treatment) | !is.null(met[[2]]$treatment) | !is.null(met[[3]]$treatment)) {
      graphs[[4*(j-1) + k]] <- plot_bar(met, Dose_Level, treatment, title = paste("% Treated at Dose for", updated_scenarios[[j]]), y_title = "% Treated at Dose", col = "blue", model_picked = 1, models = selected_models, scenarios = selected_scenarios) # Using blue for MTD
      } else if (!is.null(met[[1]]$overdose) | !is.null(met[[2]]$overdose) | !is.null(met[[3]]$overdose)) {
      graphs[[4*(j-1) + k]] <- plot_dist(met, overdose, mo, title = paste("Distribution of Overdoses for", updated_scenarios[[j]]), x_title = "Overdose", col = "blue", model_picked = 1, models = selected_models, scenarios = selected_scenarios) # Using blue for mean
      } else if (!is.null(met[[1]]$length) | !is.null(met[[2]]$length) | !is.null(met[[3]]$length)) {
      graphs[[4*(j-1) + k]] <- plot_dist(met, length, ml, title = paste("Distribution of Trial Duration for", updated_scenarios[[j]]), x_title = "Trial Duration", col = "blue", model_picked = 1, models = selected_models, scenarios = selected_scenarios) # Using blue for mean
      } else {
        graphs[[4*(j-1) + k]] <- NULL
      } 
    }
   }

  # Removing NULL values from the graphs list
  filtered_graphs <- Filter(Negate(is.null), graphs)

  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)

  output$generate_graphs_ui <- renderUI({selectInput(
      ns("m_graph"), "Select a plot to view",
      choices = names(filtered_graphs), selected = names(filtered_graphs)[1], multiple = FALSE, width = "100%"
    )})

   sim_graphs <- sim_graphs(filtered_graphs) # Updating the reactive value with the new plots

  } else if ("Comparatively by Scenario" %in% input$comparative_view) {
    # Focusing on "by scenario"
  # Adding NULLs where necessary

  plot_tpt_full <- vector("list", length = 3)
  plot_crm_full <- vector("list", length = 3)
  plot_boin_full <- vector("list", length = 3)

   y <- 1
   z <- 1
   while(y < 4) {
    if (selected_scenarios[y] == TRUE) {
      plot_tpt_full[[y]] <- plot_tpt[[z]]
      plot_crm_full[[y]] <- plot_crm[[z]]
      plot_boin_full[[y]] <- plot_boin[[z]]
      y <- y+1
      z <- z+1
    } else {
      plot_tpt_full[[y]] <- rep(list(NULL), 5)
      plot_crm_full[[y]] <- rep(list(NULL), 5)
      plot_boin_full[[y]] <- rep(list(NULL), 5)
      y <- y+1
      z <- z
    }
  }

  # Combining data for plotting
 
  tpt_by_scenario <- plot_by_scenario(plot_tpt_full, 5, 3, 4)
  crm_by_scenario <- plot_by_scenario(plot_crm_full, 5, 3, 4)
  boin_by_scenario <- plot_by_scenario(plot_boin_full, 5, 3, 4)

  plot_list_by_scenario <- list(tpt_by_scenario, crm_by_scenario, boin_by_scenario)

  median_ov_scen <- median_for_scen(median_overdose)
  median_len_scen <- median_for_scen(median_length)

    updated_model <- model[!sapply(selected_models, identical, FALSE)] 

    graphs <- vector("list", 4*n_models) # initialising for use later

    used_plots <- plot_list_by_scenario[!sapply(selected_models, identical, FALSE)] 

   for (j in 1:n_models) {
    data <- used_plots[[j]] 
  
    mo <- median_ov_scen[[j]]
    ml <- median_len_scen[[j]]

    for (k in 1:4) {
      met <- data[[k]]
      
      if(is.null(met)) 
      { next } else if (metric_no_accuracy[k] == FALSE) { next
      } else if (!is.null(met[[1]]$selection) | !is.null(met[[2]]$selection) | !is.null(met[[3]]$selection)) {
      graphs[[4*(j-1) + k]] <- plot_bar(met, Dose, selection, title = paste("% Times Dose Was Selected as MTD for", updated_model[j]), y_title = "% Times Dose Was Selected as MTD", col = "blue", model_picked = 2, models = selected_models, scenarios = selected_scenarios) # Using blue for MTD
      } else if (!is.null(met[[1]]$treatment) | !is.null(met[[2]]$treatment) | !is.null(met[[3]]$treatment)) {
      graphs[[4*(j-1) + k]] <- plot_bar(met, Dose_Level, treatment, title = paste("% Treated at Dose for", updated_model[j]), y_title = "% Treated at Dose", col = "blue", model_picked = FALSE, models = selected_models, scenarios = selected_scenarios) # Using blue for MTD
      } else if (!is.null(met[[1]]$overdose) | !is.null(met[[2]]$overdose) | !is.null(met[[3]]$overdose)) {
      graphs[[4*(j-1) + k]] <- plot_dist(met, overdose, mo, title = paste("Distribution of Overdoses for", updated_model[j]), x_title = "Overdose", col = "blue", model_picked = FALSE, models = selected_models, scenarios = selected_scenarios) # Using blue for mean
      } else if (!is.null(met[[1]]$length) | !is.null(met[[2]]$length) | !is.null(met[[3]]$length)) {
      graphs[[4*(j-1) + k]] <- plot_dist(met, length, ml, title = paste("Distribution of Trial Duration for", updated_model[j]), x_title = "Trial Duration", col = "blue", model_picked = FALSE, models = selected_models,  scenarios = selected_scenarios) # Using blue for mean
      } else {
        graphs[[4*(j-1) + k]] <- NULL
      } # Using fixed values for means for now
    }
   }

  # Removing NULL values from the graphs list
  filtered_graphs <- Filter(Negate(is.null), graphs)

  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)

  output$generate_graphs_ui <- renderUI({selectInput(
      ns("s_graph"), "Select a plot to view",
      choices = names(filtered_graphs), selected = names(filtered_graphs)[1], multiple = FALSE, width = "100%"
    )})

   sim_graphs <- sim_graphs(filtered_graphs) # Updating the reactive value with the new plots

  } else {output$generate_graphs_ui <- NULL} 


  } # else (after for loop)
  } # else (before for loop)
  } # else (for validation)
  }) # observe function

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

  # Comparative table outputs

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

  # Comparative tables outputs
  observeEvent(input$model_tables, {
    selected_model_no_tpt <- input$model_tables

    selected_model <- selected_model_no_tpt

    output$selected_title <- renderText({
      paste("Simulation Results for", selected_model_no_tpt)
    })

     if (selected_model_no_tpt == "3+3") {
      selected_model <- "3\\+3"
    }

    specific_model <- sim_df()[grep(selected_model, names(sim_df()))]

    specific_mtd <- specific_model[grep("MTD", names(specific_model))]

    if (length(specific_mtd) == 0) {
      output$mtd_table <- NULL
      output$mtd_title <- NULL
    } else {
      vector_mtd <- lapply(specific_mtd, function(x) x[-2, ])

     mtd_output <- do.call(rbind, vector_mtd)
  
     rownames(mtd_output) <- sapply(rownames(mtd_output), function(x) find_scenario(x, scenario_list()))

     output$mtd_table <- renderTable({
      mtd_output
    }, rownames = TRUE, colnames = TRUE)

    output$mtd_title <- renderText({"% Times Dose Was Selected as MTD"})
    }

    specific_treatment <- specific_model[grep("Treated", names(specific_model))]

    if (length(specific_treatment) == 0) {
      output$treatment_table <- NULL
      output$treatment_title <- NULL
    } else {
    vector_treatment <- lapply(specific_treatment, function(x) x[-2, ])

    treatment_output <- do.call(rbind, vector_treatment)
    
    rownames(treatment_output) <- sapply(rownames(treatment_output), function(x) find_scenario(x, scenario_list()))
    
    output$treatment_table <- renderTable({
      treatment_output
    }, rownames = TRUE, colnames = TRUE)

    output$treatment_title <- renderText({"% Treated at Each Dose"})
    }

    specific_mean <- specific_model[grep("Mean", names(specific_model))]

    mean_vector <- do.call(rbind, specific_mean) # long vector of means

    if (length(mean_vector) == 0) {
      mean_output <- NULL
      output$mean_title <- NULL
    } else {
       mean_data <- as.data.frame(mean_vector, colnames = TRUE)

    colnames(mean_data) <- "Value"

    # Converting to a table with rows as designs and column as means
    mean_data <- mean_data %>%
    rownames_to_column("Label")

    scenario_for_pattern <- paste(scenario_list(), collapse = "|")

   mean_data <- mean_data %>%
   mutate(
    Scenario = str_extract(Label, scenario_for_pattern),
    Mean = str_extract(Label, "Mean Accuracy|Mean Trial Length|Mean Overdose")
  )

  mean_reshaped <- mean_data %>%
  select(Scenario, Mean, Value) %>%
  pivot_wider(names_from = Mean, values_from = Value)

  mean_output <- as.data.frame(mean_reshaped[ , -1])
  row.names(mean_output) <- mean_reshaped$Scenario

  output$mean_title <- renderText({"Mean Values for Selected Metrics"})
    }

    output$mean_table <- renderTable({
      mean_output
    }, rownames = TRUE, colnames = TRUE)

  })

  observeEvent(input$scenario_tables, {
    selected_scen <- input$scenario_tables

    scen_number <- as.numeric(gsub("Scenario ", "", selected_scen)) # Extracting the scenario number from the selected scenario string

    true_dlts <- reactive_df()[scen_number, -1] # Exclude the first column (Scenario)

    output$true_dlts_title <- renderText({
      paste("True DLT Probabilities")
    })

    output$selected_title <- renderText({
      paste("Simulation Results for", selected_scen)
    })

    output$true_dlts_table <- renderTable({
      true_dlts
    }, rownames = FALSE, colnames = TRUE)

    specific_scen <- sim_df()[grep(selected_scen, names(sim_df()))]

    specific_mtd <- specific_scen[grep("MTD", names(specific_scen))]

    if (length(specific_mtd) == 0) {
      output$mtd_table <- NULL
      output$mtd_title <- NULL
    } else {
      vector_mtd <- lapply(specific_mtd, function(x) x[-2, ])

     mtd_output <- do.call(rbind, vector_mtd)

     rownames(mtd_output) <- sapply(rownames(mtd_output), find_model)

     output$mtd_table <- renderTable({
      mtd_output
    }, rownames = TRUE, colnames = TRUE)

    output$mtd_title <- renderText({"% Times Dose Was Selected as MTD"})
    }

    specific_treatment <- specific_scen[grep("Treated", names(specific_scen))]

    if (length(specific_treatment) == 0) {
      output$treatment_table <- NULL
      output$treatment_title <- NULL
    } else {
    vector_treatment <- lapply(specific_treatment, function(x) x[-2, ])

    treatment_output <- do.call(rbind, vector_treatment)

    rownames(treatment_output) <- sapply(rownames(treatment_output), find_model)

    output$treatment_table <- renderTable({
      treatment_output
    }, rownames = TRUE, colnames = TRUE)

    output$treatment_title <- renderText({"% Treated at Each Dose"})
    }

    specific_mean <- specific_scen[grep("Mean", names(specific_scen))]

    mean_vector <- do.call(rbind, specific_mean) # long vector of means

    if (length(mean_vector) == 0) {
      mean_output <- NULL
      output$mean_title <- NULL
    } else {
       mean_data <- as.data.frame(mean_vector, colnames = TRUE)

    colnames(mean_data) <- "Value"

    # Converting to a table with rows as designs and column as means
   mean_data <- mean_data %>%
  rownames_to_column("Label")

   mean_data <- mean_data %>%
  mutate(
    Design = str_extract(Label, "3\\+3|CRM|BOIN"),
    Mean = str_extract(Label, "Mean Accuracy|Mean Trial Length|Mean Overdose")
  )

  mean_reshaped <- mean_data %>%
  select(Design, Mean, Value) %>%
  pivot_wider(names_from = Mean, values_from = Value)

  mean_output <- as.data.frame(mean_reshaped[, -1])
  row.names(mean_output) <- mean_reshaped$Design

  output$mean_title <- renderText({"Mean Values for Selected Metrics"})
    }

    output$mean_table <- renderTable({
      mean_output
    }, rownames = TRUE, colnames = TRUE)

  })

  observeEvent(input$s_graph, {
    selected_plot <- input$s_graph
    
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

    observeEvent(input$m_graph, {
    selected_plot <- input$m_graph
    
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