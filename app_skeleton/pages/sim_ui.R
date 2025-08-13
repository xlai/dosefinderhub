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
            uiOutput(ns("tables_ui")), # Individual view
            textOutput(ns("titles1")),
            tableOutput(ns("tables1")),
            uiOutput(ns("buttons1")),
            tags$hr(),
            textOutput(ns("titles2")),
            tableOutput(ns("tables2")),
            uiOutput(ns("buttons2"))
          ),
          nav_panel("Simulation Output - Plots",
          h3("Simulation Output - Plots"),
          uiOutput(ns("generate_graphs_ui"))
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
          choices = c("Individually", "Comparatively"), selected = "Comparatively", inline = TRUE),
    

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
 
  scen_1_init <- c(1, example_scenarios(0.3, 3, 0.05, 5, 1))
  scen_2_init <- c(2, example_scenarios(0.3, 3, 0.05, 5, 2))
  scen_3_init <- c(3, example_scenarios(0.3, 3, 0.05, 5, 3))
  matrix <- rbind(scen_1_init, scen_2_init, scen_3_init)
  colnames(matrix) <- list("Scenario", "d1", "d2", "d3", "d4", "d5")

  reactive_df <- reactiveVal(matrix) # initalising a reactive value to store the data frame

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

  ex_scen_1 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm(), 0.05, shared$n_dosess(), 1)
  ex_scen_2 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm(), 0.05, shared$n_dosess(), 2)
  ex_scen_3 <- example_scenarios(shared$ttl(), shared$prior_mtd_crm(), 0.05, shared$n_dosess(), 3)

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

  ## The reactive table
  observeEvent(input$test_df_cell_edit, {
  if (any(is.na(reactive_df()))) {
    output$table_warning <- renderText({
      "⚠️ Please ensure all cells in the True DLT probabilities table are filled out before running the simulation."
    })
  } else if (any(reactive_df()[, -1] < 0 | reactive_df()[, -1] > 1)) {
    output$table_warning <- renderText({
      "⚠️ Please ensure all True DLT probabilities are between 0 and 1."
    })
  } else { incr_val <- rep(FALSE, nrow(reactive_df()))
    for (i in 1: nrow(reactive_df())) {
    if (is.unsorted(reactive_df()[i, -1])) {
      incr_val[i] <- TRUE
    } 
  }
  if (any(incr_val)) {
    output$table_warning <- renderText({
        "⚠️ Please ensure all rows of the table contain an increasing sequence of true DLT probabilities."
      })
  } else {
    output$table_warning <- NULL
  }
  }
  })

 ######################################### Simulation Outputs #########################################
 # Code copied directly from the trial_design tab's server code
 # Simulation outputs

 sim_df <- reactiveVal(NULL) # initialising
 sim_titles <- reactiveVal(NULL) # initialising

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
 
  if (n_scen == 0) {tables_ui <- NULL} else { for (j in 1:n_scen) {

  # Design - only running simulations that are necessary to save time.
  if ("3+3" %in% input$simulation_design_selection_input)
      { tpt_sim <- sim_tpt(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), shared$skip_tpt(), 12345)
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
  if (n_data_frames == 0) {generate_tables_ui <- NULL  # The case where nothing is entered
  } else {
  table_names <- c(paste(rep("Table", n_data_frames), as.list(as.character(1:n_data_frames)), sep = " "))
  names(combined_data_frames) <- table_names

  ## Using the names of the tables to render a UI with all the tables in it.
  generate_tables_ui <- renderUI({
    lapply(names(combined_data_frames), function(table_name) {
      table_number <- as.numeric(gsub("Table ", "", table_name)) # Extracting the number from the table name
      tagList(
      h4(combined_titles[[table_number]]), # Title for each table
        tableOutput(ns(paste0("table_", table_name))) # Table output
      )
    })
  })
  
  # Rendering each table
  lapply(names(combined_data_frames), function(table_name) {
    output[[paste0("table_", table_name)]] <- renderTable({
      combined_data_frames[[table_name]]
    }, rownames = TRUE, colnames = TRUE) 
  }) 

  tables_and_titles <- renderUI({ generate_tables_ui })

  if ("Individually" %in% input$comparative_view | n_data_frames == 1) {
    output$tables_ui <- tables_and_titles

    output$titles1 <- NULL
    output$titles2 <- NULL
    output$tables1 <- NULL
    output$tables2 <- NULL
    output$buttons1 <- NULL
    output$buttons2 <- NULL


  } else {output$tables_ui <- NULL

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

  # Focusing on "by model" 
  metric_no_accuracy <- selected_metric[-3] # Removing accuracy from the list of selected metrics

  if ("Individually" %in% input$comparative_view) {
  graphs <- vector("list", 4*n_scen) # initialising for use later

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
      } # Using fixed values for means for now
    }
   }

  # Removing NULL values from the graphs list
  filtered_graphs <- Filter(Negate(is.null), graphs)

  output$generate_graphs_ui <- renderUI({
    lapply(seq_along(filtered_graphs), function(i) {
      plotOutput(ns(paste0("plot_", i)), height = "400px")
    })
  })

  # Rendering each graph
  lapply(seq_along(filtered_graphs), function(i) {

    output[[paste0("plot_", i)]] <- renderPlot({
      filtered_graphs[[i]]
    })
  })

  } else if ("Comparatively" %in% input$comparative_view) {
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
 
  tpt_by_scenario <- plot_by_scenario(plot_tpt_full)
  crm_by_scenario <- plot_by_scenario(plot_crm_full)
  boin_by_scenario <- plot_by_scenario(plot_boin_full)

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

  output$generate_graphs_ui <- renderUI({
    lapply(seq_along(filtered_graphs), function(i) {
      plotOutput(ns(paste0("plot_", i)), height = "400px")
    })
  })

  # Rendering each graph
  lapply(seq_along(filtered_graphs), function(i) {

    output[[paste0("plot_", i)]] <- renderPlot({
      filtered_graphs[[i]]
    })
  })

  } else {output$generate_graphs_ui <- NULL} # For now.

  } # else (after for loop)
  } # else (before for loop)
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


  }) # End of moduleServer
} # End of sever function