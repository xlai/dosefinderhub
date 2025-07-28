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
     card(height = 250,
      card_header("Simulation Inputs"),
      card_body(
      simulation_inputs,
      p("Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities 
      in the table below. If the dimensions do not match, change the number of scenarios and doses and press 
      'Refresh Dimensions'."),
      test_df_table,
      input_task_button(ns("refresh_table_input"), "Refresh Table Dimensions")
      )),
      card(
        card_header("Simulation Outputs"),
        card_body(
          uiOutput(ns("tables_ui")) # Simulation tables
      )),

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
}

sim_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
ns <- session$ns
     ######################################## Configuration tab's simulation scenarios table code ########################################

  #### Simulation Variables from Configurations Tab
  n_sims <- reactive({as.numeric(input$n_sims_input)})
  n_scenarios <- reactive({as.numeric(input$n_scenarios_input)})

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
      selectizeInput(ns("scen_output_input"), "Select scenarios", choices = updated_scen_choices(),
        multiple = TRUE, list(plugins = list('remove_button')))
    )
  })

 ######################################### Simulation Outputs #########################################
 # Code copied directly from the trial_design tab's server code
 # Simulation outputs

  observeEvent(input$run_simulation, {

    # Adding in Scenarios. I am going to cap the possible number of Scenarios to 3 (this can be changed later).

  selected_scenarios <- cbind(
    scen1 <- {"Scenario 1" %in% input$scen_output_input},
    scen2 <- {"Scenario 2" %in% input$scen_output_input},
    scen3 <- {"Scenario 3" %in% input$scen_output_input}
  )
  #print(selected_scenarios)
  #print(true_dlts())
  used_true_dlts <- true_dlts()[selected_scenarios, ] # Scenarios are rows!
  #print(used_true_dlts)
  n_scen <- nrow(used_true_dlts)
  #print(n_scen)
  combined_list <- vector("list", n_scen) # initialising for use later
  title_list <- vector("list", n_scen) # initialising for use later

   # Metric - putting it outside of the for loop so only one list is created.
  selected_metric <- cbind(
  selected_mtd <- {"% times dose was selected as MTD" %in% input$metric_selection_input},
  selected_participant <- {"% participants treated at dose" %in% input$metric_selection_input},
  selected_accuracy <- {"Accuracy" %in% input$metric_selection_input},
  selected_overdose <- {"Overdosing" %in% input$metric_selection_input},
  selected_duration <- {"Duration" %in% input$metric_selection_input})
  #print(selected_metric)
  if (n_scen == 0) {tables_ui <- NULL} else { for (j in 1:n_scen) {

  # Design - only running simulations that are necessary to save time.
  if ("3+3" %in% input$simulation_design_selection_input)
      { #print(unlist(used_true_dlts[j, ]))
        tpt_sim <- sim_tpt(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), shared$skip_tpt(), 12345)
       tpt_modified_tab <- tpt_sim[-c(3,5,7)]

      tpt_modified_tab$mean_accuracy <- as.data.frame(tpt_modified_tab$mean_accuracy, row.names = "Mean Accuracy")
      tpt_modified_tab$mean_overdose <- as.data.frame(tpt_modified_tab$mean_overdose, row.names = "Mean Overdose")
      tpt_modified_tab$mean_length <- as.data.frame(tpt_modified_tab$mean_length, row.names = "Mean Trial Length")
      colnames(tpt_modified_tab$mean_accuracy) <- ""
      colnames(tpt_modified_tab$mean_length)<- ""
      colnames(tpt_modified_tab$mean_overdose) <- ""

      } else {tpt_modified_tab <- NULL}
  if ("CRM" %in% input$simulation_design_selection_input)
      {
      crm_sim <- sim_crm(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$start_dose(), n_sims(), unlist(used_true_dlts[j, ]), shared$skeleton_crm(), shared$prior_var_crm(), shared$skip_esc_crm(), shared$skip_deesc_crm(), shared$stop_tox_x_crm(), shared$stop_tox_y_crm(), shared$stop_n_mtd_crm())
      crm_modified_tab <- crm_sim[-c(3,5,7)]

      crm_modified_tab$mean_accuracy <- as.data.frame(crm_modified_tab$mean_accuracy, row.names = "Mean Accuracy", col.names = FALSE)
      crm_modified_tab$mean_overdose <- as.data.frame(crm_modified_tab$mean_overdose, row.names = "Mean Overdose", col.names = FALSE)
      crm_modified_tab$mean_length <- as.data.frame(crm_modified_tab$mean_length, row.names = "Mean Trial Length", col.names = FALSE)
      colnames(crm_modified_tab$mean_accuracy) <- ""
      colnames(crm_modified_tab$mean_length)<- ""
  colnames(crm_modified_tab$mean_overdose) <- ""
      } else {crm_modified_tab <- NULL}

  # Giving Titles to Single Value Ouputs

  tpt_to_display <- tpt_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  crm_to_display <- crm_modified_tab[c(which(selected_metric == TRUE))] # A list of lists we want to display
  
  tpt_title <- as.character(rep("3+3 Simulation for Scenario ", 5))
  crm_title <- as.character(rep("CRM Simulation for Scenario ", 5))
  scenario_number <- as.character(rep(j, 5))
  metric_names <- as.character(c(" - % Times Dose Was Selected as MTD", "- % Treated at Each Dose",  " - Mean Accuracy", " - Mean Overdose", " - Mean Trial Length"))

  if ("3+3" %in% input$simulation_design_selection_input) {
  full_tpt_titles <- paste(as.character(tpt_title), as.character(scenario_number), as.character(metric_names))
  } else {full_tpt_titles <- NULL}

  if("CRM" %in% input$simulation_design_selection_input) {
  full_crm_titles <- paste(as.character(crm_title), as.character(scenario_number), as.character(metric_names))
  } else {full_crm_titles <- NULL}

  #print(full_tpt_titles)
  used_tpt_titles <- full_tpt_titles[c(which(selected_metric == TRUE))] # A list of titles we want to display
  
  #print(used_tpt_titles)

  used_crm_titles <- full_crm_titles[c(which(selected_metric == TRUE))] # A list of titles we want to display
  #print(used_crm_titles)

  tpt_data_frames <- lapply(tpt_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes
  crm_data_frames <- lapply(crm_to_display, function(x) as.data.frame(x)) # Converting the list into a list of dataframes
  
  combined_list[[j]]  <- cbind(tpt_data_frames, crm_data_frames)
  cbind_titles <- cbind(used_tpt_titles, used_crm_titles)
  title_list[[j]] <- unname(unlist(cbind_titles))

  } # for loop end

  combined_data_frames <- do.call(c, combined_list) 
  #print(combined_data_frames)
  combined_titles <- do.call(c, title_list) 
  #print(combined_titles)
  
  n_data_frames <- length(combined_data_frames)
  #print(n_data_frames)
  # Using generic table names to render the UI with all the tables in it.
  if (n_data_frames == 0) {output$tables_ui <- NULL  # The case where nothing is entered
  } else {
  table_names <- c(paste(rep("Table", n_data_frames), as.list(as.character(1:n_data_frames)), sep = " "))
  names(combined_data_frames) <- table_names
  #print(table_names)
  ## Using the names of the tables to render a UI with all the tables in it.
   output$tables_ui <- renderUI({
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

  
  } # else (after for loop)
  } # else (before for loop)
  }) # observe function

  }) # End of moduleServer
} # End of sever function