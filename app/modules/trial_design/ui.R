library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)



#DUMMY DATA MANIPULATION

dummy_data <- readRDS("app/modules/trial_design/dummy_data1.RData")
#View(dummy_data)

dummy_data_trial <- dummy_data$trial
#View(dummy_data_trial)
n_doses <- dummy_data_trial[dummy_data_trial$q_variable == "n_doses", "value"]
#n_doses

dummy_data_method <- dummy_data$method
#View(dummy_data_method)

dummy_data_ranking <- dummy_data$ranking
#View(dummy_data_ranking)
ranking <- c(dummy_data_ranking$method)
#ranking
ranking <- c("crm", "tpt", "other") #REMOVE/COMMENT OUT; JUST TO TEST TAB DYNAMICS
prettify_ranking <- function(ranking_argument) {
  pretty_ranking <- ranking_argument
  pretty_ranking[pretty_ranking == "crm"] <- "CRM"
  pretty_ranking[pretty_ranking == "tpt"] <- "3+3"
  pretty_ranking[pretty_ranking == "other"] <- "Other design (TEST)"
  return(pretty_ranking)
}
pretty_ranking <- prettify_ranking(ranking)

##Function to change parameter strings in preparation for extracting UI specs
parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, '[', 1)
  sapply(param_list, '[', 2)
}

##Extracting non-design-specific UI specs
non_specific_ui_inputs <-
  tagList(
    lapply(dummy_data_trial$q_number, function(i) {
      question <- dummy_data_trial[dummy_data_trial$q_number == i, ]
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
    })
  )

##Extracting design-specific UI specs
method_current <- list()
dummy_data_method_current <- list()
specific_ui_inputs <- list()
for (n in 1:length(ranking)) {
  method_current <- ranking[n]
  dummy_data_method_current <- dummy_data_method[dummy_data_method$design == method_current, ]
  specific_ui_inputs[[n]] <-
    tagList(
    lapply(dummy_data_method_current$q_number, function(i) {
      question <- dummy_data_method_current[dummy_data_method_current$q_number == i, ]
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
    })
    )
}




#CONFIGURATIONS TAB

##Defining non-design-specific + simulation parameters column inputs function
non_specific_column_func <- function() {
  title <- "General Trial Parameters"
  non_specific_ui_inputs
  text <- "Simulation scenarios 'True' DLT rates input table:"
  n_scenarios_input <- numericInput("n_scenarios_input", "How many scenarios would you like to simulate?", min = 1, value = 3)
  table_output <- DT::DTOutput("table_output")
  plot_button <- actionButton("plot_button", label = "Test plot")
  plot <- plotOutput("plot")
  return <- list(title, non_specific_ui_inputs, text, n_scenarios_input, table_output, plot_button, plot)
}

##Defining Configurations tab columns
specific_columns <- list()
select_specific_columns <- function() {
  for(n in 1:(length(ranking))) {
    specific_columns[[n]] <- column(n, paste0(": ", pretty_ranking[n]), specific_ui_inputs[[n]], width = 2)
  }
  return(specific_columns)
}
config_tab_func <- function() {
  sidebarLayout(
    sidebarPanel(non_specific_column_func(), width = 4),
    mainPanel(select_specific_columns())
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
      
      #textOutput("n_scenarios_for_sim_tab"),
      
      selectizeInput("scenario_selection_input", "Select scenarios",
        choices = paste0("Scenario ", 1:10),
        multiple = TRUE,
        list(plugins = list('remove_button'))),
      
      #uiOutput("sssss"), #PART OF SECOND TRY; DIDN'T WORK
      
      selectizeInput("metric_selection_input", "Select outputs/metrics",
        choices = c("% participants treated at dose",
          "% times dose was selected as MTD",
          "Accuracy",
          "Duration",
          "Overdosing"),
        multiple = TRUE,
        list(plugins = list('remove_button'))),
      
      selectizeInput("visual_selection_input", "Select type of output",
        choices = c("Table", "Plot"),
        multiple = TRUE,
        list(plugins = list('remove_button'))),

      actionButton("submit", "Submit"),

      width = 5

     ),

     mainPanel("INSERT DYNAMIC SIMULATION OUTPUTS HERE")

    )
 }



#CONDUCT TAB

##Conduct tab UI function
cond_tab_input_func <- function() {
  sidebarLayout(
    
    sidebarPanel(
        radioButtons("conduct_design_selection_input", "Select which design to update during trial conduct",
          choices = c(unique(dummy_data_method$design)),
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




#SERVER

##Most of it is the simulation scenarios table code

column_names <- sprintf("d(%d)", 1:n_doses)

server_all <- function(input, output, session) {
  
  ######################################## Simulation scenarios table code ########################################

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
    datatable(reactive_table_data(), editable = TRUE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"), list(targets = 0, className = "not-editable"))),
              rownames = FALSE)
  }, server = FALSE)
  
  output$table_output <- renderDT({
    datatable(reactive_table_data(), editable = TRUE, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = F)
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
  
  ######################################## Simulation tab server (carryover of n_sims onto Simulation tab - not working so far) ########################################
  observe({
    n_scenarios_for_sim_tab <- as.numeric(input$n_scenarios_input)
    output$n_scenarios_for_sim_tab <- renderText(n_scenarios_for_sim_tab)
    #Second try
    #choices <- paste0("Scenario ", 1:n_scenarios_for_sim_tab)
    #output$sssss <- renderUI({
      #selectizeInput("sssss", "SSSSS", choices = choices, multiple = TRUE, options = list(plugins = list('remove_button')))
    #})
    #DIDN'T WORK
  })

  ######################################## Conduct tab server ########################################
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

  ####################End of server function####################
}



#MAIN UI

##Defining UI tabs
ui_tabs <- list()
ui_tabs[[1]] <- tabPanel("Configurations",
  config_tab_func()
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
shinyApp(ui, server_all)
