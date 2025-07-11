library(shiny)
library(DT)
library(ggplot2)
library(here)

#DUMMY DATA MANIPULATION

here::i_am('app/modules/trial_design/ui.R') #setting the path to this file
data_env <- new.env()
load(here('app', 'modules', 'trial_design', 'dummy_data5.RData'),
     envir = data_env) #loading dummy_data5.RData
dummy_data <- data_env$dummy_data 
# I have used dummy_data5 for now (generatied using generate_dummy_data) - this needs to be changed.
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
#THIS FUNCTION IS NOT FUTURE-PROOF!!!!! I HAVE SPOKEN WITH SIAN ABOUT ADDING "PRETTY" DESIGN NAMES TO DATABASE

##Function to change parameter strings in preparation for extracting UI specs
parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, '[', 1)
  sapply(param_list, '[', 2)
}

# Helper function to generate UI element
generate_ui_element <- function(question) {
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
}

# Extracting non-design-specific UI specs
non_specific_ui_inputs <- tagList(
  lapply(dummy_data_trial$q_number, function(i) {
    question <- subset(dummy_data_trial, q_number == i)
    generate_ui_element(question)
  })
)

# Extracting design-specific UI specs
specific_ui_inputs <- lapply(ranking, function(method_current) {
  dummy_data_method_current <- subset(dummy_data_method, design == method_current)
  tagList(
    lapply(dummy_data_method_current$q_number, function(i) {
      question <- subset(dummy_data_method_current, q_number == i)
      generate_ui_element(question)
    })
  )
})



#CONFIGURATIONS TAB

##Defining non-design-specific + simulation parameters column inputs function
non_specific_column_func <- function() {
  #label <- "Input file with all configurations"
  upload_button <- fileInput("config_file_upload", "Input file with all configurations", accept = c(".csv", ".rds"))
  #label_2 <- "Download file with all configurations"
  download_button <- downloadButton("config_save_button", "Download file with all configurations")
  separator <- "___________________________________________"
  title <- "GENERAL TRIAL PARAMETERS"
  display_button <- checkboxInput("display_input_all", "Display parameters", value = F)
  conditional_non_specific_ui_inputs <- conditionalPanel(condition = "input.display_input_all==1", non_specific_ui_inputs)
  separator <- "___________________________________________"
  text <- "SIMULATION PARAMETERS"
  n_sims_input <- numericInput("n_sims_input", "How many simulations would you like to run per design per scenario?", value = 10)
  n_scenarios_input <- numericInput("n_scenarios_input", "How many scenarios would you like to simulate?", min = 1, value = 3)
  text2 <- "Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities in the table below:"
  table_output <- DT::DTOutput("table_output")
  #plot_button <- actionButton("plot_button", label = "Test plot")
  #plot <- plotOutput("plot")
  return <- list(upload_button, download_button, separator, title, display_button, conditional_non_specific_ui_inputs, separator, text, n_sims_input, n_scenarios_input, text2, table_output)
}

##Defining Configurations tab columns
display_input_id <- list()
display_condition <- list()
specific_columns <- list()
select_specific_columns <- function() {
  for(n in 1:(length(ranking))) {
    display_input_id[[n]] <- paste0("display_input_", ranking[n], sep="")
    display_condition[[n]] <- paste0("input.", display_input_id[[n]], "==1", sep="")
    specific_columns[[n]] <- column(
      n,
      paste0(": ", pretty_ranking[n]),
      checkboxInput(display_input_id[[n]], "Display parameters", value = F),
      conditionalPanel(condition = display_condition[[n]], specific_ui_inputs[[n]]),
      width = 4)
  }
  return(specific_columns)
}

##Defining Configurations tab input function
config_tab_input_func <- function() {
  sidebarLayout(
    sidebarPanel(non_specific_column_func(), width = 4),
    mainPanel(select_specific_columns()),
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
      
      uiOutput("scen_output_question"),
      
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
        #list(plugins = list('remove_button'))),

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
          choices = c(unique(pretty_ranking)),
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



#MAIN UI

##Defining UI tabs
ui_tabs <- list()
ui_tabs[[1]] <- tabPanel("Configurations",
  config_tab_input_func()
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



#shinyApp(ui, server_all)
