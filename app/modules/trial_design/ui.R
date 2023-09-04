library(shiny)
library(shinyjs)
library(DT)
library(escalation)
library(trialr)
library(ggplot2)





#1.0 DEFINING VARIABLES TO BE INPUTTED FROM QUESTIONNAIRE UI

#Variables/parameters are defined individually here as Sian is working on unifying them into a database
#I should eventually be able to pull from database to send to backend modelling (Jake)
#Variables such as n_sims should be sent to Jake in list format

##1.1 user_profile variables:
##ranking <- c("tpt", "crm", "other") #Comment/uncomment and re-run to check UI
##ranking <- c("other", "tpt", "crm")
##ranking <- c("tpt")
##ranking <- c("tpt", "crm")
##ranking <- c("crm", "tpt")
ranking <- c("tpt", "crm", "other", "other2")
##ranking <- c("other2", "tpt", "crm", "other")

##1.2 non_spec variables (non-design-specific):
n_doses <- 5
start_dose <- 1
cohort_size <- 3
ttl <- 0.2
max_n <- 24
curent_seed <- 1234
##Perhaps simulation parameters (true_dlt_ss, n_sims) should be in this section

##1.3 spec_tpt variables (design-specific, particularly 3+3):
tpt_allow_deesc <- FALSE
true_dlt_ss_tpt_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6)
true_dlt_ss_tpt_2 <- c(0, 0.1, 0.2, 0.3, 0.4)
true_dlt_ss_tpt_3 <- c(0, 0, 0, 0.1, 0.2)
n_scenarios_tpt <- 5 #Defining this because it should be a constumaziable parameter for the scenarios table
n_sims_tpt <- 20

##1.4 spec_crm variables (design-specific, particularly CRM):
prior_mtd <- 3
prior_ttp <- c(0.02, 0.08, 0.20, 0.36, 0.52)
prior_var <- 0.5
skip_esc <- FALSE
dont_skip_esc <- TRUE #Defining this because if fits better as a get_dfcrm() argument
skip_deesc <- TRUE
dont_skip_deesc <- FALSE #Defining this because if fits better as a get_dfcrm() argument
no_esc_if_observed_gt_target <- TRUE
stop_tox_x <- 0.1
stop_tox_y <- 0.7
stop_n_mtd <- 12 #12 default value not taken from spreadsheet because 25 seemed too high
true_dlt_ss_crm_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6)
true_dlt_ss_crm_2 <- c(0, 0.1, 0.2, 0.3, 0.4)
true_dlt_ss_crm_3 <- c(0, 0, 0, 0.1, 0.2)
n_scenarios_crm <- 5 #Defining this because it should be a constumaziable parameter for the scenarios table
n_sims_crm <- 20

##1.5 spec_other variables (design-specific, particularly "other" design):
other_allow_deesc <- FALSE
true_dlt_ss_other_1 <- c(0.2, 0.3, 0.4, 0.5, 0.6)
true_dlt_ss_other_2 <- c(0, 0.1, 0.2, 0.3, 0.4)
true_dlt_ss_other_3 <- c(0, 0, 0, 0.1, 0.2)
n_scenarios_other <- 5 #Defining this because it should be a constumaziable parameter for the scenarios table
n_sims_other <- 20

##1.6 spec_other2 variables (design-specific, particularly "other2" design):
##This is just so I can test whether my code is robust to more than 3 recommended designs
mock_parameter <- "Test, test, test"

##1.7 Joining some of the variables into lists to be sent to Jake, and testing they can be indexed
##Not much of a point in coding this now since variables will eventually be pulled from database (?)

###1.7.1 Simulation scenario true DLT rates
true_dlt_ss_tpt_list <- list(
  true_dlt_ss_tpt_1,
  true_dlt_ss_tpt_2,
  true_dlt_ss_tpt_3
)
true_dlt_ss_crm_list <- list(
  true_dlt_ss_crm_1,
  true_dlt_ss_crm_2,
  true_dlt_ss_crm_3
)
true_dlt_ss_other_list <- list(
  true_dlt_ss_other_1,
  true_dlt_ss_other_2,
  true_dlt_ss_other_3
)
true_dlt_ss_all_list <- list(
  true_dlt_ss_tpt_list,
  true_dlt_ss_crm_list,
  true_dlt_ss_other_list
)
names(true_dlt_ss_all_list) <- c("tpt", "crm", "other")
true_dlt_ss_all_list
true_dlt_ss_all_list$tpt
true_dlt_ss_all_list$crm
true_dlt_ss_all_list$other

###1.7.2 Numbers of simulations
n_sims_all_list <- list(
  n_sims_tpt,
  n_sims_crm,
  n_sims_other
)
names(n_sims_all_list) <- c("tpt", "crm", "other")
n_sims_all_list
n_sims_all_list$tpt
n_sims_all_list$crm
n_sims_all_list$other





#2.0 MOCK CONFIGURATIONS TAB

#For each possible recommended design, defining a function containing its design-specific parameter inputs (as defined above)
#Defining a function database containing these functions
#Depending on the ranking vector, functions will be selected from the database and called inside the Configurations tab's columns
#(Xiaoran suggested adding a "generic function that could be adapted for any future methods, perhaps even a database", so most of this section will be changed)

##2.1 3+3 parameter inputs function
input_func_tpt <- function() {
  message <-
    "3+3-specific parameters"

  tpt_dropdown_input <- checkboxInput("tpt_dropdown_input", "Display", value = F)

  conditional_inputs <- conditionalPanel("input.tpt_dropdown_input==1",
    tpt_allow_deesc_input <- checkboxInput("tpt_allow_deesc_input",
      "Allow any de-escalation for 3+3", value = tpt_allow_deesc),

    true_dlt_ss_tpt_1_input <- textInput("true_dlt_ss_tpt_1_input",
      "Simulation scenario 1 true DLT rates vector (3+3)", value = true_dlt_ss_tpt_1),
    true_dlt_ss_tpt_2_input <- textInput("true_dlt_ss_tpt_2_input",
      "Simulation scenario 2 true DLT rates vector (3+3)", value = true_dlt_ss_tpt_2),
    true_dlt_ss_tpt_3_input <- textInput("true_dlt_ss_tpt_3_input",
      "Simulation scenario 3 true DLT rates vector (3+3)", value = true_dlt_ss_tpt_3),
    
    ##Xiaoran's reactive table code:
    ##If you prefer entering scenario information as a table:",
    ##ss_table_n_rows_input <- numericInput("ss_table_n_rows_input",
      ##"If you prefer entering as a table, number of rows:", value = 5),
    ##add_rows_input <- actionButton("add_rows_input",
      ##"Add rows"),
    ##tableOutput("ss_table"),
    ##Reactive table code I found online (closer to what I was picturing):
    "If you prefer entering scenario information into a table:",
    numericInput("n_doses_tpt_input", "Please confirm the number of doses", value = n_doses), #This is non-design-specific, so shouldn't be in Configurations tab, but it will determine the table's number of columns and I want to make it reactive
    sliderInput("n_scenarios_tpt_input", "How many scenarios would you like to run?", 1, 10, value = n_scenarios_tpt),
    my_datatable <- DTOutput("my_datatable"),
    actionButton("go", label = "Input and plot scenarios"),
    plotOutput("my_plot"),

    n_sims_tpt_input <- numericInput("n_sims_tpt_input",
      "Number of simulations per scenario (3+3)", value = n_sims_tpt)
  )

  output <- list(message,
    tpt_dropdown_input,
    conditional_inputs)

  return(output)
}

###2.1.1 3+3 parameter server function
server_func_tpt <- function(input, output, session) {
  ###Xiaoran's reactive table code:
  ###ss_table_data_frame <- data.frame(Dose = numeric(0), Scenario = character(0)) #"Initializing data frame" (?)
  ###reactive_data <- reactiveVal(ss_table_data_frame) #"Reactive value to store the data" frame (?)
  ###observeEvent(input$add_rows_input, {
    ###current_data <- reactive_data()
    ###new_rows <- data.frame(Dose = rep(0, input$ss_table_n_rows_input), Scenario = rep("", input$ss_table_n_rows_input))
    ###new_data <- rbind(current_data, new_rows)
    ###reactive_data(new_data)
  ###})
  ###output$ss_table <- renderTable({reactive_data()})

  ###Reactive table code I found online (closer to what I was picturing)
  ###So far, user inputs 2 variables into a table and then pushes a button to press them
  ###Currently changing it to fit our purposes
  v <- reactiveValues(data = {
    data.frame(x = numeric(0), y = numeric(0)) %>% dplyr::add_row(x = rep(0,5), y = rep(0,5), )
  })
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  observeEvent(input$my_datatable_cell_edit, {
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    v$data[i,j] <- k
  })
  output$my_plot <- renderPlot({
    req(input$go)
    isolate(v$data) %>% ggplot(aes(x,y)) + geom_point() + geom_smooth(method = "lm")
  })
}

##2.2 CRM parameter inputs function
input_func_crm <- function() {
  message <-
    "CRM-specific parameters"
  
  crm_dropdown_input <- checkboxInput("crm_dropdown_input", "Display", value = F)

  conditional_inputs <- conditionalPanel("input.crm_dropdown_input==1",
  prior_mtd_input <- numericInput("prior_mtd_input",
    "Your guess of which dose level is the MTD", value = prior_mtd),
  prior_ttp_input <- textInput("prior_ttp_input",
    "Prior DLT rates vector", value = prior_ttp),
  prior_var_input <- numericInput("prior_var_input",
    "Prior variance", value = prior_var),

  skip_esc_input <- checkboxInput("skip_esc_input",
    "Allow skipping doses while escalating", value = skip_esc),
  skip_deesc_input <- checkboxInput("skip_deesc_input",
    "Allow skipping doses while de-escalating", value = skip_deesc),
  no_esc_if_observed_gt_target_input <- checkboxInput("no_esc_if_observed_gt_target_input",
    "No escalation if current dose level's observed DLT rate > TTL", value = no_esc_if_observed_gt_target),

  formula <- "Formula for safety stopping: p(true DLT rate at dose > TTL + x | observed data) > y",
  stop_tox_x_input <- sliderInput("stop_tox_x_input",
    "Toxicity exceeding TTL (x)", 0, ttl, value = stop_tox_x),
  stop_tox_y_input <- numericInput("stop_tox_y_input",
    "Confidence (y)", 0, 1, value = stop_tox_y),
  stop_n_mtd_input <- numericInput("stop_n_mtd_input",
    "Min n at MTD for stopping", value = stop_n_mtd),

  true_dlt_ss_crm_1_input <- textInput("true_dlt_ss_crm_1_input",
    "Simulation scenario 1 true DLT rates vector (CRM)", value = true_dlt_ss_crm_1),
  true_dlt_ss_crm_2_input <- textInput("true_dlt_ss_crm_2_input",
    "Simulation scenario 2 true DLT rates vector (CRM)", value = true_dlt_ss_crm_2),
  true_dlt_ss_crm_3_input <- textInput("true_dlt_ss_crm_3_input",
    "Simulation scenario 3 true DLT rates vector (CRM)", value = true_dlt_ss_crm_3),

  n_sims_crm_input <- numericInput("n_sims_crm_input",
    "Number of simulations per scenario (CRM)", value = n_sims_crm)
  )

  output <- list(
    message,
    crm_dropdown_input,
    conditional_inputs
  )
  
  return(output)
}

#2.3 "Other" design parameter inputs function
input_func_other <- function() {
  message <-
    "[Design Name Here]-specific parameters"

  other_dropdown_input <- checkboxInput("other_dropdown_input", "Display", value = F)

  conditional_inputs <- conditionalPanel("input.other_dropdown_input==1",
    other_allow_deesc_input <- checkboxInput("other_allow_deesc_input",
      "Allow any de-escalation for [Design Name Here]", value = other_allow_deesc),

    true_dlt_ss_other_1_input <- textInput("true_dlt_ss_other_1_input",
      "Simulation scenario 1 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_1),
    true_dlt_ss_other_2_input <- textInput("true_dlt_ss_other_2_input",
      "Simulation scenario 2 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_2),
    true_dlt_ss_other_3_input <- textInput("true_dlt_ss_other_3_input",
      "Simulation scenario 3 true DLT rates vector ([Design Name Here])", value = true_dlt_ss_other_3),

    n_sims_other_input <- numericInput("n_sims_other_input",
      "Number of simulations per scenario ([Design Name Here])", value = n_sims_other)
  )

  output <- list(message,
    other_dropdown_input,
    conditional_inputs)

  return(output)
}

##2.4 "Other2" design parameter inputs function
input_func_other2 <- function() {
  return(mock_parameter)
}

##2.5 Function database
func_database <- list(input_func_tpt(), input_func_crm(), input_func_other(), input_func_other2())
names(func_database) <- c("tpt", "crm", "other", "other2")

##2.6 Selecting functions for Configuration tab columns
configuration_columns <- list() #Will store column() objects to be called inside fluidPage()
select_configuration_columns <- function() {
  for(n in 1:length(ranking)) {
    configuration_columns[[n]] <- column(n, func_database[ranking[n]], width = 3)
  }
  return(configuration_columns)
}





#3.0 MOCK CONDUCT TAB

##3.1 Potential CRM conduct parameter inputs function, based on Christina's prototype CRM app
input_func_crm_conduct <- function() {
  next_cohort_input <- sliderInput("next_cohort_input", "Next cohort number (e.g., if two cohorts have been completed, this should be 3)", 2, 10, value = 2)
  dose_levels_input <- textInput("dose_levels_input", "Dose levels of previous cohorts delimited by comma (e.g., 3, 4)")
  outcome_input <- textInput("outcome_input", "Number of observed DLT events observed in previous cohorts delimited by comma (e.g., 0, 1)")
  output = list(next_cohort_input, dose_levels_input, outcome_input)
  return(output)
}

##3.2 Potential function to convert the above inputs into something that can be fitted to a get_dfcrm() object (e.g., 3NNN 4NNY)
##Not finished; hence why it is commented out.
##convert_crm_conduct_inputs <- function(x = dose_levels_input, y = outcome_input) {
  ##n_of_previous_cohorts <- length(dose_levels_input)
  ##inferred_cohort_size <-
##}

####################################################################################################
##3.X Mock backend modelling
##Coding the CRM model object just so I can make a somewhat functional mock Conduct tab
model_obj_CRM <- get_dfcrm(target = ttl, skeleton = prior_ttp, scale = sqrt(prior_var))
model_obj_CRM <- stop_at_n(model_obj_CRM, n = max_n)
model_obj_CRM <- dont_skip_doses(model_obj_CRM, when_escalating = dont_skip_esc, when_deescalating = dont_skip_deesc)
model_obj_CRM <- stop_when_too_toxic(model_obj_CRM, dose = 1, ttl + stop_tox_x, confidence = stop_tox_y)
model_obj_CRM <- stop_when_n_at_dose(model_obj_CRM, dose = "recommended", n = stop_n_mtd)
####################################################################################################





#4.0 MAIN UI (CONFIGURATIONS TAB + DESIGN TABS + COMPARISON TAB + CONDUCT TAB)

##4.1 Depending on ranking vector, selecting tabs and their content functions
ui_tabs <- list()
select_ui_tabs <- function() {

  ui_tabs[[1]] <- tabPanel("Parameter Configurations", fluidRow(select_configuration_columns()))

  for(i in 2:(length(ranking)+1)) {
    ui_tabs[[i]] <- tabPanel(title = ranking[i-1], ranking[i-1]) #Functions to add Design tabs' contents should be indexed and called in lieu of this last argument
  }

  ui_tabs[[length(ranking)+2]] <- tabPanel("Cross-Method Comparison", "Cross-Method Comparison") #Function to add Comparison tab contents should be called in lieu of of this last argument
  
  ui_tabs[[length(ranking)+3]] <- tabPanel("Conduct", input_func_crm_conduct()) #Functions to add Conduct tab contents should be indexed and called in lieu of this last argument
  
  return(ui_tabs)
}

##4.2 Running the UI and the app
main_ui <- fluidPage(
  do.call(tabsetPanel, c(select_ui_tabs()))
)
main_server <- function(input, output, session) {}
#shinyApp(main_ui, main_server)
shinyApp(main_ui, server_func_tpt)
