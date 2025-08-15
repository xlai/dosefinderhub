#library(shiny)
#library(shiny.semantic)

trial_design_ui <- function(id) {
  ns <- NS(id)

#################################### From Configurations Tab UI ####################################


# Non-specific UI inputs for trial design
  n_doses_output <- numericInput(ns("n_doses_inputt"), "How many dose levels are being tested?", min = 1, value = "5", step = 1)
  ttl_output <- numericInput(ns("ttl_inputt"), "What is the target toxicity level for this trial, as a decimal?", min = 0, max = 0.999, value = "0.3", step = 0.01)
  max_size_output <- numericInput(ns("max_size_inputt"), "What is the maximum sample size for this trial?", min = 1, value = "30", step = 1)
  start_dose_output <- numericInput(ns("start_dose_inputt"), "What is the starting dose level?", min = 1, value = "1", step = 1)
  cohort_output <- numericInput(ns("cohort_inputt"), "What size will the cohorts be?", min = 1, value = "5", step = 1)

  n_doses_warning_text <- textOutput(ns("n_doses_warning"))
  ttl_warning_text <- textOutput(ns("ttl_warning"))
  max_size_warning_text <- textOutput(ns("max_size_warning"))
  start_dose_warning_text <- textOutput(ns("start_dose_warning"))
  cohort_warning_text <- textOutput(ns("cohort_warning"))

  non_specific_ui_inputts <- tagList(
    n_doses_output,
    n_doses_warning_text,
    ttl_output,
    ttl_warning_text,
    max_size_output,
    max_size_warning_text,
    start_dose_output,
    start_dose_warning_text,
    cohort_output,
    cohort_warning_text
  )

# Specific UI inputs for trial design
# CRM specific inputs
skip_esc_crm_input <- radioButtons(ns("skip_esc_crm_input"),"Would you like to be able to skip doses when escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE)
skip_deesc_crm_input <- radioButtons(ns("skip_deesc_crm_input"),"Would you like to be able to skip doses when de-escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)
above_target_input <- radioButtons(ns("above_target_input"),
"Do you want to prevent escalation of doses if the overall observed DLT rate at the current dose level is above the target DLT rate?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)
prior_var_input <- numericInput(ns("prior_var_input"), "What is the estimate of the prior variance?", min = 0, value = 0.1)
stop_n_mtd_input <- numericInput(ns("stop_n_mtd_input"), "What is the minimum number of patients required at recommended dose before early stopping?", min = 1, value = 24)

skeleton_table <- DT::DTOutput(ns("skeleton_df"))

prior_mtd_input <- numericInput(ns("prior_mtd_input"), "What is your prior guess of the MTD?", min = 1, value = 3)
stop_tox_x_input <- numericInput(ns("stop_tox_x_input"), "When using the this Bayesian safety early criterion: p(true DLT rate at lowest dose > target DLT rate + x | data) > y, what would you like x to be? This is the excess toxicity above the target DLT.", min = 0, value = 0.09)
stop_tox_y_input <- numericInput(ns("stop_tox_y_input"), "What would you like y to be? This is the confidence level for safety stopping.", min = 0, max = 1, value = 0.77)

prior_var_warning_text <- textOutput(ns("prior_var_warning"))
stop_n_mtd_warning_text <- textOutput(ns("stop_n_mtd_warning"))
prior_mtd_warning_text <- textOutput(ns("prior_mtd_warning"))
stop_tox_x_warning_text <- textOutput(ns("stop_tox_x_warning"))
stop_tox_y_warning_text <- textOutput(ns("stop_tox_y_warning"))

specific_ui_inputs_crm <- tagList(
  skip_esc_crm_input,
  skip_deesc_crm_input,
  above_target_input,
  prior_var_input,
  prior_var_warning_text,
  stop_n_mtd_input,
  stop_n_mtd_warning_text,
   p("What are the prior estimates of the DLT rates at each dose? Please add them to the table below."),
  skeleton_table,
  prior_mtd_input,
  prior_mtd_warning_text,
  stop_tox_x_input,
  stop_tox_x_warning_text,
  stop_tox_y_input,
  stop_tox_y_warning_text
)

# BOIN specific inputs
boin_input_choice <- radioButtons("boin_input_choice", "How Would You Like to Input the BOIN Escalation Boundaries?",
choices = c("Enter the toxicity probability threshold as a multiple of the target toxicity level" = 1, 
            "Enter the toxicity probability threshold independently of the target toxicity level" = 2,
            "Enter the escalation boundaries directly" = 3),
              selected = NULL, inline = TRUE)

boin_stopping_rule <- radioButtons(ns("boin_stopping_rule"), "Would you like to use the BOIN toxicity stopping rule?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE)

boin_cohorts <- numericInput(ns("boin_cohorts"), "How many cohorts would you like to use?", min = 1, value = 3)
stop_n_mtd_boin <- numericInput(ns("stop_n_mtd_boin"), "What is the minimum number of patients required at recommended dose before early stopping?", min = 1, value = 24)

ttl_multiple_phi_1 <- numericInput(ns("ttl_multiple_phi_1"), "What is the highest multiple of the target toxicity level such that dose escalation should be made?", min = 0, max = 1, value = 0.6)
ttl_multiple_phi_2 <- numericInput(ns("ttl_multiple_phi_2"), "What is the lowest multiple of the target toxicity level such that dose de-escalation should be made?", min = 0, max = 1, value = 1.4)

direct_phi_1 <- numericInput(ns("direct_phi_1"), "What is the highest toxicity probability below the target toxicity level such that dose escalation should be made?", min = 0, max = 1, value = 0.18)
direct_phi_2 <- numericInput(ns("direct_phi_2"), "What is the lowest toxicity probability above the target toxicity level such that dose de-escalation should be made?", min = 0, max = 1, value = 0.42)

direct_lambda_e <- numericInput(ns("direct_lambda_e"), "What is the escalation boundary, lambda_e?", min = 0, value = 0.9)
direct_lambda_d <- numericInput(ns("direct_lambda_d"), "What is the de-escalation boundary, lambda_d?", min = 0, value = 0.1)

boin_general_inputs <- tagList(
  boin_input_choice,
  hr(),
  boin_stopping_rule,
  boin_cohorts,
  boin_cohorts_warning_text <- textOutput(ns("boin_cohorts_warning")),
  stop_n_mtd_boin,
  stop_n_mtd_boin_warning_text <- textOutput(ns("stop_n_mtd_boin_warning"))
)
boin_ui_inputs_multiple_ttl <- tagList( 
  hr(),
  ttl_multiple_phi_1,
  ttl_multiple_phi_1_warning_text <- textOutput(ns("ttl_multiple_phi_1_warning")),
  ttl_multiple_phi_2,
  ttl_multiple_phi_2_warning_text <- textOutput(ns("ttl_multiple_phi_2_warning")),
  hr(),
  actionButton(ns("boin_generate_boundaries"), "Generate Escalation Boundaries"),
  hr(),
  textOutput(ns("boin_output_lambda_e")),
  textOutput(ns("boin_output_lambda_d"))
)

boin_ui_inputs_direct_tox_prob <- tagList(
  hr(),
  direct_phi_1,
  direct_phi_1_warning_text <- textOutput(ns("direct_phi_1_warning")),
  direct_phi_2,
  direct_phi_2_warning_text <- textOutput(ns("direct_phi_2_warning")),
  hr(),
  actionButton(ns("boin_generate_boundaries_dir"), "Generate Escalation Boundaries"),
  hr(),
  textOutput(ns("boin_output_lambda_e_dir")),
  textOutput(ns("boin_output_lambda_d_dir"))
)

boin_ui_inputs_direct_boundaries <- tagList(
  hr(),
  direct_lambda_e,
  direct_lambda_d
)

other_basic_inputs <- tagList(
  numericInput(ns("basic_prior_mtd_input"), "What is your prior guess of the true MTD?", min = 1, value = 3, step = 1),
  textOutput(ns("basic_prior_warning")),
  radioButtons(ns("basic_skip_esc_input"),"Would you like to be able to skip doses when escalating? (For CRM models only)",
                   choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE),
  radioButtons(ns("basic_skip_deesc_input"),"Would you like to be able to skip doses when de-escalating? (For CRM models only)",
                  choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE)
)


########################################### Running the UI ###########################################


  page_sidebar( 
      radioButtons(ns("modes"), "Which mode would you prefer to use?", choices = c("Basic" = 1, "Advanced" = 2), selected = 2, inline = TRUE),
      tags$hr(),
      textOutput(ns("mode_title"), container = h3),
   # General Trial Design Parameters
     layout_column_wrap(  
     card(full_screen = TRUE,
      card_header("General Trial Parameters"),
      card_body(
       checkboxInput("display_input_all", "Display parameters", value = FALSE),
       conditionalPanel(condition = "input.display_input_all==1", non_specific_ui_inputts)
     )
      )),

      #Specific Trial Design Parameters
       conditionalPanel(condition = sprintf("input['%s'] == 2", ns("modes")),
       layout_column_wrap(  
        card( full_screen = TRUE,
          card_header("CRM Parameters"),
          card_body(
           checkboxInput("display_crm", "Display parameters", value = FALSE),
           conditionalPanel(condition = "input.display_crm==1", specific_ui_inputs_crm)
          )
        ),
        card(full_screen = TRUE,
          card_header("BOIN Parameters"),
          card_body(
            checkboxInput("display_boin", "Display parameters", value = FALSE),
           conditionalPanel(condition = "input.display_boin == 1", boin_general_inputs),
           conditionalPanel(condition = "input.display_boin == 1 && input.boin_input_choice == 1", boin_ui_inputs_multiple_ttl),
           conditionalPanel(condition = "input.display_boin == 1 && input.boin_input_choice == 2", boin_ui_inputs_direct_tox_prob),
           conditionalPanel(condition = "input.display_boin == 1 && input.boin_input_choice == 3", boin_ui_inputs_direct_boundaries),
          )
        )
      )
       ),
       conditionalPanel(condition = sprintf("input['%s'] == 1", ns("modes")),
       layout_column_wrap(  
        card( full_screen = TRUE,
          card_header("Model-Specific Basic Parameters"),
          card_body(
           checkboxInput("display_basic", "Display parameters", value = FALSE),
           conditionalPanel(condition = "input.display_basic==1", other_basic_inputs)
          )
        ),
      ),
        p("Once you've finished filling out the basic mode, press the button below to update the advanced parameters to 'general' results that match the basic inputs."),
        actionButton(ns("transfer_advanced"), "Transfer Results to Advanced Mode")
       ),
    sidebar = sidebar(
      h3("Trial Design"),
      tags$hr(), # Separator line
      fileInput("config_file_upload", 
      "Saved a Trial Design file? Input it here to retrieve your configurations.", 
      accept = c(".csv", ".rds")),

      p("Want to save your configurations for later? Download a them as a file by clicking the button below."),
      downloadButton("config_save_button", "Download file with all configurations"),
      tags$hr(), # Separator line
      p("Done filling out the configurations? Click the button below to run simulations."),
      actionButton(ns("view_simulation"), "View Simulation"),

    )
    )
}

trial_design_server <- function(id, shared, move_data, parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
##### Reactive Skeleton Input ######
reactive_skeleton <- reactiveVal() # initalising a reactive value to store the data frame

 observeEvent({input$n_doses_inputt | input$prior_mtd_input}, {  
    if (is.na(input$n_doses_inputt) | is.na(input$prior_mtd_input)) {  
      reactive_skeleton(NULL)  
    } else if (input$prior_mtd_input > input$n_doses_inputt)  {  
      reactive_skeleton(NULL)  
    } else {
  dose <- as.integer(input$n_doses_inputt)  
  Prior <- dfcrm::getprior(halfwidth = 0.25*input$ttl_inputt, target = input$ttl_inputt, nu = input$prior_mtd_input, nlevel = dose)

  rownames_skel <-paste("d", 1:dose, sep = "")

  df <- data.frame(Dose = rownames_skel, Prior = Prior)

  reactive_skeleton(df) # Updating the reactive value with the new data frame
   } })
  
  output$skeleton_df <- renderDT({
    datatable(reactive_skeleton(), editable = TRUE, rownames = FALSE, options = list(searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE)) #, scrollX = TRUE, scrollX="250px", paging = FALSE
  })

  observeEvent(input$skeleton_df_cell_edit, {
    info <- input$skeleton_df_cell_edit

    modified_data <- reactive_skeleton()
    modified_data[info$row, info$col + 1] <- DT::coerceValue(info$value, modified_data[info$row, info$col]) # +1 is here to counterract the movement of edited data.
    reactive_skeleton(modified_data)
  })

  shared$skeleton_crm <- reactive({
    as.vector(reactive_skeleton()[, -1])
  })
  #print(shared$skeleton_crm())

################## Questionnaire Inputs ##################    
# There needs to be logic here to trasnfer data - I have taken this directly from the old Transfer Results From Questionnaire button.
observeEvent(move_data(), {
# Transfer questionnaire results to trial design inputs
  if (length(shared$q_n_doses()) > 0) {
    updateNumericInput(session, "n_doses_inputt", value = shared$q_n_doses())
  } else {
    updateNumericInput(session, "n_doses_inputt", value = 5) # Default value if not set
  }
  if (length(shared$q_ttl()) > 0) {
    updateNumericInput(session, "ttl_inputt", value = shared$q_ttl())
  } else {
    updateNumericInput(session, "ttl_inputt", value = 0.3) # Default value if not set
  }
  if (length(shared$q_max_size()) > 0) {
    updateNumericInput(session, "max_size_inputt", value = shared$q_max_size())
  }
  else {
    updateNumericInput(session, "max_size_inputt", value = 30) # Default value if not set
  }
  if (length(shared$q_start_dose()) > 0) {
    updateNumericInput(session, "start_dose_inputt", value = shared$q_start_dose())
  }
  else {
    updateNumericInput(session, "start_dose_inputt", value = 1) # Default value if not set
  }
  if (length(shared$q_cohort()) > 0) {
    updateNumericInput(session, "cohort_inputt", value = shared$q_cohort())
  }
  else {
    updateNumericInput(session, "cohort_inputt", value = 3) # Default value if not set
  }

  # Need to make move_data FALSE again so that it doesn't keep transferring data
  move_data(FALSE)
})


    #################################### From Configurations Tab Server #####################################

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

  #Download - This doesn't work. Should be changed to allow for new variable names.

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

  # General variables from configuration tab - use questionnaire values if available, otherwise UI inputs
  shared$n_dosess <- reactive({
    if (!is.null(shared$q_n_doses) && length(shared$q_n_doses()) > 0) {
      shared$q_n_doses()
    } else {
      as.numeric(input$n_doses_inputt)
    }
  })
  shared$ttl <- reactive({
    if (!is.null(shared$q_ttl) && length(shared$q_ttl()) > 0) {
      shared$q_ttl()
    } else {
      as.numeric(input$ttl_inputt)
    }
  })
  shared$max_size <- reactive({
    if (!is.null(shared$q_max_size) && length(shared$q_max_size()) > 0) {
      shared$q_max_size()
    } else {
      as.numeric(input$max_size_inputt)
    }
  })
  shared$start_dose <- reactive({
    if (!is.null(shared$q_start_dose) && length(shared$q_start_dose()) > 0) {
      shared$q_start_dose()
    } else {
      as.numeric(input$start_dose_inputt)
    }
  })
  shared$cohort_size <- reactive({
    if (!is.null(shared$q_cohort) && length(shared$q_cohort()) > 0) {
      shared$q_cohort()
    } else {
      as.numeric(input$cohort_inputt)
    }
  })

  # Model-specific variables from configuration tab
  # CRM
  shared$skip_esc_crm <- reactive({as.logical(input$skip_esc_crm_input)})
  shared$skip_deesc_crm <- reactive({as.logical(input$skip_deesc_crm_input)})
  shared$above_target_crm <- reactive({as.logical(input$above_target_input)}) # This isn't used in the sim_crm function
  shared$prior_var_crm <- reactive({as.numeric(input$prior_var_input)})
  shared$stop_n_mtd_crm <- reactive({as.numeric(input$stop_n_mtd_input)})

  shared$prior_mtd_crm <- reactive({as.numeric(input$prior_mtd_input)})  # This isn't used in the sim_crm function
  shared$stop_tox_x_crm <- reactive({as.numeric(input$stop_tox_x_input)})
  shared$stop_tox_y_crm <- reactive({as.numeric(input$stop_tox_y_input)})  

  # BOIN
  shared$boin_stopping_rule <- reactive({as.logical(input$boin_stopping_rule)})
  shared$boin_cohorts <- reactive({as.numeric(input$boin_cohorts)})
  shared$stop_n_mtd_boin <- reactive({as.numeric(input$stop_n_mtd_boin)})
  
  shared$phi_1 <- 0.18
  shared$phi_2 <- 0.42

  observeEvent(input$ttl_multiple_phi_1, {
    shared$phi_1 <- as.numeric(input$ttl_multiple_phi_1) * shared$ttl()
    updateNumericInput(session, "direct_phi_1", value = shared$phi_1)
    lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)
    updateNumericInput(session, "direct_lambda_e", value = lambda$lambda_e)
  })
  observeEvent(input$ttl_multiple_phi_2, {
    shared$phi_2 <- as.numeric(input$ttl_multiple_phi_2) * shared$ttl()
    updateNumericInput(session, "direct_phi_2", value = shared$phi_2)
   lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)
    updateNumericInput(session, "direct_lambda_d", value = lambda$lambda_d)
  })
  observeEvent(input$direct_phi_1, {
    shared$phi_1 <- as.numeric(input$direct_phi_1)
    updateNumericInput(session, "ttl_multiple_phi_1", value = shared$phi_1 / shared$ttl())
    lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)
    updateNumericInput(session, "direct_lambda_e", value = lambda$lambda_e)
  })
  observeEvent(input$direct_phi_2, {
    shared$phi_2 <- as.numeric(input$direct_phi_2)
    updateNumericInput(session, "ttl_multiple_phi_2", value = shared$phi_2 / shared$ttl())
     lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)
    updateNumericInput(session, "direct_lambda_d", value = lambda$lambda_d)
  })
  observeEvent(input$lambda_e, {
    shared$phi_1 <- boin_probs(shared$ttl(), as.numeric(input$lambda_e), 1)
    updateNumericInput(session, "direct_phi_1", value = shared$phi_1)
    updateNumericInput(session, "ttl_multiple_phi_1", value = shared$phi_1 / shared$ttl())
  })
  observeEvent(input$lambda_d, {
    shared$phi_2 <- boin_probs(shared$ttl(), as.numeric(input$lambda_d), 2)
    updateNumericInput(session, "direct_phi_2", value = shared$phi_2)
    updateNumericInput(session, "ttl_multiple_phi_2", value = shared$phi_2 / shared$ttl())
  })

  observeEvent(input$boin_generate_boundaries, {
   lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)

    output$boin_output_lambda_e <- renderText({ lambda$text_e })
    output$boin_output_lambda_d <- renderText({ lambda$text_d })
  })
  observeEvent(input$boin_generate_boundaries_dir, {
   lambda <- boin_boundaries(target = shared$ttl(), ncohort = shared$boin_cohorts(), 
                                 cohortsize =  shared$cohort_size(), n.earlystop = shared$stop_n_mtd_boin(), 
                                 p.saf = shared$phi_1, p.tox = shared$phi_2)

    output$boin_output_lambda_e_dir <- renderText({ lambda$text_e })
    output$boin_output_lambda_d_dir <- renderText({ lambda$text_d })
  })


  ############################ Validation Checks and Warning Messages ##############################
  validation_state <- reactiveValues(
    n_doses_val = NULL,
    ttl_val = NULL,
    max_size_val = NULL,
    start_dose_val = NULL,
    cohort_val = NULL,

    prior_var_val = NULL,
    stop_n_mtd_val = NULL,
    prior_mtd_val = NULL,
    stop_tox_x_val = NULL,
    stop_tox_y_val = NULL,

    boin_cohorts_val = NULL,
    stop_n_mtd_boin_val = NULL,
    ttl_multiple_phi_1_val = NULL,
    ttl_multiple_phi_2_val = NULL,
    direct_phi_1_val = NULL,
    direct_phi_2_val = NULL,

    basic_prior_val = NULL
  )
  val_length <- reactive({
    length(validation_state)
  })
  
  # Define base validation rules for each input
  base_validation_rules <- list(
    n_doses_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    ttl_val = list(min_val = 0, max_val = 0.999, integer_only = FALSE),
    max_size_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    start_dose_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    cohort_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),

    prior_var_val = list(min_val = 0, max_val = 1, integer_only = FALSE),
    stop_n_mtd_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    prior_mtd_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    stop_tox_x_val = list(min_val = 0, max_val = 1, integer_only = FALSE),
    stop_tox_y_val = list(min_val = 0, max_val = 1, integer_only = FALSE),

    boin_cohorts_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    stop_n_mtd_boin_val = list(min_val = 1, max_val = NULL, integer_only = TRUE),
    ttl_multiple_phi_1_val = list(min_val = 0, max_val = 1, integer_only = FALSE),
    ttl_multiple_phi_2_val = list(min_val = 1, max_val = NULL, integer_only = FALSE),
    direct_phi_1_val = list(min_val = 0, max_val = 0.999, integer_only = FALSE),
    direct_phi_2_val = list(min_val = 0, max_val = 0.999, integer_only = FALSE),

    basic_prior_val = list(min_val = 1, max_val = NULL, integer_only = TRUE)
  )
  
  ##### Dynamic validation rules
  validation_rules <- reactive({
    rules <- base_validation_rules
    # start_dose <= n_doses
    if (!is.null(input$n_doses_inputt) && !is.na(input$n_doses_inputt) && input$n_doses_inputt > 0) {
      rules$start_dose_val$max_val <- input$n_doses_inputt
    }
    # cohort <= max_size
    if (!is.null(input$max_size_inputt) && !is.na(input$max_size_inputt) && input$max_size_inputt > 0) {
      rules$cohort_val$max_val <- input$max_size_inputt
    }
    # stop_n_mtd <= max_size
    if (!is.null(input$max_size_inputt) && !is.na(input$max_size_inputt) && input$max_size_inputt > 0) {
      rules$stop_n_mtd_val$max_val <- input$max_size_inputt
    }
    # prior_mtd <= n_doses
    if (!is.null(input$n_doses_inputt) && !is.na(input$n_doses_inputt) && input$n_doses_inputt > 0) {
      rules$prior_mtd_val$max_val <- input$n_doses_inputt
    }
    if (!is.null(input$n_doses_inputt) && !is.na(input$n_doses_inputt) && input$n_doses_inputt > 0) {
      rules$basic_prior_val$max_val <- input$n_doses_inputt
    }
    # boin_cohorts <= max_size/cohort
    if (!is.null(input$cohort_inputt) && !is.na(input$cohort_inputt) && input$cohort_inputt > 0) {
      rules$boin_cohorts_val$max_val <- input$max_size_inputt / input$cohort_inputt
    }
    # stop_n_mtd_boin <= max_size
    if (!is.null(input$max_size_inputt) && !is.na(input$max_size_inputt) && input$max_size_inputt > 0) {
      rules$stop_n_mtd_boin_val$max_val <- input$max_size_inputt
    }
    # ttl_multiple_phi_2 <= 1/ttl
    if (!is.null(input$ttl_inputt) && !is.na(input$ttl_inputt) && input$ttl_inputt > 0) {
      rules$ttl_multiple_phi_2_val$max_val <- 1 / input$ttl_inputt
    }
    # direct_phi_1 < ttl
    if (!is.null(input$ttl_inputt) && !is.na(input$ttl_inputt) && input$ttl_inputt > 0) {
      rules$direct_phi_1_val$max_val <- input$ttl_inputt
    }
    # direct_phi_2 > ttl
    if (!is.null(input$ttl_inputt) && !is.na(input$ttl_inputt) && input$ttl_inputt > 0) {
      rules$direct_phi_2_val$min_val <- input$ttl_inputt
    }
    return(rules)
  })

  
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
  # General Inputs
  observe({
    update_validation("n_doses_val", input$n_doses_inputt)
    # When n_doses changes, re-validate start_dose and prior_mtd to show warning if needed
    if (!is.null(input$start_dose_inputt)) {
      update_validation("start_dose_val", input$start_dose_inputt)
    }
    if (!is.null(input$prior_mtd_input)) {
      update_validation("prior_mtd_val", input$prior_mtd_input)
    }
    if (!is.null(input$basic_prior_mtd_input)) {
      update_validation("basic_prior_val", input$basic_prior_mtd_input)
    }
  })

  observe({
    update_validation("ttl_val", input$ttl_inputt)
    # When ttl changes, re-validate phi_1 and phi_2 to show warning if needed
    if (!is.null(input$ttl_multiple_phi_2)) {
      update_validation("ttl_multiple_phi_2_val", input$ttl_multiple_phi_2)
    }
    if (!is.null(input$direct_phi_1)) {
      update_validation("direct_phi_1_val", input$direct_phi_1)
    }
    if (!is.null(input$direct_phi_2)) {
      update_validation("direct_phi_2_val", input$direct_phi_2)
    }
  })

    observe({
    update_validation("max_size_val", input$max_size_inputt)
    # When max_size changes, re-validate cohort and stop_n_mtd to show warning if needed
    if (!is.null(input$cohort_inputt)) {
      update_validation("cohort_val", input$cohort_inputt)
    }
    if (!is.null(input$stop_n_mtd_input)) {
      update_validation("stop_n_mtd_val", input$stop_n_mtd_input)
    }
    if (!is.null(input$boin_cohorts)) {
      update_validation("boin_cohorts_val", input$boin_cohorts)
    }
    if (!is.null(input$stop_n_mtd_boin)) {
      update_validation("stop_n_mtd_boin_val", input$stop_n_mtd_boin)
    }
  })

  observe({
    update_validation("basic_max_size_val", input$basic_max_size_inputt)
    # When max_size changes, re-validate cohort and stop_n_mtd to show warning if needed
    if (!is.null(input$basic_cohort_inputt)) {
      update_validation("basic_cohort_val", input$basic_cohort_inputt)
    }
  })

  observe({
    update_validation("start_dose_val", input$start_dose_inputt)
  })
  
  observe({
    update_validation("cohort_val", input$cohort_inputt)
  })
  
  # CRM Inputs
  observe({
    update_validation("prior_var_val", input$prior_var_input)
  })

  observe({
    update_validation("basic_prior_val", input$basic_prior_mtd_input)
  })

  observe({
    update_validation("stop_n_mtd_val", input$stop_n_mtd_input)
  })

  observe({
    update_validation("prior_mtd_val", input$prior_mtd_input)
  })

  observe({
    update_validation("stop_tox_x_val", input$stop_tox_x_input)
  })

  observe({
    update_validation("stop_tox_y_val", input$stop_tox_y_input)
  })

  # BOIN Inputs
  observe({
    update_validation("boin_cohorts_val", input$boin_cohorts)
  })
  observe({
    update_validation("stop_n_mtd_boin_val", input$stop_n_mtd_boin)
  })
  observe({
    update_validation("ttl_multiple_phi_1_val", input$ttl_multiple_phi_1)
  })
  observe({
    update_validation("ttl_multiple_phi_2_val", input$ttl_multiple_phi_2)
  })
  observe({
    update_validation("direct_phi_1_val", input$direct_phi_1)
  })
  observe({
    update_validation("direct_phi_2_val", input$direct_phi_2)
  })

  # Render individual warning messages next to each input
  output$n_doses_warning <- renderText({
    validation_state$n_doses_val_warning %||% ""
  })

  output$ttl_warning <- renderText({
    validation_state$ttl_val_warning %||% ""
  })

  output$max_size_warning <- renderText({
    validation_state$max_size_val_warning %||% ""
  })

  output$start_dose_warning <- renderText({
    validation_state$start_dose_val_warning %||% ""
  })
  
  output$cohort_warning <- renderText({
    validation_state$cohort_val_warning %||% ""
  })

  # CRM 

  output$prior_var_warning <- renderText({
    validation_state$prior_var_val_warning %||% ""
  })

    output$basic_prior_warning <- renderText({
    validation_state$basic_prior_val_warning %||% ""
  })

  output$stop_n_mtd_warning <- renderText({
    validation_state$stop_n_mtd_val_warning %||% ""
  })

  output$prior_mtd_warning <- renderText({
    validation_state$prior_mtd_val_warning %||% ""
  })

  output$stop_tox_x_warning <- renderText({
    validation_state$stop_tox_x_val_warning %||% ""
  })

  output$stop_tox_y_warning <- renderText({
    validation_state$stop_tox_y_val_warning %||% ""
  })

  # BOIN

  output$boin_cohorts_warning <- renderText({
    validation_state$boin_cohorts_val_warning %||% ""
  })

  output$stop_n_mtd_boin_warning <- renderText({
    validation_state$stop_n_mtd_boin_val_warning %||% ""
  })

  output$ttl_multiple_phi_1_warning <- renderText({
    validation_state$ttl_multiple_phi_1_val_warning %||% ""
  })

  output$ttl_multiple_phi_2_warning <- renderText({
    validation_state$ttl_multiple_phi_2_val_warning %||% ""
  })

  output$direct_phi_1_warning <- renderText({
    validation_state$direct_phi_1_val_warning %||% ""
  })

  output$direct_phi_2_warning <- renderText({
    validation_state$direct_phi_2_val_warning %||% ""
  })

  ################################ Basic Mode ################################

  output$mode_title <- renderText({
    if (input$modes == 1) {
      "Trial Design - Basic Mode"
    } else {
      "Trial Design - Advanced Mode"
    }
  })

  observeEvent(input$transfer_advanced, {
    if (!is.null(validation_state$basic_prior_val) | 
        !is.null(validation_state$n_doses_val) |
        !is.null(validation_state$ttl_val) |
        !is.null(validation_state$max_size_val) |
        !is.null(validation_state$start_dose_val) |
        !is.null(validation_state$cohort_val) ) {
      showNotification("Please correct the incorrect values before transferring to advanced mode.", type = "error")
      return()
    } else{
    # Update model-specific shared variables with basic mode inputs
    updateRadioButtons(session, "skip_esc_crm_input", selected = input$basic_skip_esc_input)
    updateRadioButtons(session, "skip_deesc_crm_input", selected = input$basic_skip_deesc_input)
    updateNumericInput(session, "prior_mtd_input", value = input$basic_prior_mtd_input)

    # Updating other advanced inputs to make sense with basic mode variables
    stop_number <- ceiling(shared$max_size()/2)
    num_cohorts <- floor(shared$max_size()/shared$cohort_size())

    updateNumericInput(session, "stop_n_mtd_input", value = stop_number)
    updateNumericInput(session, "stop_n_mtd_boin", value = stop_number)
    updateNumericInput(session, "boin_cohorts", value = num_cohorts)
    }
  })

  # This is clunky - needs to be changed to be more elegant.
  observeEvent(input$n_doses_inputt, {
    shared$n_dosess <- reactive({as.numeric(input$n_doses_inputt)})
  })

  observeEvent(input$ttl_inputt, {
    shared$ttl <- reactive({as.numeric(input$ttl_inputt)})
  })

  observeEvent(input$max_size_inputt, {
    shared$max_size <- reactive({as.numeric(input$max_size_inputt)})
  })

  observeEvent(input$start_dose_inputt, {
    shared$start_dose <- reactive({as.numeric(input$start_dose_inputt)})
  })

  observeEvent(input$cohort_inputt, {
    shared$cohort_size <- reactive({as.numeric(input$cohort_inputt)})
  })

  ## Moving to Simulation Tab

 observe({
  # Convert reactiveValues to a regular list
  validation_values <- reactiveValuesToList(validation_state)

  # Remove elements whose names or values contain "warning"
  filtered <- validation_values[!grepl("warning", names(validation_values))]

  # Count how many of the remaining elements are not NULL
  shared$td_warnings <- sum(!sapply(filtered, is.null))
})

  observeEvent(input$view_simulation, {
    # Validate all inputs before proceeding
    validation_errors <- sapply(names(validation_state), function(x) validation_state[[x]])
    validation_errors <- validation_errors[!grepl("warning", names(validation_errors))]
    validation_errors <- validation_errors[!grepl("basic", names(validation_errors))]
    validation_errors <- Filter(Negate(is.null), validation_errors)

      if (length(validation_errors) > 0) {
      names <- vector("list", 3)
      crm_names_var <- grep("var_val", names(validation_errors))
      crm_names_mtd <- grep("mtd_val", names(validation_errors))
      crm_names_tox <- grep("stop_tox", names(validation_errors))

      boin_names <- grep("boin", names(validation_errors))
      boin_names_phi <- grep("phi", names(validation_errors))

      if (length(crm_names_var) == 0 && length(crm_names_mtd) == 0 && length(crm_names_tox) == 0
          && length(boin_names) == 0 && length(boin_names_phi) == 0) {
          general_names <- names(validation_errors)
          } else {
      general_names <- validation_errors[-c(crm_names_var, crm_names_mtd, crm_names_tox, boin_names, boin_names_phi)]
          }
      if (length(general_names) > 0) {
        names[[1]] <- "General Parameters"
      } else {
        names[[1]] <- NULL
      }
      if (length(crm_names_var) > 0 || length(crm_names_mtd) > 0 || length(crm_names_tox) > 0) {
        names[[2]] <- "CRM Parameters"
      } else {
        names[[2]] <- NULL
      }
      if (length(boin_names) > 0 || length(boin_names_phi) > 0) {
        names[[3]] <- "BOIN Parameters"
      } else {
        names[[3]] <- NULL
      }
      names <- Filter(Negate(is.null), names)
      if (1 %in% input$modes & (input$basic_prior_mtd_input != input$prior_mtd_input | 
                                       input$basic_skip_esc_input != input$skip_esc_crm_input |
                                       input$basic_skip_deesc_input != input$skip_deesc_crm_input)) {
        showNotification(paste("Please click the 'Transfer Results to Advanced Mode' button to update the advanced parameters with the basic mode inputs."), type = "warning")
      } else if (1 %in% input$modes & (length(crm_names_var) + length(crm_names_mtd) + length(boin_names) > 0)) {
         showNotification(paste("Please click the 'Transfer Results to Advanced Mode' button to update the advanced parameters with the basic mode inputs."), type = "warning")
      } else {
        showNotification(paste("Please change incorrect values, found in the following input areas:", paste(names, collapse = ", ")), type = "error")
    } }  else {
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "nav", selected = "Simulation")
      }
    }
  })

} # End of function within moduleServer
) # End of moduleServer
} # End of trial_design_server function