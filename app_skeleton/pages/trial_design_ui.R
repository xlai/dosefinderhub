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
# textInput(ns("skeleton_input"), "What are the prior estimates of the DLT rates at each dose? Please make this an increasing list and separate each value with a comma.", value = "0.108321683015674,0.255548628279939,0.425089891767129,0.576775912195444,0.817103320499882")
prior_mtd_input <- numericInput(ns("prior_mtd_input"), "What is your prior guess of the MTD?", min = 1, value = 8)
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

# 3+3 specific inputs
skip_tpt_input <- radioButtons(ns("skip_tpt_input"),"Would you like to be able to skip doses when de-escalating?",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)

# Other specific inputs (placeholder)
specific_ui_inputs_other <- radioButtons(ns("specific_ui_inputs_other"),"This is a placeholder. Clicking this button will do nothing.",
choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE)

########################################### Running the UI ###########################################


  page_sidebar( 
      
   
   # General Trial Design Parameters
     layout_column_wrap(  
     card(full_screen = TRUE,
      card_header("General Trial Parameters"),
      card_body(
       checkboxInput("display_input_all", "Display parameters", value = F),
       conditionalPanel(condition = "input.display_input_all==1", non_specific_ui_inputts)
     )
      )),

      #Specific Trial Design Parameters
       layout_column_wrap(  
        card( full_screen = TRUE,
          card_header("CRM Parameters"),
          card_body(
           checkboxInput("display_crm", "Display parameters", value = F),
           conditionalPanel(condition = "input.display_crm==1", specific_ui_inputs_crm)
          )
        ),
        card(full_screen = TRUE,
          card_header("3+3 Parameters"),
          card_body(
          checkboxInput("display_tpt", "Display parameters", value = F),
           conditionalPanel(condition = "input.display_tpt==1", skip_tpt_input)
          )
        ),
        card(full_screen = TRUE,
          card_header("Other (TEST) Parameters"),
          card_body(
            checkboxInput("display_other", "Display parameters", value = F),
           conditionalPanel(condition = "input.display_other==1", specific_ui_inputs_other)
          )
        )
      ),
    sidebar = sidebar(
      h3("Trial Design"),
      h4("Transfer Questionnaire Results"),
      p("Press the button below to transfer your results from the questionnaire to the trial design inputs. This will allow you to run simulations and conduct based on your questionnaire responses. Note that you must press Generate Recommendation to save your questionnaire responses."),
      input_task_button(ns("transfer_questionnaire"), "Transfer Questionnaire Results"),

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

trial_design_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

##### Reactive Skeleton Input ######
reactive_skeleton <- reactiveVal() # initalising a reactive value to store the data frame

  observeEvent({input$n_doses_inputt}, {
    if (is.na(input$n_doses_inputt)) {
      reactive_skeleton(NULL)
    } else {
  dose <- as.integer(input$n_doses_inputt)
  large_vector <- c(2:(dose + 1))
  Prior <- large_vector/(dose + 2)

  rownames_skel <-paste("d", 1:dose, sep = "")

  df <- data.frame(Dose = rownames_skel, Prior = Prior)

  reactive_skeleton(df) # Updating the reactive value with the new data frame
   } })
  
  output$skeleton_df <- renderDT({
    datatable(reactive_skeleton(), editable = TRUE, rownames = FALSE, options = list(searching = FALSE, paging = FALSE, info = FALSE)) #, scrollX = TRUE, scrollX="250px", paging = FALSE
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
observeEvent({input$transfer_questionnaire}, {
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

  # General variables from configuration tab 
  shared$n_dosess <- reactive({as.numeric(input$n_doses_inputt)}) # Using double ending letters to avoid mixing up with other input (for now)
  shared$ttl <- reactive({as.numeric(input$ttl_inputt)})
  shared$max_size <- reactive({as.numeric(input$max_size_inputt)})
  shared$start_dose <- reactive({as.numeric(input$start_dose_inputt)}) 
  shared$cohort_size <- reactive({as.numeric(input$cohort_inputt)})

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

  # 3+3
  shared$skip_tpt <- reactive({as.logical(input$skip_tpt_input)})

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
    stop_tox_y_val = NULL
  )
  
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
    stop_tox_y_val = list(min_val = 0, max_val = 1, integer_only = FALSE)
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
  })
  
  observe({
    update_validation("ttl_val", input$ttl_inputt)
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

} # End of function within moduleServer
) # End of moduleServer
} # End of trial_design_server function