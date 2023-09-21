source('app/modules/questionnaire/generate_questions_UI.R')

data <- read.csv("app/data/questionnaire_inputs/method_q_database.csv")
titles <- data.frame(titles = c("3+3", "CRM", "BOIN"), designs = c("tpt", "crm", "boin"))

questionnaireUI <- function(id) {
  ns <- NS(id)
  
  shiny::fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    shiny::titlePanel("Design Parameters"),
    
    shiny::mainPanel(
      shiny::uiOutput(ns("questionsUI")),
      shiny::tags$hr(),
      fluidRow(
        column(8, uiOutput(ns("progress_bar"))),
        column(4, 
              div(style="display: flex; align-items: center;",
                  actionButton(ns("prev_button"), "Previous"), 
                  actionButton(ns("next_or_submit_button"), "Next"),
                  actionButton(ns("reset_button"), "Reset")
              )
        )
      )
    )
  )
}

questionnaireServer <- function(id, ns, design_name, data, currentUI) {
  
  moduleServer(id, function(input, output, session) {

    questions_filtered <- data %>% filter(design == design_name)

    current_index <- shiny::reactiveVal(1)

    # Render the UI for the current question
    output$questionsUI <- shiny::renderUI({
      current_question <- questions_filtered[current_index(), ]
      generate_questions_UI(current_question)
    })


    # Update the progress bar
    output$progress_bar <- renderUI({
      
      progress_value <- (current_index() / nrow(questions_filtered)) * 100
      
      div(style = "width: 100%; display: flex; align-items: center;",  # CSS to vertically center the content
        div(id = "progressBar", class = "progress", style="height: 20px; width: 300px;", 
          div(class = "progress-bar", role = "progressbar", 
              `aria-valuenow` = progress_value, `aria-valuemin` = "0", 
              `aria-valuemax` = "100", style = sprintf("width: %s%%;", progress_value)
          )
        ),
        tags$span(style="margin-left: 10px;", 
                  sprintf("%s/%s questions answered", current_index(), nrow(questions_filtered)))
      )
      
    })

    observe({
      if (current_index() >= nrow(questions_filtered)) {
        updateActionButton(session, ns("next_or_submit_button"), label = "Submit")
    } else {
        updateActionButton(session, ns("next_or_submit_button"), label = "Next")
    }
    })
    observeEvent(input[[ns("next_or_submit_button")]], {
    if (current_index() < nrow(questions_filtered)) {
        current_index(current_index() + 1)
    } else {
        # Revert back to the main UI after submission
        currentUI("main")
        output$dynamicUI <- renderUI({})
    }
    })    
    # Update the current question when the next button is clicked
    shiny::observeEvent(input[[ns("next_button")]], {
      if (current_index() < nrow(questions_filtered)) {    
        new_index <- current_index() + 1
        current_index(new_index)
      }
      }
    )
    shiny::observeEvent(
      input$previous_button, {
        if (current_index() > 1) {
          current_index(current_index() - 1)
        }
      }
    )
    shiny::observeEvent(
      input$reset_button, {
        current_index(1)
      }
    )
    # Observe the Submit button and navigate to main UI
    observeEvent(input$submit, {
      returnToMain()
    })
  })
}

ui <- fluidPage(
  h2("Design Selection"),

  div(style = "display: inline-block; width: 48%; vertical-align: top;", 
    shiny::fileInput("file_upload", "Upload Previous Responses:",
                    accept = c(".csv", ".rds"))
  ),
  div(style = "display: inline-block; width: 40%; vertical-align: top;", 
    shiny::downloadButton("save_button", "Save Responses"),
  ),  
  # Generate design UI components dynamically from the dataframe
  tagList(
    lapply(1:nrow(titles), function(i) {
      div(
        paste(titles$titles[i],"Design"),
        actionButton(paste0("explore_", titles$designs[i]), "Explore")
      )
    })
  ),
  
  uiOutput("dynamicUI")
)


server <- function(input, output, session){
  ns = session$ns
  # Track which UI we are currently showing
  currentUI <- reactiveVal("main")
    observe({
    lapply(titles$designs, function(design) {
        observeEvent(input[[paste0("explore_", design)]], {
        if(currentUI() == "main") {
            output$dynamicUI <- renderUI({
            questionnaireUI(paste0("questionnaire", toupper(design)))
            })
            currentUI(design)
        }
        })
    })
    })

  lapply(titles$designs, function(design) {
  questionnaireServer(
      paste0("questionnaire",toupper(design)), 
      ns, design_name = design, data = data, currentUI
  )
  })
  # Save button
  output$save_button <- shiny::downloadHandler(
    filename = function() {
      paste("user_responses-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      inputs_to_save <- data$q_variable
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for (input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      write.csv(inputs_data_frame, file)
    }
  )    
}