library(shiny)
library(DT) # For datatable rendering
library(gridExtra)

ui <- fluidPage(
  
  titlePanel("Simulation tab"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("designInput", "Choose Design:", choices = NULL, multiple = TRUE),
      selectizeInput("scenarioInput", "Choose Scenario:", choices = NULL, multiple = FALSE),
      selectizeInput("typeInput", "Choose Type:", choices = NULL, multiple = TRUE),
      actionButton("goButton", "Display")
    ),
    mainPanel(
      radioButtons("displayMode", "Display Mode:", choices = c("Table", "Graph"), selected = "Table"),
      uiOutput("dataContent")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "designInput", choices = unique(tidy_results$Design))
    updateSelectizeInput(session, "scenarioInput", choices = unique(tidy_results$Scenario))
    updateSelectizeInput(session, "typeInput", choices = unique(tidy_results$Type))
  })
  output$dataContent <- renderUI({
    if (input$displayMode == "Table") {
      DTOutput("dataTable")
    } else {
      plotOutput("dataPlot")
    }
  })


  output$dataPlot <- renderPlot({
    req(input$goButton)

    # Extract selections
    selected_design <- input$designInput
    selected_type <- input$typeInput # Assuming only one type is selected at a time for the histogram

    # Generate the histograms using your function for each type
    list_of_histograms <- lapply(selected_type, function(type) {
      generate_histogram(
        tidy_results %>% filter(Dose != 'NoDose' | is.na(Dose)), 
        type, selected_design
      )
    })

    # Display the plots
    if (length(list_of_histograms) == 1) {
      print(list_of_histograms[[1]])
    } else {
      do.call(grid.arrange, list_of_histograms)
    }

  })
  
  output$dataTable <- renderDT({
    req(input$goButton)
    
    isolate({
      filtered_data <- tidy_results %>% 
        filter(
          Design %in% input$designInput,
          Scenario == input$scenarioInput,
          Type %in% input$typeInput
        ) %>%
        select(-Scenario) %>% # Remove Scenario column
        tidyr::pivot_wider(names_from = Dose, values_from = Value)
      
      # Formatting data to display only up to 2 decimal places
      cols <- names(filtered_data)[-c(1, 2)]
      for (col in cols) {
        filtered_data[[col]] <- round(filtered_data[[col]], 2)
      }

      # Displaying table with a custom title, row grouping, and fixed width
      datatable(
        filtered_data,
        caption = paste("Scenario:", input$scenarioInput),
        extensions = 'RowGroup',
        options = list(
          rowGroup = list(dataSrc = 1), # Group by the "Type" column
          columnDefs = list(list(width = '100px', targets = "_all")) # Setting fixed width for all columns
        )
      )
    })
  })
  
}


shinyApp(ui = ui, server = server)
