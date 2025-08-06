#library(shiny)
#library(shiny.semantic)
library(bslib)
library(shiny)
library(htmltools)
library(plotly)
library(leaflet)
library(DT)

# Define your manual dataset
manual_data_results <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(29, 34, 41),
  Role = c("Data Scientist", "Statistician", "Researcher")
)
manual_data_overview <- data.frame(
  Name = c("Jim", "Sid", "James"),
  Age = c(29, 26, 41),
  Role = c("Data Scientist", "Statistician", "Researcher")
)

plotly_widget <- plot_ly(x = diamonds$cut) %>%
  config(displayModeBar = FALSE) %>%
  layout(margin = list(t = 0, b = 0, l = 0, r = 0))

con_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_column_wrap(
      width = 1/2,
      height = 300,
      card(
        full_screen = TRUE,
        card_header("Results"),
        card_body(
          tagList(
            plotly_widget,
            DTOutput(ns("manual_table_results"))
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Overview"),
        card_body(
          tagList(
            plotly_widget,
            DTOutput(ns("manual_table_overview"))
          )
        )
      )
    ),
      sidebar = sidebar(
        radioButtons(
          inputId = ns("choice"),
        label = "Select which design to update during trial conduct:",
        choices = c("CRM", "3+3", "Other"),
        inline = FALSE
      ),
      fileInput(ns("file_upload"), "Upload Previous Responses:", accept = c(".csv", ".rds")),
      downloadButton(ns("save_button"), "Save Responses"),
      actionButton(ns("trial_design_share"), "Use Trial Design Input")
    )
  )
}

con_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$manual_table_results <- renderDT({
      datatable(manual_data_results)
    })
    output$manual_table_overview <- renderDT({
      datatable(manual_data_overview)
    })
  })
}