#library(shiny)
#library(shiny.semantic)


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
          DTOutput(ns("my_table"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Overview"),
        card_body("here is a map")
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
      downloadButton(ns("save_button"), "Save Responses")
    )
  )
}

con_server <- function(input, output, session) {
  output$my_table <- renderDT({
    datatable(
      data.frame(
        Name = c("Alice", "Bob", "Charlie"),
        Age = c(25, 30, 35),
        Occupation = c("Engineer", "Doctor", "Artist")
      ),
      options = list(pageLength = 5))
  })
}
