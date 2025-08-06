# Load required libraries
library(shiny)
library(bslib)
library(DT)
library(plotly)

# Dummy plotly widget
plotly_widget <- plot_ly(x = diamonds$cut) %>%
  config(displayModeBar = FALSE) %>%
  layout(margin = list(t = 0, b = 0, l = 0, r = 0))

# ---------------- UI MODULE ----------------
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
            numericInput(ns("treated_participants_input"), "Number of Treated Participants", value = 3, min = 0),
            DTOutput(ns("editable_table"))
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
      actionButton(ns("update_design"), "Update Design"),
      fileInput(ns("file_upload"), "Upload Previous Responses:", accept = c(".csv", ".rds")),
      downloadButton(ns("con_save_button"), "Save Responses"),
      actionButton(ns("trial_design_share"), "Use Trial Design Input")
    )
  )
}

# ---------------- SERVER MODULE ----------------
con_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive table data
    conduct_reactive_table_data <- reactiveVal(
      data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, c("No.number", "Dose_level", "DLT", "Posterior_DLT"))))
    )

    # Update table based on numeric input
    observe({
      target_rows <- input$treated_participants_input
      current_data <- conduct_reactive_table_data()
      current_rows <- nrow(current_data)
      rows_to_add <- target_rows - current_rows

      if (rows_to_add > 0) {
        new_rows <- data.frame(
          matrix(0, ncol = 4, nrow = rows_to_add, dimnames = list(NULL, c("No.Cohort", "Dose_Level", "DLT", "Posterior_DLT")))
        )
        updated_data <- rbind(current_data, new_rows)
      } else {
        updated_data <- head(current_data, target_rows)
      }

      conduct_reactive_table_data(updated_data)
    })

    # Render editable DT table
    output$editable_table <- renderDT({
      datatable(
        conduct_reactive_table_data(),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        )
      )
    }, server = FALSE)

    # Handle cell edits
    observeEvent(input$editable_table_cell_edit, {
      info <- input$editable_table_cell_edit
      data <- conduct_reactive_table_data()
      data[info$row, info$col + 1] <- info$value
      conduct_reactive_table_data(data)
    })
  })
}