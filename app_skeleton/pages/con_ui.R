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
            DTOutput(ns("editable_table")),
            tags$style(HTML("
              input[type='checkbox'] {
                transform: scale(1.2);
                margin: 5px;
              }
            "))
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

    # Reactive table data with logical DLT
    conduct_reactive_table_data <- reactiveVal(
      data.frame(
        No.Cohort = integer(0),
        Dose_Level = numeric(0),
        DLT = logical(0),  # Logical for checkbox
        Posterior_DLT = numeric(0),
        stringsAsFactors = FALSE
      )
    )

    # Update table based on numeric input, safely handling NULL or empty
    observe({
      target_rows <- input$treated_participants_input

      # Skip update if input is NULL, NA, or not a valid number
      if (is.null(target_rows) || is.na(target_rows) || !is.numeric(target_rows)) return()

      current_data <- conduct_reactive_table_data()
      current_rows <- nrow(current_data)
      rows_to_add <- target_rows - current_rows

      if (rows_to_add > 0) {
        new_rows <- data.frame(
          No.Cohort = integer(rows_to_add),
          Dose_Level = numeric(rows_to_add),
          DLT = logical(rows_to_add),
          Posterior_DLT = numeric(rows_to_add),
          stringsAsFactors = FALSE
        )
        updated_data <- rbind(current_data, new_rows)
      } else {
        updated_data <- head(current_data, target_rows)
      }

      conduct_reactive_table_data(updated_data)
    })

    # Render editable DT table with checkbox for DLT
    output$editable_table <- renderDT({
      datatable(
        conduct_reactive_table_data(),
        editable = TRUE,
        rownames = FALSE,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 2,  # DLT column index (0-based)
              render = JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    return '<input type=\"checkbox\" ' + (data ? 'checked' : '') + '>';",
                "  }",
                "  return data;",
                "}"
              )
            ),
            list(className = 'dt-center', targets = "_all")
          )
        )
      )
    }, server = FALSE)

    # Handle cell edits
    observeEvent(input$editable_table_cell_edit, {
      info <- input$editable_table_cell_edit
      data <- conduct_reactive_table_data()
      col_name <- colnames(data)[info$col + 1]

      # Convert checkbox value to logical
      if (col_name == "DLT") {
        data[info$row, col_name] <- tolower(info$value) %in% c("true", "1", "yes", "checked")
      } else {
        data[info$row, col_name] <- info$value
      }

      conduct_reactive_table_data(data)
    })
  })
}