library(shiny)
library(bslib)
library(DT)

# Simulated shared values (replace with actual shared object in app context)
shared <- reactiveValues(
  dose_level = 0,     # Number of additional cohorts added
  cohort = 3,         # Cohort size
  ttl = NULL,
  max_size = 12,      # Initial sample size
  start_dose = NULL
)

# ---------------- UI MODULE ----------------
con_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    layout_column_wrap(
      width = 1/2,
      height = "auto",
      gap = "1rem",
      card(
        full_screen = TRUE,
        card_header("Results"),
        card_body(
          div(    
            style = "display: flex; justify-content: flex-end; gap: 10px; margin-bottom: 10px;",
            actionButton(ns("add_cohort"), "Add Cohort"),
            actionButton(ns("remove_cohort"), "Remove Cohort")
          ), 
          DTOutput(ns("editable_table"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Overview"),
        card_body(
          actionButton(ns("generate_plot"), "Generate Graph"),
          plotOutput(ns("dose_plot"), height = "400px")
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

    conduct_reactive_table_data <- reactiveVal(
      data.frame(
        No.Cohort = integer(0),
        Dose_Level = numeric(0),
        DLT = logical(0),
        stringsAsFactors = FALSE
      )
    )

    # Initial table generation based on max_size
    observe({
      req(shared$max_size, shared$cohort)
      initial_cohorts <- ceiling(shared$max_size / shared$cohort)
      total_patients <- initial_cohorts * shared$cohort

      current_data <- conduct_reactive_table_data()
      current_rows <- nrow(current_data)
      rows_to_add <- total_patients - current_rows

      if (rows_to_add > 0) {
        new_rows <- data.frame(
          No.Cohort = rep(1:initial_cohorts, each = shared$cohort)[1:total_patients],
          Dose_Level = rep(1:initial_cohorts, each = shared$cohort)[1:total_patients],
          DLT = rep(FALSE, rows_to_add),
          stringsAsFactors = FALSE
        )
        updated_data <- rbind(current_data, new_rows)
        conduct_reactive_table_data(updated_data)
      }
    })

    # Add cohort button logic
    observeEvent(input$add_cohort, {
      data <- conduct_reactive_table_data()
      current_cohorts <- if (nrow(data) == 0) 0 else max(data$No.Cohort)
      new_cohort_number <- current_cohorts + 1

      new_rows <- data.frame(
        No.Cohort = rep(new_cohort_number, shared$cohort),
        Dose_Level = rep(new_cohort_number, shared$cohort),
        DLT = rep(FALSE, shared$cohort),
        stringsAsFactors = FALSE
      )

      shared$dose_level <- shared$dose_level + 1
      updated_data <- rbind(data, new_rows)
      conduct_reactive_table_data(updated_data)
    })

    # Remove cohort button logic
    observeEvent(input$remove_cohort, {
      data <- conduct_reactive_table_data()
      if (nrow(data) == 0) return()

      last_cohort <- max(data$No.Cohort)
      updated_data <- data[data$No.Cohort != last_cohort, ]
      shared$dose_level <- max(shared$dose_level - 1, 0)
      conduct_reactive_table_data(updated_data)
    })

    output$editable_table <- renderDT({
    datatable(
    conduct_reactive_table_data(),
    editable = list(target = "cell", disable = list(columns = c(0))),
    rownames = FALSE,
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      )
    )
    )
    }, server = TRUE)

    observeEvent(input$editable_table_cell_edit, {
      info <- input$editable_table_cell_edit
      data <- conduct_reactive_table_data()
      col_name <- colnames(data)[info$col + 1]

      if (col_name == "Dose_Level") {
        cohort_number <- data$No.Cohort[info$row]
        new_value <- as.numeric(info$value)
        data$Dose_Level[data$No.Cohort == cohort_number] <- new_value
      } else {
        data[info$row, col_name] <- info$value
      }
      conduct_reactive_table_data(data)
    })

    observeEvent(input$generate_plot, {
      output$dose_plot <- renderPlot({
        data <- conduct_reactive_table_data()
        if (nrow(data) == 0) return(NULL)

        data$Patient <- seq_len(nrow(data))
        data$Cohort_Position <- ave(data$No.Cohort, data$No.Cohort, FUN = seq_along)
        data$X <- data$No.Cohort + (data$Cohort_Position - 2) * 0.2
        colors <- ifelse(data$DLT, "red", "green")
        ylim <- c(0.5, max(data$Dose_Level) + 0.5)

        plot(
          x = data$X,
          y = data$Dose_Level,
          col = colors,
          pch = 19,
          cex = 2,
          xlab = "Cohort",
          ylab = "Dose Level",
          main = "Cohort Grouped Patient Dose Level with DLT's",
          xaxt = "n",
          ylim = ylim
        )
        axis(1, at = sort(unique(data$No.Cohort)), labels = sort(unique(data$No.Cohort)))
        text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
        legend("bottom", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19)
      })
    })
  })
}