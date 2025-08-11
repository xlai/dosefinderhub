library(shiny)
library(bslib)
library(DT)
library(shinydashboard)

# ---------------- UI MODULE ----------------
con_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      radioButtons(
        inputId = ns("choice"),
        label = "Select which design to update during trial conduct:",
        choices = c("3+3", "CRM", "Other"),
        inline = FALSE
      ),
      actionButton(ns("update_design"), "Update Design"),
      fileInput(ns("file_upload"), "Upload Previous Responses:", accept = c(".csv", ".rds")),
      downloadButton(ns("con_save_button"), "Save Responses")
    ),
    layout_columns(
      value_box(
        title = "Current Number of Patients",
        value = textOutput(ns("patient_count")),
        showcase = bsicons::bs_icon("people-fill"),
        theme_color = "primary"
      ),
      value_box(
        title = "Latest Dose Level",
        value = textOutput(ns("latest_dose")),
        showcase = bsicons::bs_icon("capsule"),
        theme_color = "success"
      ),
      value_box(
        title = "Recommended Next Dose Level",
        value = textOutput(ns("recommended_dose")),
        showcase = bsicons::bs_icon("arrow-up-circle"),
        theme_color = "warning"
      )
    ),
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Data"),
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
          textInput(ns("plot_title"), "Graph Title:", value = "Cohort Grouped Patient Dose Level with DLT's"),
          actionButton(ns("generate_plot"), "Generate Graph"),
          uiOutput(ns("dose_plot_ui")),
          actionButton(ns("reset_title"), "Reset Title")
        )
      )
      ),
      uiOutput(ns("results_card_ui"))
  )
}


# ---------------- SERVER MODULE ----------------
con_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive table data
    conduct_reactive_table_data <- reactiveVal(
      data.frame(
        No.Cohort = integer(0),
        Dose_Level = numeric(0),
        DLT = logical(0),
        stringsAsFactors = FALSE
      )
    )
    
    # Reactive flag to show CRM card
    show_crm_card <- reactiveVal(FALSE)
    
    output$results_card_ui <- renderUI({
      if (!show_crm_card()) return(NULL)
      card(
        full_screen = TRUE,
        card_header("Results"),
        card_body(
          p("This card appears when CRM is selected and 'Update Design' is clicked.")
        )
      )
    })

    # Initial table generation
    observe({
      req(shared$max_size, shared$cohort)
      initial_cohorts <- ceiling(shared$max_size / shared$cohort)
      total_patients <- initial_cohorts * shared$cohort

      new_rows <- data.frame(
        No.Cohort = rep(1:initial_cohorts, each = shared$cohort)[1:total_patients],
        Dose_Level = rep(1:initial_cohorts, each = shared$cohort)[1:total_patients],
        DLT = rep(FALSE, total_patients),
        stringsAsFactors = FALSE
      )
      conduct_reactive_table_data(new_rows)
    })

    # Add cohort
    observeEvent(input$add_cohort, {
      data <- conduct_reactive_table_data()
      current_cohorts <- if (nrow(data) == 0) 0 else max(data$No.Cohort)
      new_cohort_number <- current_cohorts + 1

      # Determine recommended dose level
      if (nrow(data) == 0) {
      recommended_dose <- 1  # Default starting dose
      } else {
      latest_cohort <- max(data$No.Cohort, na.rm = TRUE)
      cohort_data <- data[data$No.Cohort == latest_cohort, ]
      current_dose <- unique(cohort_data$Dose_Level)

      if (length(current_dose) != 1 || is.na(current_dose)) {
      recommended_dose <- 1
     } else if (any(cohort_data$DLT)) {
      recommended_dose <- max(current_dose - 1, 1)
     } else {
      recommended_dose <- current_dose + 1
     }}
     new_rows <- data.frame(
       No.Cohort = rep(new_cohort_number, 3),
       Dose_Level = rep(recommended_dose, 3),
       DLT = rep(FALSE, 3),
       stringsAsFactors = FALSE
     )
     updated_data <- rbind(data, new_rows)
     conduct_reactive_table_data(updated_data)
    })


    # Remove cohort
    observeEvent(input$remove_cohort, {
      data <- conduct_reactive_table_data()
      if (nrow(data) <= 3 || is.na(max(data$No.Cohort))) return()

      last_cohort <- max(data$No.Cohort)
      updated_data <- data[data$No.Cohort != last_cohort, ]
      conduct_reactive_table_data(updated_data)
    })

    # Update design button logic
    observeEvent(input$update_design, {
      if (input$choice == "CRM") {
        show_crm_card(TRUE)
      } else {
        show_crm_card(FALSE)
      }
    })

   output$latest_dose <- renderText({
   data <- conduct_reactive_table_data()
   if (nrow(data) == 0) return("N/A")
   # Find the most recent cohort
   latest_cohort <- max(data$No.Cohort, na.rm = TRUE)
   # Get the dose level for that cohort
   latest_dose <- unique(data$Dose_Level[data$No.Cohort == latest_cohort])
   # If multiple dose levels exist for the cohort, show them all (there should be only one)
   paste(latest_dose, collapse = ", ")
   })

   output$recommended_dose <- renderText({
     data <- conduct_reactive_table_data()
     if (nrow(data) == 0) return("N/A")
     latest_cohort <- max(data$No.Cohort, na.rm = TRUE)
     cohort_data <- data[data$No.Cohort == latest_cohort, ]
     current_dose <- unique(cohort_data$Dose_Level)
     if (length(current_dose) != 1 || is.na(current_dose)) return("N/A")
     if (any(cohort_data$DLT)) {
     recommended <- max(current_dose - 1, 1)  # Ensure dose doesn't go below 1
     } else {
     recommended <- current_dose + 1
     }
     recommended
    })

    output$patient_count <- renderText({
      data <- conduct_reactive_table_data()
      nrow(data)
    })

    # Editable table
    output$editable_table <- renderDT({
      datatable(
        conduct_reactive_table_data(),
        editable = list(target = "cell", disable = list(columns = c(0))),
        rownames = FALSE,
        options = list(
           pageLength = nrow(conduct_reactive_table_data()),  # Show all rows
           dom = 'fti',  
           columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        )
      )
    }, server = TRUE)

    # Table cell edit logic — update entire cohort's dose level
    observeEvent(input$editable_table_cell_edit, {
      info <- input$editable_table_cell_edit
      data <- conduct_reactive_table_data()
      col_name <- colnames(data)[info$col + 1]

      if (col_name == "Dose_Level") {
        cohort_number <- data$No.Cohort[info$row]
        new_value <- as.numeric(info$value)
        data$Dose_Level[data$No.Cohort == cohort_number] <- new_value
      } else if (col_name == "DLT") {
        new_value <- as.logical(info$value)
        data[info$row, col_name] <- new_value
      } else {
        data[info$row, col_name] <- info$value
      }

      conduct_reactive_table_data(data)
    })

    # Plot generation
    
   observeEvent(input$generate_plot, {
   output$dose_plot_ui <- renderUI({
    plotOutput(ns("dose_plot"), height = "400px")
    })

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
     main = input$plot_title,
     xaxt = "n",
     ylim = ylim
     )
     axis(1, at = sort(unique(data$No.Cohort)), labels = sort(unique(data$No.Cohort)))
     text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
     legend("bottom", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19)
    })
   })

    observeEvent(input$reset_title, {
    updateTextInput(
     session,
     "plot_title",
     value = "Cohort Grouped Patient Dose Level with DLT's"
     )
    })

    # CRM card output
    output$crm_card_ui <- renderUI({
      if (!show_crm_card()) return(NULL)

      card(
        full_screen = TRUE,
        card_header("CRM Design Details"),
        card_body(
          p("This card appears when CRM is selected and 'Update Design' is clicked."),
          # Add CRM-specific UI elements here
          verbatimTextOutput(ns("crm_info"))
        )
      )
    })

    output$crm_info <- renderText({
      "CRM design logic and parameters would be shown here."
    })
  })
}