library(shiny)
library(bslib)
library(DT)
library(shinydashboard)
#actionButton(ns("complete_table"), "Complete Table")
#actionButton("complete_graph", "Complete Graph")
#uiOutput("download_ui")  # This will conditionally show the download button

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
      radioButtons(ns("export_type"), "Choose Export Format:",
             choices = c("PDF", "Excel"),
             inline = TRUE),
     downloadButton(ns("download_report"), "Download Report")
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


################################### SERVER MODULE ##########################
con_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive table data
    conduct_reactive_table_data <- reactiveVal(
      data.frame(
        Patient_Number = integer(0),
        No.Cohort = integer(0),
        Dose_Level = numeric(0),
        DLT = logical(0),
        stringsAsFactors = FALSE
      )
    )

    # Initial table generation
    observe({
      req(shared$max_size, shared$cohort)
      initial_cohorts <- ceiling(shared$max_size / shared$cohort)
      total_patients <- initial_cohorts * shared$cohort

      new_rows <- data.frame(
        Patient_Number = seq_len(total_patients),
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

      if (nrow(data) == 0) {
        recommended_dose <- 1
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
        }
      }

      start_patient <- nrow(data) + 1
      new_rows <- data.frame(
        Patient_Number = seq(from = start_patient, length.out = 3),
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
      show_crm_card(input$choice == "CRM")
    })

    output$latest_dose <- renderText({
      data <- conduct_reactive_table_data()
      if (nrow(data) == 0) return("N/A")
      latest_cohort <- max(data$No.Cohort, na.rm = TRUE)
      latest_dose <- unique(data$Dose_Level[data$No.Cohort == latest_cohort])
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
        recommended <- max(current_dose - 1, 1)
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
          pageLength = nrow(conduct_reactive_table_data()),
          dom = 'fti',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      )
    }, server = TRUE)

    # Table cell edit logic
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

        data$Patient <- data$Patient_Number
        data$Cohort_Position <- ave(data$No.Cohort, data$No.Cohort, FUN = seq_along)
        data$X <- data$No.Cohort + (data$Cohort_Position - 2) * 0.2
        colors <- ifelse(data$DLT, "red", "green")
        ylim <- c(0.5, max(data$Dose_Level) + 0.5)

       # Set layout: 1 row, 2 columns (plot + legend)
layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))  # 4:1 ratio

# Plot area
par(mar = c(5, 4, 4, 1))  # Normal margins
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

# Legend area
par(mar = c(0, 0, 0, 0))  # No margins
plot.new()
legend("center", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19, cex = 1.2, bty = "n")
      })
    })

    observeEvent(input$reset_title, {
      updateTextInput(session, "plot_title", value = "Cohort Grouped Patient Dose Level with DLT's")
    })

    show_crm_card <- reactiveVal(FALSE)

    output$results_card_ui <- renderUI({
      if (!show_crm_card()) return(NULL)
      card(
        full_screen = TRUE,
        card_header("Results"),
        card_body(p("This card appears when CRM is selected and 'Update Design' is clicked."))
      )
    })

    ################################### Rmd file generation #########################################################
    output$download_report <- downloadHandler(
      filename = function() {
        ext <- if (input$export_type == "PDF") ".pdf" else ".xlsx"
        paste0("cohort_report_", Sys.Date(), ext)
      },
      content = function(file) {
        data <- conduct_reactive_table_data()

        # Common plot logic
        plot_file <- tempfile(fileext = ".png")
        png(plot_file, width = 800, height = 600)
        data$Patient <- data$Patient_Number
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
        dev.off()

        if (input$export_type == "PDF") {
          rmd_file <- tempfile(fileext = ".Rmd")
          rmd_content <- c(
            "---",
            "title: \"Cohort Report\"",
            "output: pdf_document",
            "---",
            "",
            "## Patient Table",
            "",
            "```{r}",
            "library(knitr)",
            "library(dplyr)",
            "data <- tibble::tibble(",
            paste0("  Patient_Number = c(", paste(data$Patient_Number, collapse = ", "), "),"),
            paste0("  No.Cohort = c(", paste(data$No.Cohort, collapse = ", "), "),"),
            paste0("  Dose_Level = c(", paste(data$Dose_Level, collapse = ", "), "),"),
            paste0("  DLT = c(", paste(as.character(data$DLT), collapse = ", "), ")"),
            ")",
            "kable(data)",
            "```",
            "",
            "## Dose Plot",
            "",
            paste0("!")
          )
          writeLines(rmd_content, rmd_file)
          rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)

        } else if (input$export_type == "Excel") {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Cohort Table")
          openxlsx::writeData(wb, "Cohort Table", data)

          openxlsx::addWorksheet(wb, "Dose Plot")
          openxlsx::insertImage(wb, "Dose Plot", plot_file, startRow = 2, startCol = 2, width = 6, height = 6)

          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
      }
    )
  })
}