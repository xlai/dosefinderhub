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
        choices = c("3+3", "CRM", "BOIN"),
        inline = FALSE
      ),
      actionButton(ns("update_design"), "Update Design"),
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
        title = "Trial Status",
        value = textOutput(ns("trial_status")),
        showcase = bsicons::bs_icon("hourglass-split"),
        theme_color = "info"
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
      uiOutput(ns("crm_results_card_ui")),
      uiOutput(ns("boin_results_card_ui"))
  )
}


################################### SERVER MODULE ###############################################################
con_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store CRM model
    crm_model <- reactiveVal(NULL)
    boin_model <- reactiveVal(NULL)
   
    design_initialized <- reactiveVal(FALSE)

    observeEvent(input$update_design, {
     design_initialized(input$choice)

     # Reset table to first cohort
     initial_cohort_size <- shared$cohort_size()
     initial_rows <- data.frame(
       Patient_Number = seq_len(initial_cohort_size),
       Cohort_Number = rep(1, initial_cohort_size),
       Dose_Level = rep(1, initial_cohort_size),
       DLT = rep(FALSE, initial_cohort_size),
       stringsAsFactors = FALSE
      )
     conduct_reactive_table_data(initial_rows)

     # Reset plot
     output$dose_plot_ui <- renderUI(NULL)
     output$dose_plot <- renderPlot(NULL)

     # Initialize model
     if (input$choice == "CRM") {
       target <- shared$ttl()
       skeleton <- shared$skeleton_crm()
       model <- get_dfcrm(skeleton = skeleton, target = target) %>%
       dont_skip_doses(when_escalating = TRUE) %>%
       stop_at_n(n = shared$max_size()) %>%
       stop_when_n_at_dose(dose = "recommended", n = 9)
       crm_model(model)
       boin_model(NULL)
      }

     if (input$choice == "BOIN") {
       target <- shared$ttl()
       num_doses <- shared$n_dosess()
       model <- get_boin(num_doses = num_doses, target = target) %>%
         dont_skip_doses(when_escalating = TRUE) %>%
         stop_at_n(n = shared$max_size()) %>%
       stop_when_n_at_dose(dose = "recommended", n = 9)
       boin_model(model)
       crm_model(NULL)
      }

     show_crm_card(input$choice == "CRM")
     show_boin_card(input$choice == "BOIN")
    })

    # Reactive table data
    conduct_reactive_table_data <- reactiveVal(
      data.frame(
        Patient_Number = integer(0),
        Cohort_Number = integer(0),
        Dose_Level = numeric(0),
        DLT = logical(0),
        stringsAsFactors = FALSE
      )
    )

    # Initial table generation
    observe({
      req(shared$max_size(), shared$cohort_size())
      initial_cohort_size <- shared$cohort_size()
      initial_rows <- data.frame(
        Patient_Number = seq_len(initial_cohort_size),
        Cohort_Number = rep(1, initial_cohort_size),
        Dose_Level = rep(1, initial_cohort_size),
        DLT = rep(FALSE, initial_cohort_size),
        stringsAsFactors = FALSE
      )
      conduct_reactive_table_data(initial_rows)
    })

    # Add cohort
   observeEvent(input$add_cohort, {
     data <- conduct_reactive_table_data()
     current_patient_count <- nrow(data)
     max_patients <- shared$max_size()
     cohort_size <- shared$cohort_size()
     if (current_patient_count + cohort_size > max_patients) return()
     new_cohort_number <- if (nrow(data) == 0) 1 else max(data$Cohort_Number) + 1
     recommended_dose <- 1
     if (input$choice == "CRM" && !is.null(crm_model()) && nrow(data) > 0) {
       outcome_str <- convert_table_to_crm_outcome(data)
       fit_result <- tryCatch(crm_model() %>% fit(outcome_str), error = function(e) NULL)
       if (!is.null(fit_result)) {
         recommended_dose <- fit_result %>% recommended_dose()
        }
      } else if (input$choice == "BOIN" && !is.null(boin_model())) {
      outcome_str <- convert_table_to_crm_outcome(data)
      fit_result <- tryCatch(boin_model() %>% fit(outcome_str), error = function(e) NULL)
      if (!is.null(fit_result)) {
        recommended_dose <- fit_result %>% recommended_dose()
      }
    } else if (input$choice == "3+3") {
      latest_cohort <- max(data$Cohort_Number, na.rm = TRUE)
      cohort_data <- data[data$Cohort_Number == latest_cohort, ]
      current_dose <- unique(cohort_data$Dose_Level)

     if (length(current_dose) == 1 && !is.na(current_dose)) {
      dlt_count <- sum(cohort_data$DLT)
      previous_cohort_data <- data[data$Cohort_Number == (latest_cohort - 1), ]
      previous_same_dose <- nrow(previous_cohort_data) > 0 &&
                            all(previous_cohort_data$Dose_Level == current_dose)

      if (previous_same_dose) {
        total_dlt <- dlt_count + sum(previous_cohort_data$DLT)
        if (total_dlt >= 2) {
          recommended_dose <- max(current_dose - 1, 1)
        } else if (sum(previous_cohort_data$DLT) == 1 && dlt_count == 0) {
          recommended_dose <- current_dose + 1
        } else {
          recommended_dose <- current_dose
        }
      } else {
        if (dlt_count == 0) {
          recommended_dose <- current_dose + 1
        } else if (dlt_count == 1) {
          recommended_dose <- current_dose
        } else {
          recommended_dose <- max(current_dose - 1, 1)
        }
      }
    }
    }

     start_patient <- current_patient_count + 1
     new_rows <- data.frame(
       Patient_Number = seq(from = start_patient, length.out = cohort_size),
       Cohort_Number = rep(new_cohort_number, cohort_size),
       Dose_Level = rep(recommended_dose, cohort_size),
       DLT = rep(FALSE, cohort_size),
       stringsAsFactors = FALSE
      )
     conduct_reactive_table_data(rbind(data, new_rows))
    })

    # Remove cohort
    observeEvent(input$remove_cohort, {
      data <- conduct_reactive_table_data()
      if (nrow(data) <= 3 || is.na(max(data$Cohort_Number))) return()
      last_cohort <- max(data$Cohort_Number)
      updated_data <- data[data$Cohort_Number != last_cohort, ]
      conduct_reactive_table_data(updated_data)
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
        cohort_number <- data$Cohort_Number[info$row]
        new_value <- as.numeric(info$value)
        data$Dose_Level[data$Cohort_Number == cohort_number] <- new_value
      } else if (col_name == "DLT") {
        new_value <- as.logical(info$value)
        data[info$row, col_name] <- new_value
      } else {
        data[info$row, col_name] <- info$value
      }
      conduct_reactive_table_data(data)
    })

    # Convert table to CRM outcome string
    convert_table_to_crm_outcome <- function(data) {
      split_data <- split(data, data$Cohort_Number)
      outcome_str <- lapply(split_data, function(cohort) {
        paste0(cohort$Dose_Level[1], paste(ifelse(cohort$DLT, "T", "N"), collapse = ""))
      })
      paste(outcome_str, collapse = " ")
    }

    ##### Value boxes #####
   output$latest_dose <- renderText({
     data <- conduct_reactive_table_data()
     if (nrow(data) == 0) return("N/A")
     latest_cohort <- max(data$Cohort_Number, na.rm = TRUE)
     latest_dose <- unique(data$Dose_Level[data$Cohort_Number == latest_cohort])
     paste(latest_dose, collapse = ", ")
    })


    output$recommended_dose <- renderText({
    data <- conduct_reactive_table_data()
    if (nrow(data) == 0) return("N/A")
 
    if (input$choice == "CRM") {
    model <- crm_model()
    if (is.null(model)) return("N/A")
    outcome_str <- convert_table_to_crm_outcome(data)
    tryCatch({
      fit_result <- model %>% fit(outcome_str)
      dose <- fit_result %>% recommended_dose()
      as.character(dose)
    }, error = function(e) {
      "Error in CRM fitting"
    })
    } else if (input$choice == "BOIN") {
    model <- boin_model()
    if (is.null(model)) return("N/A")
    outcome_str <- convert_table_to_crm_outcome(data)
    tryCatch({
      fit_result <- model %>% fit(outcome_str)
      dose <- fit_result %>% recommended_dose()
      as.character(dose)
    }, error = function(e) {
      "Error in BOIN fitting"
    })
    } else if (input$choice == "3+3") {
    latest_cohort <- max(data$Cohort_Number, na.rm = TRUE)
    cohort_data <- data[data$Cohort_Number == latest_cohort, ]
    current_dose <- unique(cohort_data$Dose_Level)
    if (length(current_dose) != 1 || is.na(current_dose)) return("N/A")

    dlt_count <- sum(cohort_data$DLT)
    previous_cohort_data <- data[data$Cohort_Number == (latest_cohort - 1), ]
    previous_same_dose <- nrow(previous_cohort_data) > 0 &&
                          all(previous_cohort_data$Dose_Level == current_dose)

    if (previous_same_dose) {
      total_dlt <- dlt_count + sum(previous_cohort_data$DLT)
      if (total_dlt >= 2) {
        recommended <- max(current_dose - 1, 1)
      } else if (sum(previous_cohort_data$DLT) == 1 && dlt_count == 0) {
        recommended <- current_dose + 1
      } else {
        recommended <- current_dose
      }
    } else {
      if (dlt_count == 0) {
        recommended <- current_dose + 1
      } else if (dlt_count == 1) {
        recommended <- current_dose
      } else {
        recommended <- max(current_dose - 1, 1)
      }
    }
    recommended
    } else {
    return("N/A")
    }
    })

    output$trial_status <- renderText({
     data <- conduct_reactive_table_data()
     max_patients <- shared$max_size()

     # Show prompt if current design hasn't been initialized
      if (design_initialized() != input$choice) {
       return("Press 'Update Design' to start")
      }

      if (nrow(data) >= max_patients) {
       return("Trial Recruitment Complete")
      }

     if (input$choice == "CRM" && !is.null(crm_model()) && nrow(data) > 0) {
       outcome_str <- convert_table_to_crm_outcome(data)
       fit_result <- tryCatch(crm_model() %>% fit(outcome_str), error = function(e) NULL)
       if (!is.null(fit_result)) {
         recommended <- fit_result %>% recommended_dose()
         n_at_recommended <- sum(data$Dose_Level == recommended)
         if (n_at_recommended >= 9) {
           return("Suggestion: Stop trial - 9 patients at recommended dose")
         }
       }
     }
     "Recruiting"
    })


    # Patient count
    output$patient_count <- renderText({
      data <- conduct_reactive_table_data()
      nrow(data)
    })

    # Plot generation#################################
    observeEvent(input$generate_plot, {
      output$dose_plot_ui <- renderUI({
        plotOutput(ns("dose_plot"), height = "400px")
      })

      output$dose_plot <- renderPlot({
        data <- conduct_reactive_table_data()
        if (nrow(data) == 0) return(NULL)

        data$Patient <- data$Patient_Number
        data$Cohort_Position <- ave(data$Cohort_Number, data$Cohort_Number, FUN = seq_along)
        data$X <- data$Cohort_Number + (data$Cohort_Position - 2) * 0.2
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
       axis(1, at = sort(unique(data$Cohort_Number)), labels = sort(unique(data$Cohort_Number)))
       text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
       # Legend area
       par(mar = c(0, 0, 0, 0))
       plot.new()
       legend("center", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19, cex = 1.2, bty = "n")
      })
   })

    observeEvent(input$reset_title, {
      updateTextInput(session, "plot_title", value = "Cohort Grouped Patient Dose Level with DLT's")
    })
    
    ###### CRM results card logic ###################
   show_crm_card <- reactiveVal(FALSE)

   output$crm_results_card_ui <- renderUI({
     if (!show_crm_card()) return(NULL)
     card(
       full_screen = TRUE,
       card_header("CRM Results"),
       card_body(
         p("This table summarises DLTs and posterior estimates by dose level."),
         h5("CRM Summary Table"),
         tableOutput(ns("crm_results_table"))
        )
      )
    })
    
    output$crm_results_table <- renderTable({
      data <- conduct_reactive_table_data()
      if (nrow(data) == 0) return(NULL)
      # Summarise DLTs by Dose Level
      summary <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
      # placeholder CRM stats ###FAKE NEED TO CHANGE
      summary$Posterior_DLT_Rate <- round(runif(nrow(summary), 0.1, 0.5), 2)
      summary$CI_Upper <- round(summary$Posterior_DLT_Rate + runif(nrow(summary), 0.05, 0.15), 2)
      summary$CI_Lower <- round(summary$Posterior_DLT_Rate - runif(nrow(summary), 0.05, 0.1), 2)
      colnames(summary) <- c("Dose Level", "No. of DLTs", "Posterior DLT Rate", "CI Upper", "CI Lower")
      # Set row names to Dose Level
      rownames(summary) <- paste("Dose Level", summary$`Dose Level`)
      summary
    })

    #### BOIN results card logic ####
    show_boin_card <- reactiveVal(FALSE)

    output$boin_results_card_ui <- renderUI({
      if (!show_boin_card()) return(NULL)
      card(
        full_screen = TRUE,
        card_header("BOIN Results"),
        card_body(
          p("This table summarises DLTs and posterior estimates by dose level."),
          h5("BOIN Summary Table"),
          tableOutput(ns("boin_results_table"))
        )
      )
    })
    
    output$boin_results_table <- renderTable({
      data <- conduct_reactive_table_data()
      if (nrow(data) == 0) return(NULL)
      # Summarise DLTs by Dose Level
      summary <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
      # placeholder BOIN stats ###FAKE NEED TO CHANGE
      summary$Posterior_DLT_Rate <- round(runif(nrow(summary), 0.1, 0.5), 2)
      summary$CI_Upper <- round(summary$Posterior_DLT_Rate + runif(nrow(summary), 0.05, 0.15), 2)
      summary$CI_Lower <- round(summary$Posterior_DLT_Rate - runif(nrow(summary), 0.05, 0.1), 2)
      summary$Desirability_Score <- round(runif(nrow(summary), 0, 1), 2)
      colnames(summary) <- c("Dose Level", "No. of DLTs", "Posterior DLT Rate", "CI Upper", "CI Lower", "Desirability Score")
      # Set row names to Dose Level
      rownames(summary) <- paste("Dose Level", summary$`Dose Level`)
      summary
    })

    ################################### Rmd file generation #########################################################
   output$download_report <- downloadHandler(
   filename = function() {
     ext <- if (input$export_type == "PDF") ".pdf" else ".xlsx"
     paste0("cohort_report_", Sys.Date(), ext)
     },
     content = function(file) {
       data <- conduct_reactive_table_data()

       # Generate plot
       plot_file <- tempfile(fileext = ".png")
       png(plot_file, width = 800, height = 600)
       data$Patient <- data$Patient_Number
       data$Cohort_Position <- ave(data$Cohort_Number, data$Cohort_Number, FUN = seq_along)
       data$X <- data$Cohort_Number + (data$Cohort_Position - 2) * 0.2
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
       axis(1, at = sort(unique(data$Cohort_Number)), labels = sort(unique(data$Cohort_Number)))
       text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
       legend("bottom", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19)
       dev.off()

       # Generate CRM or BOIN summary table content
       table_section <- NULL
       crm_data <- NULL
       boin_data <- NULL

       if (input$choice == "CRM") {
         crm_data <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
         crm_data$Posterior_DLT_Rate <- round(runif(nrow(crm_data), 0.1, 0.5), 2)
         crm_data$CI_Upper <- round(crm_data$Posterior_DLT_Rate + runif(nrow(crm_data), 0.05, 0.15), 2)
         crm_data$CI_Lower <- round(crm_data$Posterior_DLT_Rate - runif(nrow(crm_data), 0.05, 0.1), 2)

         table_section <- c(
          "## CRM Summary Table",
          "```{r}",
          "crm_data <- data.frame(",
          paste0("  Dose_Level = c(", paste(crm_data$Dose_Level, collapse = ", "), "),"),
          paste0("  No_of_DLTs = c(", paste(crm_data$DLT, collapse = ", "), "),"),
          paste0("  Posterior_DLT_Rate = c(", paste(crm_data$Posterior_DLT_Rate, collapse = ", "), "),"),
          paste0("  CI_Upper = c(", paste(crm_data$CI_Upper, collapse = ", "), "),"),
          paste0("  CI_Lower = c(", paste(crm_data$CI_Lower, collapse = ", "), ")"),
          ")",
          "knitr::kable(crm_data)",
          "```"
        )
       } else if (input$choice == "BOIN") {
         boin_data <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
         boin_data$Posterior_DLT_Rate <- round(runif(nrow(boin_data), 0.1, 0.5), 2)
         boin_data$CI_Upper <- round(boin_data$Posterior_DLT_Rate + runif(nrow(boin_data), 0.05, 0.15), 2)
         boin_data$CI_Lower <- round(boin_data$Posterior_DLT_Rate - runif(nrow(boin_data), 0.05, 0.1), 2)
         boin_data$Desirability_Score <- round(runif(nrow(boin_data), 0, 1), 2)

         table_section <- c(
         "## BOIN Summary Table",
         "```{r}",
         "boin_data <- data.frame(",
         paste0("  Dose_Level = c(", paste(boin_data$Dose_Level, collapse = ", "), "),"),
         paste0("  No_of_DLTs = c(", paste(boin_data$DLT, collapse = ", "), "),"),
         paste0("  Posterior_DLT_Rate = c(", paste(boin_data$Posterior_DLT_Rate, collapse = ", "), "),"),
         paste0("  CI_Upper = c(", paste(boin_data$CI_Upper, collapse = ", "), "),"),
         paste0("  CI_Lower = c(", paste(boin_data$CI_Lower, collapse = ", "), "),"),
         paste0("  Desirability_Score = c(", paste(boin_data$Desirability_Score, collapse = ", "), ")"),
         ")",
         "knitr::kable(boin_data)",
         "```"
        )
       }

       if (input$export_type == "PDF") {
         rmd_file <- tempfile(fileext = ".Rmd")
         rmd_content <- c(
           "---",
           "title: \"Cohort Report\"",
           "output: pdf_document",
           "---",
           "",
           "## Patient Table",
           "```{r}",
           "library(knitr)",
           "library(dplyr)",
           "data <- tibble::tibble(",
           paste0("  Patient_Number = c(", paste(data$Patient_Number, collapse = ", "), "),"),
           paste0("  Cohort_Number = c(", paste(data$Cohort_Number, collapse = ", "), "),"),
           paste0("  Dose_Level = c(", paste(data$Dose_Level, collapse = ", "), "),"),
           paste0("  DLT = c(", paste(as.character(data$DLT), collapse = ", "), ")"),
           ")",
           "kable(data)",
           "```",
           "",
           "## Dose Plot",
           paste0("![](path/to/dose_plot.png)"),
           "",
           table_section
          )
       writeLines(rmd_content, rmd_file)
       rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)
 
       } else if (input$export_type == "Excel") {
         wb <- openxlsx::createWorkbook()
         openxlsx::addWorksheet(wb, "Cohort Table")
         openxlsx::writeData(wb, "Cohort Table", data)

         openxlsx::addWorksheet(wb, "Dose Plot")
        openxlsx::insertImage(wb, "Dose Plot", plot_file, startRow = 2, startCol = 2, width = 6, height = 6)

      if (!is.null(crm_data)) {
        openxlsx::addWorksheet(wb, "CRM Summary")
        openxlsx::writeData(wb, "CRM Summary", crm_data)
      } else if (!is.null(boin_data)) {
        openxlsx::addWorksheet(wb, "BOIN Summary")
        openxlsx::writeData(wb, "BOIN Summary", boin_data)
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }})
  })
}