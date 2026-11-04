library(shiny)
library(bslib)
library(DT)
library(shinyjs)

# ---------------- UI MODULE ----------------
con_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),  # Enable shinyjs
    tags$style(HTML("
      button:disabled {
        background-color: #ccc !important;
        color: #666 !important;
        cursor: not-allowed !important;
      }
    ")),
    page_sidebar(
      sidebar = sidebar(
        radioButtons(
          inputId = ns("choice"),
          label = "Select which design to update during trial conduct:",
          choices = c("3+3", "CRM", "BOIN"),
          inline = FALSE
        ),
        actionButton(ns("update_design"), "Update Design"),
        tags$hr(),
        p("Download the trial so far: a report with the settings, patients, results and plots (HTML), or the patient table (CSV)."),
        downloadButton(ns("download_report"), "Download Report (HTML)"),
        downloadButton(ns("download_patients"), "Download Patient Table (CSV)")
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
      uiOutput(ns("crm_plot_card_ui")),
      uiOutput(ns("boin_results_card_ui")),
      uiOutput(ns("boin_plot_card_ui"))
    )
  )
}



################################### SERVER MODULE ###############################################################
con_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store reactives
    crm_model <- reactiveVal(NULL)
    boin_model <- reactiveVal(NULL)
    tpt_model <- reactiveVal(NULL)
    # The chosen design fitted to the patients in the table, as list(fit) or
    # list(error); NULL before "Update Design" or with an empty table.
    conduct_fit <- reactive({
      data <- conduct_reactive_table_data()
      model <- switch(input$choice, "CRM" = crm_model(), "BOIN" = boin_model(), "3+3" = tpt_model())
      if (is.null(model) || nrow(data) == 0) return(NULL)
      tryCatch(list(fit = model %>% fit(convert_table_to_crm_outcome(data))),
               error = function(e) list(error = conditionMessage(e)))
    })
    # The trial has stopped when the fitted design says so. It follows the
    # table, so "Update Design" or removing a cohort can clear it.
    trial_stopped <- reactive({
      fitted <- conduct_fit()
      !is.null(fitted$fit) && !continue(fitted$fit)
    })
    final_dose <- reactiveVal(NULL)

   
    design_initialized <- reactiveVal(FALSE)
    # Settings of the design in use, for the report. Captured on "Update
    # Design", the moment the design is built from them.
    conduct_settings <- reactiveVal(NULL)

    observeEvent(input$update_design, {
     design_initialized(input$choice)
     conduct_settings(report_design_settings(shared, input$choice, first = c("Design" = input$choice)))

     # Reset table to first cohort
     initial_cohort_size <- shared$cohort_size()
     initial_rows <- data.frame(
       Patient_Number = seq_len(initial_cohort_size),
       Cohort_Number = rep(1, initial_cohort_size),
       Dose_Level = rep(shared$start_dose(), initial_cohort_size),
       DLT = rep(0, initial_cohort_size),
       stringsAsFactors = FALSE
      )
     conduct_reactive_table_data(initial_rows)

     # Reset plot
     output$dose_plot_ui <- renderUI(NULL)
     output$dose_plot <- renderPlot(NULL)

     # Initialize model
     # The same designs as in Simulation (see the design builders in global.R)
     if (input$choice == "CRM") {
       model <- crm_design(shared$skeleton_crm(), shared$ttl(), shared$max_size(), shared$prior_var_crm(),
                           shared$skip_esc_crm(), shared$skip_deesc_crm(), shared$stop_tox_x_crm(),
                           shared$stop_tox_y_crm(), shared$stop_n_mtd_crm())
       crm_model(model)
       boin_model(NULL)
       tpt_model(NULL)
      }

     if (input$choice == "BOIN") {
       model <- boin_design(shared$n_dosess(), shared$ttl(), shared$max_size(), shared$stop_n_mtd_boin(),
                            p_tox = shared$phi_2, p_saf = shared$phi_1, shared$boin_stopping_rule())
       boin_model(model)
       crm_model(NULL)
       tpt_model(NULL)
      }

      if (input$choice == "3+3") {
        model <- tpt_design(shared$n_dosess(), shared$max_size())
       crm_model(NULL)
       boin_model(NULL)
       tpt_model(model)
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
        DLT = numeric(0),
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
        Dose_Level = rep(shared$start_dose(), initial_cohort_size),
        DLT = rep(0, initial_cohort_size),
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
      if (trial_stopped()) return()
      # Prevent exceeding max sample size
      if (current_patient_count + cohort_size > max_patients) return()
      # New cohort number
      new_cohort_number <- if (nrow(data) == 0) 1 else max(data$Cohort_Number) + 1
      # Default dose level
      recommended_dose <- 1
      # Use model-based escalation for CRM, BOIN, and 3+3
      if (input$choice %in% c("CRM", "BOIN", "3+3")) {
        model <- switch(input$choice,
                    "CRM" = crm_model(),
                    "BOIN" = boin_model(),
                    "3+3" = tpt_model())

      if (!is.null(model) && nrow(data) > 0) {
        outcome_str <- convert_table_to_crm_outcome(data)
        fit_result <- tryCatch(model %>% fit(outcome_str), error = function(e) NULL)

        if (!is.null(fit_result)) {
          recommended_dose <- tryCatch({
            fit_result %>% recommended_dose()
          }, error = function(e) {
           message("Error in recommended_dose(): ", e$message)
           NA
          })
        }
      }
      }

      # Add new cohort rows
      start_patient <- current_patient_count + 1
      new_rows <- data.frame(
        Patient_Number = seq(from = start_patient, length.out = cohort_size),
        Cohort_Number = rep(new_cohort_number, cohort_size),
        Dose_Level = rep(recommended_dose, cohort_size),
        DLT = rep(0, cohort_size),
        stringsAsFactors = FALSE
      )

     # Update the reactive data
     conduct_reactive_table_data(rbind(data, new_rows))
    })

    # Remove cohort
    observeEvent(input$remove_cohort, {
      data <- conduct_reactive_table_data()
      # The first cohort always stays, whatever the cohort size.
      if (nrow(data) == 0 || max(data$Cohort_Number, na.rm = TRUE) <= 1) return()
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
        new_value <- as.numeric(info$value)
        if (new_value %in% c(0, 1)) {
          data[info$row, col_name] <- new_value
        }

      } else {
        data[info$row, col_name] <- info$value
      }
      conduct_reactive_table_data(data)
    })

    # Convert table to CRM outcome string
    convert_table_to_crm_outcome <- function(data) {
      split_data <- split(data, data$Cohort_Number)
      outcome_str <- lapply(split_data, function(cohort) {
        paste0(cohort$Dose_Level[1], paste(ifelse(cohort$DLT == 1, "T", "N"), collapse = ""))
      })
      paste(outcome_str, collapse = " ")
    }

    ##### Value boxes #####
    #Latest/Current Dose Level
   output$latest_dose <- renderText({
     data <- conduct_reactive_table_data()
     if (nrow(data) == 0) return("N/A")
     latest_cohort <- max(data$Cohort_Number, na.rm = TRUE)
     latest_dose <- unique(data$Dose_Level[data$Cohort_Number == latest_cohort])
     paste(latest_dose, collapse = ", ")
    })

    # Recommended next dose level
    output$recommended_dose <- renderText({
     fitted <- conduct_fit()
     if (is.null(fitted$fit)) return("N/A")
     dose <- fitted$fit %>% recommended_dose()
     final_dose(as.character(dose))  # Save final dose
     if (trial_stopped()) {
       paste("Final MTD Level:", if (is.na(dose)) "none" else dose)
     } else {
       as.character(dose)
     }
    })
    
    # Trial status
    status_text <- reactive({
     data <- conduct_reactive_table_data()
     max_patients <- shared$max_size()
     if (design_initialized() != input$choice) {
       return("Press 'Update Design' to start")
      }
      fitted <- conduct_fit()
      if (!is.null(fitted$error)) {
        return(paste("The design could not be fitted to the table:", fitted$error))
      }
      if (trial_stopped()) {
        return(paste0("Trial stopped: ", stop_reason(fitted$fit), "."))
      }
      if (nrow(data) >= max_patients) {
       return("Trial Recruitment Complete")
      }
    "Recruiting"
    })
    output$trial_status <- renderText(status_text())
   
    #Disables Add Cohort button if trial is stopped
   observe({
     if (trial_stopped()) {
     shinyjs::disable("add_cohort")
     } else {
       shinyjs::enable("add_cohort")
     }
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
        draw_cohort_plot(data, input$plot_title)
      })
   })

    # The patients by cohort and dose, red for a DLT. Drawn on screen and in the report.
    draw_cohort_plot <- function(data, title) {
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
         main = title,
         xaxt = "n",
         yaxt = "n",
         ylim = ylim
       )
       grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dashed", lwd = 0.5)  # Add grid lines
       axis(2, at = seq(1, max(data$Dose_Level), by = 1), las = 2)  # Y-axis with dose levels
       axis(1, at = sort(unique(data$Cohort_Number)), labels = sort(unique(data$Cohort_Number)))
       text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
       # Legend area
       par(mar = c(0, 0, 0, 0))
       plot.new()
       legend("center", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19, cex = 1.2, bty = "n")
    }

    observeEvent(input$reset_title, {
      updateTextInput(session, "plot_title", value = "Cohort Grouped Patient Dose Level with DLT's")
    })
    
    ###### CRM results card logic #######################
   show_crm_card <- reactiveVal(FALSE)
   
   ## Reactive Results for CRM
    crm_results_data <- reactive({
     model <- crm_model()
     data <- conduct_reactive_table_data()

     if (is.null(model) || nrow(data) == 0) return(NULL)

     outcome_str <- convert_table_to_crm_outcome(data)
     fit <- tryCatch(model %>% fit(outcome_str), error = function(e) NULL)
     if (is.null(fit)) return(NULL)

     treated <- fit %>% n_at_dose()
     posterior <- fit %>% mean_prob_tox()
     ci_lower <- fit %>% prob_tox_quantile(0.025)
     ci_upper <- fit %>% prob_tox_quantile(0.975)

     dlt_counts <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
     colnames(dlt_counts) <- c("Dose_Level", "No_of_DLTs")

     dose_levels <- seq_along(posterior)
     results <- data.frame(
       Dose_Level = dose_levels,
       No_of_Patients = treated,
       Posterior_DLT_Rate = round(posterior, 3),
       CI_Lower = round(ci_lower, 3),
       CI_Upper = round(ci_upper, 3),
       stringsAsFactors = FALSE
     )

     results <- merge(results, dlt_counts, by = "Dose_Level", all.x = TRUE)
     results$No_of_DLTs[is.na(results$No_of_DLTs)] <- 0
     results$No_of_DLTs <- as.integer(results$No_of_DLTs)

     results
    })
    ## Results table for CRM
    output$crm_results_table <- renderTable({
     results <- crm_results_data()
     if (is.null(results)) return(NULL)

     results <- results[, c("Dose_Level", "No_of_Patients", "No_of_DLTs", "Posterior_DLT_Rate", "CI_Lower", "CI_Upper")]
     colnames(results) <- c("Dose Level", "No. of Patients", "No. of DLTs", "Posterior DLT Rate", "CI Lower(95%)", "CI Upper(95%)")
     rownames(results) <- paste("Dose Level", results$`Dose Level`)
     results
    })

    ## CRM results card UI
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

    ## Crm DLT Plot
output$crm_dlt_plot <- renderPlot({
  results <- crm_results_data()
  if (is.null(results)) return(NULL)
  draw_crm_plot(results)
})

draw_crm_plot <- function(results) {
  dose_levels <- results$Dose_Level
  posterior <- results$Posterior_DLT_Rate
  ci_lower <- results$CI_Lower
  ci_upper <- results$CI_Upper
  patients <- results$No_of_Patients

  plot(
    dose_levels, posterior,
    pch = 19, col = "blue", cex = 1.5,
    xlab = "Dose Level", ylab = "Posterior DLT Rate",
    main = "Posterior DLT Rates by Dose Level (CRM)",
    ylim = c(0, max(ci_upper) + 0.1)
  )

  # Add dashed line connecting points
  lines(dose_levels, posterior, col = "blue", lty = 2, lwd = 1.5)

  # Add shaded CI region (optional)
  polygon(
    x = c(dose_levels, rev(dose_levels)),
    y = c(ci_lower, rev(ci_upper)),
    col = adjustcolor("skyblue", alpha.f = 0.4),
    border = NA
  )

  # Add whiskers (error bars)
  arrows(
    x0 = dose_levels, y0 = ci_lower,
    x1 = dose_levels, y1 = ci_upper,
    angle = 90, code = 3, length = 0.05, col = "darkblue"
  )

  # Add patient count labels
  text(dose_levels, posterior + 0.05, labels = paste(patients, "pts"), cex = 0.8)

  grid()
}


    ## CRM plot card UI
    output$crm_plot_card_ui <- renderUI({
      if (!show_crm_card()) return(NULL)
     card(
       full_screen = TRUE,
       card_header("CRM Posterior DLT Plot"),
       card_body(
          p("This plot shows posterior DLT rates with 95% credible intervals shaded."),
          plotOutput(ns("crm_dlt_plot"), height = "400px")
        )
       )
    })

   #### BOIN results card logic ####
    show_boin_card <- reactiveVal(FALSE)

   ## Reactive Results for BOIN
    boin_results_data <- reactive({
     model <- boin_model()
     data <- conduct_reactive_table_data()

     if (is.null(model) || nrow(data) == 0) return(NULL)

     outcome_str <- convert_table_to_crm_outcome(data)
     fit <- tryCatch(model %>% fit(outcome_str), error = function(e) NULL)
     if (is.null(fit)) return(NULL)

     treated <- fit %>% n_at_dose()
     posterior <- fit %>% mean_prob_tox()
     ci_lower <- fit %>% prob_tox_quantile(0.025)
     ci_upper <- fit %>% prob_tox_quantile(0.975)

     dlt_counts <- aggregate(DLT ~ Dose_Level, data = data, FUN = function(x) sum(x, na.rm = TRUE))
     colnames(dlt_counts) <- c("Dose_Level", "No_of_DLTs")

     dose_levels <- seq_along(posterior)
     results <- data.frame(
       Dose_Level = dose_levels,
       No_of_Patients = treated,
       Posterior_DLT_Rate = round(posterior, 3),
       CI_Lower = round(ci_lower, 3),
       CI_Upper = round(ci_upper, 3),
       stringsAsFactors = FALSE
     )

     results <- merge(results, dlt_counts, by = "Dose_Level", all.x = TRUE)
     results$No_of_DLTs[is.na(results$No_of_DLTs)] <- 0
     results$No_of_DLTs <- as.integer(results$No_of_DLTs)

     results
    })
    
    ## Results table for BOIN
    output$boin_results_table <- renderTable({
     results <- boin_results_data()
     if (is.null(results)) return(NULL)

     results <- results[, c("Dose_Level", "No_of_Patients", "No_of_DLTs", "Posterior_DLT_Rate", "CI_Lower", "CI_Upper")]
     colnames(results) <- c("Dose Level", "No. of Patients", "No. of DLTs", "Posterior DLT Rate", "CI Lower(95%)", "CI Upper(95%)")
     rownames(results) <- paste("Dose Level", results$`Dose Level`)
     results
    })

    ## BOIN results card UI
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

    ## BOIN DLT Plot
   output$boin_dlt_plot <- renderPlot({
     results <- boin_results_data()
     
     validate(
       need(!is.null(results), "Please reach status: complete to see the plot.")
      )
     draw_boin_plot(results)
    })

    draw_boin_plot <- function(results) {
     # BOIN gives no estimate (NA) for doses not given yet, so plot only the
     # doses with one, on an axis that still shows every dose.
     n_doses <- nrow(results)
     results <- results[!is.na(results$Posterior_DLT_Rate), ]

     dose_levels <- results$Dose_Level
     posterior <- results$Posterior_DLT_Rate
     ci_lower <- results$CI_Lower
     ci_upper <- results$CI_Upper
     patients <- results$No_of_Patients

     plot(
       dose_levels, posterior,
       pch = 19, col = "blue", cex = 1.5,
       xlab = "Dose Level", ylab = "Posterior DLT Rate",
       main = "Posterior DLT Rates by Dose Level (BOIN)",
       xlim = c(1, n_doses), ylim = c(0, max(ci_upper) + 0.1)
      )

     polygon(
       x = c(dose_levels, rev(dose_levels)),
       y = c(ci_lower, rev(ci_upper)),
       col = adjustcolor("skyblue", alpha.f = 0.4),
       border = NA
      )

     text(dose_levels, posterior + 0.05, labels = paste(patients, "pts"), cex = 0.8)
     grid()
    }

    ## BOIN plot card UI
    output$boin_plot_card_ui <- renderUI({
      if (!show_boin_card()) return(NULL)
     card(
       full_screen = TRUE,
       card_header("BOIN Posterior DLT Plot"),
       card_body(
          p("This plot shows posterior DLT rates with 95% credible intervals shaded."),
          plotOutput(ns("boin_dlt_plot"), height = "400px")
        )
       )
    })


    ################################### Downloads (decision 0003) ###################################
    # The trial so far as an HTML report, and the patient table as CSV. Both
    # work once the chosen design has been built with "Update Design".
    observe({
      ready <- identical(design_initialized(), input$choice)
      shinyjs::toggleState("download_report", condition = ready)
      shinyjs::toggleState("download_patients", condition = ready)
    })

    # A dose-level results table as shown on screen
    results_for_report <- function(results) {
      results <- results[, c("Dose_Level", "No_of_Patients", "No_of_DLTs", "Posterior_DLT_Rate", "CI_Lower", "CI_Upper")]
      colnames(results) <- c("Dose Level", "No. of Patients", "No. of DLTs", "Posterior DLT Rate", "CI Lower (95%)", "CI Upper (95%)")
      results
    }

    output$download_report <- downloadHandler(
      filename = function() paste0("conduct-report-", Sys.Date(), ".html"),
      content = function(file) {
        data <- conduct_reactive_table_data()
        fitted <- conduct_fit()
        dose <- if (!is.null(fitted$fit)) recommended_dose(fitted$fit) else NA
        status <- data.frame(
          Value = c(nrow(data), status_text(),
                    if (is.na(dose)) "none" else as.character(dose)),
          row.names = c("Number of patients", "Trial status",
                        if (trial_stopped()) "Final MTD level" else "Recommended next dose level"),
          check.names = FALSE
        )
        results <- switch(input$choice, "CRM" = crm_results_data(), "BOIN" = boin_results_data(), NULL)
        draw_results <- switch(input$choice, "CRM" = draw_crm_plot, "BOIN" = draw_boin_plot, NULL)
        html <- report_page(
          "Conduct Report",
          h2("Settings"),
          report_table(conduct_settings()),
          h2("Trial status"),
          report_table(status),
          h2("Patients"),
          report_table(data, rownames = FALSE),
          if (nrow(data) > 0) report_plot(function() draw_cohort_plot(data, input$plot_title),
                                           alt = input$plot_title),
          if (!is.null(results)) tagList(
            h2(paste(input$choice, "results by dose level")),
            report_table(results_for_report(results), rownames = FALSE),
            report_plot(function() draw_results(results),
                        alt = paste("Posterior DLT rates by dose level", input$choice))
          ),
          h2("Appendix: questionnaire answers"),
          report_answers(shared$q_responses()),
          h2("Appendix: package versions"),
          report_table(report_versions())
        )
        writeLines(html, file, useBytes = TRUE)
      }
    )

    output$download_patients <- downloadHandler(
      filename = function() paste0("conduct-patients-", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(conduct_reactive_table_data(), file, row.names = FALSE)
      }
    )

  })
}