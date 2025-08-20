library(shiny)
library(bslib)
library(DT)
library(shinydashboard)
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
        radioButtons(ns("export_type"), "Choose Export Format:",
               choices = c("Rmd","PDF", "Excel"),
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
    trial_stopped <- reactiveVal(FALSE)
    final_dose <- reactiveVal(NULL)

   
    design_initialized <- reactiveVal(FALSE)

    observeEvent(input$update_design, {
     design_initialized(input$choice)

     # Reset table to first cohort
     initial_cohort_size <- shared$cohort_size()
     initial_rows <- data.frame(
       Patient_Number = seq_len(initial_cohort_size),
       Cohort_Number = rep(1, initial_cohort_size),
       Dose_Level = rep(1, initial_cohort_size),
       DLT = rep(0, initial_cohort_size),
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
       tpt_model(NULL)
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
       tpt_model(NULL)
      }

      if (input$choice == "3+3") {
        target <- shared$ttl()
        num_doses <- shared$n_dosess()
        model <- get_three_plus_three(num_doses = num_doses, target = target) %>%
       dont_skip_doses(when_escalating = TRUE) %>%
       stop_at_n(n = shared$max_size()) %>%
       stop_when_n_at_dose(dose = "recommended", n = 9)
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
        Dose_Level = rep(1, initial_cohort_size),
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
     data <- conduct_reactive_table_data()
     if (nrow(data) == 0) return("N/A")
     choice <- input$choice

     if (choice %in% c("CRM", "BOIN", "3+3")) {
       model <- switch(choice,
                    "CRM" = crm_model(),
                    "BOIN" = boin_model(),
                    "3+3" = tpt_model())

       if (is.null(model)) return("N/A")
       outcome_str <- convert_table_to_crm_outcome(data)

       tryCatch({
         fit_result <- model %>% fit(outcome_str)
         dose <- fit_result %>% recommended_dose()
         final_dose(as.character(dose))  # Save final dose
        if (trial_stopped()) {
          paste("Final MTD Level:", dose)
        } else {
        as.character(dose)
      }
      }, error = function(e) {
      trial_stopped(TRUE)
      "Final MTD Level: Latest Dose"
      })
      } else {
       "N/A"
      }
    })
    
    # Trial status
    output$trial_status <- renderText({
     data <- conduct_reactive_table_data()
     max_patients <- shared$max_size()
     if (design_initialized() != input$choice) {
       return("Press 'Update Design' to start")
      }
      if (trial_stopped()) {
        return("Trial Stopped Due to Toxicity Levels")
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
      })
   })

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
})


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
       ylim = c(0, max(ci_upper) + 0.1)
      )

     polygon(
       x = c(dose_levels, rev(dose_levels)),
       y = c(ci_lower, rev(ci_upper)),
       col = adjustcolor("skyblue", alpha.f = 0.4),
       border = NA
      )

     text(dose_levels, posterior + 0.05, labels = paste(patients, "pts"), cex = 0.8)
     grid()
    })

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


    ################################### Rmd file generation #########################################################
   output$download_report <- downloadHandler(
  filename = function() {
    ext <- switch(input$export_type,
                  "PDF" = ".pdf",
                  "Excel" = ".xlsx",
                  "Rmd" = ".Rmd")
    paste0("cohort_report_", Sys.Date(), ext)
  },
  content = function(file) {
    data <- conduct_reactive_table_data()

    # --- Generate cohort plot ---
    cohort_plot_file <- tempfile(fileext = ".png")
    png(cohort_plot_file, width = 800, height = 600)
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
      main = "Cohort Grouped Patient Dose Level with DLTs",
      xaxt = "n",
      ylim = ylim
    )
    axis(1, at = sort(unique(data$Cohort_Number)), labels = sort(unique(data$Cohort_Number)))
    text(data$X, data$Dose_Level + 0.3, labels = paste0("P", data$Patient), cex = 0.8)
    legend("bottom", legend = c("DLT", "No DLT"), col = c("red", "green"), pch = 19)
    dev.off()

    # --- Generate CRM plot ---
    crm_plot_file <- tempfile(fileext = ".png")
    crm_data <- NULL
    boin_data <- NULL

    if (input$choice == "CRM") {
      crm_data <- get_crm_results()
      png(crm_plot_file, width = 800, height = 600)
      plot(
        crm_data$`Dose Level`, crm_data$`Posterior DLT Rate`,
        pch = 19, col = "blue", cex = 1.5,
        xlab = "Dose Level", ylab = "Posterior DLT Rate",
        main = "Posterior DLT Rates by Dose Level (CRM)",
        ylim = c(0, max(crm_data$`CI Upper`) + 0.1)
      )
      arrows(
        x0 = crm_data$`Dose Level`, y0 = crm_data$`CI Lower`,
        x1 = crm_data$`Dose Level`, y1 = crm_data$`CI Upper`,
        angle = 90, code = 3, length = 0.05, col = "darkblue"
      )
      text(crm_data$`Dose Level`, crm_data$`Posterior DLT Rate` + 0.05,
           labels = paste(crm_data$`No. of Patients`, "pts"), cex = 0.8)
      grid()
      dev.off()
    } else if (input$choice == "BOIN") {
      boin_data <- get_boin_results()
    }

    # --- Rmd content ---
    table_section <- NULL
    if (!is.null(crm_data)) {
      table_section <- c(
        "## CRM Summary Table",
        "```{r}",
        "crm_data <- data.frame(",
        paste0("  `Dose Level` = c(", paste(crm_data$`Dose Level`, collapse = ", "), "),"),
        paste0("  `No. of Patients` = c(", paste(crm_data$`No. of Patients`, collapse = ", "), "),"),
        paste0("  `No. of DLTs` = c(", paste(crm_data$`No. of DLTs`, collapse = ", "), "),"),
        paste0("  `Posterior DLT Rate` = c(", paste(crm_data$`Posterior DLT Rate`, collapse = ", "), "),"),
        paste0("  `CI Upper` = c(", paste(crm_data$`CI Upper`, collapse = ", "), "),"),
        paste0("  `CI Lower` = c(", paste(crm_data$`CI Lower`, collapse = ", "), ")"),
        ")",
        "knitr::kable(crm_data)",
        "```"
      )
    } else if (!is.null(boin_data)) {
      table_section <- c(
        "## BOIN Summary Table",
        "```{r}",
        "boin_data <- data.frame(",
        paste0("  `Dose Level` = c(", paste(boin_data$`Dose Level`, collapse = ", "), "),"),
        paste0("  `No. of Patients` = c(", paste(boin_data$`No. of Patients`, collapse = ", "), "),"),
        paste0("  `No. of DLTs` = c(", paste(boin_data$`No. of DLTs`, collapse = ", "), "),"),
        paste0("  `Posterior DLT Rate` = c(", paste(boin_data$`Posterior DLT Rate`, collapse = ", "), "),"),
        paste0("  `CI Upper` = c(", paste(boin_data$`CI Upper`, collapse = ", "), "),"),
        paste0("  `CI Lower` = c(", paste(boin_data$`CI Lower`, collapse = ", "), ")"),
        ")",
        "knitr::kable(boin_data)",
        "```"
      )
    }

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
      "## Cohort Dose Plot",
      paste0("!"),
      "",
      if (!is.null(crm_data)) {
        c("## CRM Posterior DLT Plot", paste0("!"))
      } else {
        NULL
      },
      "",
      table_section
    )

    # --- Export ---
    if (input$export_type == "PDF") {
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, rmd_file)
      rmarkdown::render(rmd_file, output_file = file, quiet = TRUE)
    } else if (input$export_type == "Excel") {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Cohort Table")
      openxlsx::writeData(wb, "Cohort Table", data)

      openxlsx::addWorksheet(wb, "Cohort Plot")
      openxlsx::insertImage(wb, "Cohort Plot", cohort_plot_file, startRow = 2, startCol = 2, width = 6, height = 6)

      if (!is.null(crm_data)) {
        openxlsx::addWorksheet(wb, "CRM Summary")
        openxlsx::writeData(wb, "CRM Summary", crm_data)
        openxlsx::addWorksheet(wb, "CRM Plot")
        openxlsx::insertImage(wb, "CRM Plot", crm_plot_file, startRow = 2, startCol = 2, width = 6, height = 6)
      } else if (!is.null(boin_data)) {
        openxlsx::addWorksheet(wb, "BOIN Summary")
        openxlsx::writeData(wb, "BOIN Summary", boin_data)
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    } else if (input$export_type == "Rmd") {
      rmd_file <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, rmd_file)
      file.copy(rmd_file, file)
    }
  }
)

  })
}