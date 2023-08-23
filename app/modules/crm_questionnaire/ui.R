library(shiny)

ui <- shiny::fluidPage(

  theme = bslib::bs_theme(bootswatch = "darkly"),

  shiny::radioButtons("crm_skip_esc",
                      label = "Would you like to be able to skip doses when
                      escalating?",
                      choices = list("Yes", "No")),

  shiny::radioButtons("crm_skip_deesc",
                      label = "Would you like to be able to skip doses when
                      de-escalating?",
                      choices = list("Yes", "No")),

  shiny::radioButtons("no_esc_if_observed_gt_target",
                      label = "Do you want to prevent escalation of doses if
                      the overall observed DLT rate at the current dose level
                      is above the target DLT rate?",
                      choices = list("Yes", "No")),

  shiny::numericInput("prior_var",
                      label = "What is the estimate of the prior variance?",
                      value = 0.5, min = 0, max = 100),

  shiny::numericInput("stop_n_mtd",
                      label = "What is the minimum number of patients
                      required at recommended dose before early stopping?",
                      value = 12, min = 1, max = 1000),

  shiny::textInput("prior_ttp",
                   label = "What are the prior estimates of the DLT
                   rates at each dose? Please separate each value with
                   a comma, ie 0.1, 0.12, 0.22, 0.5 for 4 doses."),

  shiny::textInput("true_dlt_ss",
                   label = "What are the true DLT rates at each dose
                   for this scenario? Please separate each value with
                   a comma, ie 0.05, 0.15, 0.3, 0.7 for 4 doses."),

  shiny::numericInput("prior_mtd",
                      label = "What is your prior guess of the MTD",
                      value = 2, min = 1, max = 10, step = 1),

  shiny::numericInput("n_sims$crm",
                      label = "How many simulations would you like to run?",
                      value = 20, min = 1, max = 10000),

  shiny::fluidRow(
    actionButton("get_rating",
                 label = "Simulate!",
                 class = "btn-block")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
