library(shiny)

ui <- shiny::fluidPage(
  shiny::selectInput("drug_type",
                     label = "What type of drug is being tested?",
                     choices = list("Chemotherapy",
                                    "Immunotherapy",
                                    "Targeted Agent")),

  shiny::radioButtons("know_doses",
                      label = "Do you know how many dose levels are being tested?",
                      choices = list("Yes", "No")),

  shiny::sliderInput("n_doses",
                     label = "How many dose levels are being tested?",
                     value = 5, min = 1, max = 10),

  shiny::numericInput("start_dose",
                      label = "What is the starting dose level?",
                      value = 1, min = 1, max = 10),

  shiny::radioButtons("know_prior_tox_info",
                      label = "How much prior knowledge of the drug's toxicity do you have?",
                      choices = list("None",
                                     "Some pre-clinical information",
                                     "Lots of pre-clinical information",
                                     "Some clinical information",
                                     "Lots of clinical information")),

  shiny::radioButtons("know_ttl",
                      label = "Do you know the target toxicity level for your trial?",
                      choices = list("Yes", "No")),

  shiny::sliderInput("ttl",
                     label = "What is the target toxicity level for this trial?",
                     value = 0.2, min = 0, max = 1, step = 0.05),

  shiny::radioButtons("need_tox_interval",
                      label = "Do you want to provide an indifference interval in which you are happy for the toxicity rate of the recommended dose to fall?",
                      choices = list("Yes", "No")),

  shiny::radioButtons("stats_help",
                      label = "Are you able and willing to provide real time data to a statistician for analysis during the trial?",
                      choices = list("Yes", "No")),

  shiny::radioButtons("know_late_tox",
                      label = "Do you need to consider late toxicities occurring in your trial?",
                      choices = list("Yes", "No")),
                     
  shiny::radioButtons("cohort_vary",
                      label = "Do you want to have varying cohort sizes?",
                      choices = list("Yes", "No")),

  shiny::sliderInput("cohort_size",
                     label = "What size will the cohorts be?",
                     value = 3, min = 1, max = 6,
                     step = 1, ticks = FALSE),

  shiny::radioButtons("know_max_n",
                      label = "Is there a maximum sample size for this trial?",
                      choices = list("Yes", "No")),

  shiny::numericInput("max_n",
                      label = "What is the maximum sample size for this trial?",
                      value = 1, min = 1, max = 1000),

  shiny::fluidRow(
    actionButton("get_rating",
                 label = "Generate recommendations!",
                 class = "btn-block")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
