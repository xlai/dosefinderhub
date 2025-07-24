library(shiny)
library(shiny.semantic)

# Simulation-Specific Inputs
  n_sims_input <- numericInput("n_sims_input", "How many simulations would you like to run per design per scenario?", value = 10)
  n_scenarios_input <- numericInput("n_scenarios_input", "How many scenarios would you like to simulate?", min = 1, max = 3, value = 3) # Capping the number of scenarios at 3 (for now)
  text2 <- "Please fill out each scenario's and each dose's 'True' Dose Limiting Toxicity probabilities in the table below:"
  table_output <- DT::DTOutput("table_output") # This is to test the table output used for the simulations tab.
  test_df_table <- DT::DTOutput("test_df")
simulation_inputs <- tagList(
  n_sims_input,
  n_scenarios_input,
  text2,
  table_output,
  test_df_table
)


sim_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    main = div(
      h3("Trial Design Simulation"),
      p("Here you can visualize how the recommended trial design performs under various scenarios."),
      p("Click 'Run Simulation' to start the simulation process.")
    ),
    sidebar = sidebar(
      h3("Simulation Inputs"),
      p("Please fill out the following inputs and click 'Run Simulation' to see the results."),
      actionButton(ns("run_simulation"), "Run Simulation"),
      tags$hr(), # Separator line
      simulation_inputs
    )
  )
}

sim_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder logic
  })
}