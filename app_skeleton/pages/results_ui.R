results_ui <- function(id) {
    ns <- NS(id)
    layout_sidebar(
        sidebar = sidebarPanel(
            h4("Results"),
            p("Here are the results based on your inputs."),
            actionButton(ns("view_simulation"), "View Simulation")
        ),
        main = mainPanel(
            h3("Trial Design Results"),
            p("This section displays the results of your questionnaire and the recommended trial design."),
            p("Click 'View Simulation' to see how the design performs under different scenarios.")
        )
    )
}

results_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$view_simulation, {
            updateTabsetPanel(session$ns("nav"), selected = "Simulation")
        })
    })
}