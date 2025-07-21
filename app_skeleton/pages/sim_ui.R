sim_ui <- function(id) {
    ns <- NS(id)
    layout_sidebar(
        sidebar = sidebarPanel(
            h4("Simulation"),
            p("This section allows you to simulate the trial design based on your inputs."),
            actionButton(ns("run_simulation"), "Run Simulation")
        ),
        main = mainPanel(
            h3("Trial Design Simulation"),
            p("Here you can visualize how the recommended trial design performs under various scenarios."),
            p("Click 'Run Simulation' to start the simulation process.")
        )
    )
}

sim_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$run_simulation, {
            # Placeholder for simulation logic
            showModal(modalDialog(
                title = "Simulation Running",
                "The simulation is currently running. Please wait...",
                easyClose = TRUE,
                footer = NULL
            ))
            # Simulate a delay for the simulation process
            Sys.sleep(2)
            removeModal()
            updateTabsetPanel(session$ns("nav"), selected = "Conduct")
        })
    })
}

