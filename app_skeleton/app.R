#app.R#
# Load necessary libraries
library(shiny)
library(bslib)
library(htmltools)

here::i_am("app_skeleton/app.R")

source("app_skeleton/pages/intro_ui.R")
source("app_skeleton/pages/questionnaire_ui.R")
source("app_skeleton/pages/trial_design_ui.R")
source("app_skeleton/pages/sim_ui.R")
source("app_skeleton/pages/con_ui.R")
#works without sourcing yippeee (liar)

# Define UI for the application
ui <- navbarPage(
    title = "Dose Finder Hub",
    id = "nav",
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    # Intro tab
 #   tabPanel("Introduction", intro_ui("intro")),
    # Questionnaire tab
    tabPanel("Questionnaire", mod_questionnaire_ui("questionnaire")),
    # Results tab
    tabPanel("Trial Design", trial_design_ui("trial_design")),
    # Simulation tab
    tabPanel("Simulation", sim_ui("simulation")),
    # Conduct tab
    tabPanel("Conduct", con_ui("conduct"))
)

# Define server logic
server <- function(input, output, session){
#    intro_server("intro")
    questionnaire_results <- mod_questionnaire_server("questionnaire")
    trial_design_server("trial_design")
    sim_server("simulation")
    con_server("conduct")

      observe({
    if (!is.null(questionnaire_results) && 
        !is.null(questionnaire_results$is_complete) &&
        questionnaire_results$is_complete()) {
      
      # Enable other tabs or provide navigation hints
      showNotification(
        "Questionnaire completed! You can now proceed to Trial Design.",
        type = "success",
        duration = 5
      )
    }
    })
}

shinyApp(ui, server)