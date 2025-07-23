#app.R#
# Load necessary libraries
library(shiny)
library(bslib)
library(htmltools)

here::i_am("app_skeleton/app.R")

#source("app_skeleton/pages/intro_ui.R")
#source("app_skeleton/pages/question_ui.R")
#source("app_skeleton/pages/trial_design_ui.R")
#source("app_skeleton/pages/sim_ui.R")
#source("app_skeleton/pages/con_ui.R")
#works without sourcing yippeee (liar)

# Define UI for the application
ui <- navbarPage(
    title = "Dose Finder Hub",
    id = "nav",
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    # Intro tab
    tabPanel("Introduction", intro_ui("intro")),
    # Questionnaire tab
    tabPanel("Questionnaire", question_ui("questionnaire")),
    # Results tab
    tabPanel("Trial Design", trial_design_ui("trial_design")),
    # Simulation tab
    tabPanel("Simulation", sim_ui("simulation")),
    # Conduct tab
    tabPanel("Conduct", con_ui("conduct"))
)

# Define server logic
server <- function(input, output, session){
    intro_server("intro")
    question_server("questionnaire")
    trial_design_server("trial_design")
    sim_server("simulation")
    con_server("conduct")
}