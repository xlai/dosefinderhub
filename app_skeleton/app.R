#app.R#
# Load necessary libraries
library(shiny)
library(bslib)
library(htmltools)


#source("app_skeleton/pages/intro_ui.R")
#source("app_skeleton/pages/question_ui.R")
#source("app_skeleton/pages/trial_design_ui.R")
#source("app_skeleton/pages/sim_ui.R")
#source("app_skeleton/pages/con_ui.R")
#works without sourcing yipeee

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
    # Defining a shared reactive variable for n_doses
    shared <- reactiveValues()

    # The UIs
    intro_server("intro")
    question_server("questionnaire")
    trial_design_server("trial_design", shared)
    sim_server("simulation", shared)
    con_server("conduct")
}

shinyApp(ui, server)

##### List of Shared Variables #####

### Defined in trial_design_server ###

# shared$n_dosess: Number of doses
# shared$ttl: Total number of trials
# shared$max_size: Maximum sample size
# shared$start_dose: Starting dose level
# shared$cohort: Cohort size

# shared$skip_esc_crm: Skip escalation in CRM
# shared$skip_deesc_crm: Skip de-escalation in CRM
# shared$above_target_crm: Above target in CRM
# shared$prior_var_crm: Prior variance in CRM
# shared$stop_n_mtd_crm: Stop at n MTD in CRM
# shared$skeleton_crm: Skeleton in CRM
# shared$prior_mtd_crm: Prior MTD in CRM
# shared$stop_tox_x_crm: Stop toxicity x in CRM
# shared$stop_tox_y_crm: Stop toxicity y in CRM

# shared$skip_tpt: Skip de-escalation in 3+3
