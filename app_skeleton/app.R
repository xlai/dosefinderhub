#app.R#
# Load necessary libraries
library(shiny)
library(bslib)
library(htmltools)
library(markdown)

here::i_am("app_skeleton/app.R")

source("app_skeleton/pages/intro_ui.R")
source("app_skeleton/R/mod_02_questionnaire.R")
source("app_skeleton/pages/global.R")
source("app_skeleton/pages/trial_design_ui.R")
source("app_skeleton/pages/sim_ui.R")
source("app_skeleton/pages/con_ui.R")
source("app_skeleton/R/utils_plotting.R")
source("app_skeleton/R/utils_simulation.R")

# Define UI for the application
ui <- navbarPage(
    title = "DoseFinderHub",
    id = "nav",
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    # Intro tab
    tabPanel("Introduction", intro_ui("intro")),
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
    # Defining Buttons that move data between tabs
    move_data <- reactiveVal(FALSE)

    # Defining shared reactive variables
    shared <- reactiveValues()

    intro_server("intro", session)
    questionnaire_results <- mod_questionnaire_server("questionnaire", shared, session, move_data = move_data)
    trial_design_server("trial_design", shared, move_data = move_data, session)
    sim_server("simulation", shared)
    con_server("conduct", shared)

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

##### List of Shared Variables #####

### Defined in qusetionnaire_server ###

# shared$q_n_doses: Number of doses response from questionnaire
# shared$q_start_dose: Starting dose level response from questionnaire
# shared$q_ttl: ttl response from questionnaire
# shared$q_cohort: Cohort size response from questionnaire
# shared$q_max_size: Maximum sample size response from questionnaire

### Defined in trial_design_server ###

# shared$n_dosess: Number of doses (be careful of two 's's! n_doses is a different variable)
# shared$ttl: ttl
# shared$max_size: Maximum sample size
# shared$start_dose: Starting dose level
# shared$cohort_size: Cohort size

# shared$skip_esc_crm: Skip escalation in CRM
# shared$skip_deesc_crm: Skip de-escalation in CRM
# shared$above_target_crm: Above target in CRM
# shared$prior_var_crm: Prior variance in CRM
# shared$stop_n_mtd_crm: Stop at n MTD in CRM
# shared$skeleton_crm: Skeleton in CRM
# shared$prior_mtd_crm: Prior MTD in CRM
# shared$stop_tox_x_crm: Stop toxicity x in CRM
# shared$stop_tox_y_crm: Stop toxicity y in CRM

# shared$boin_stopping_rule: Stopping rule in BOIN
# shared$boin_cohorts: Number of cohorts in BOIN
# shared$stop_n_mtd_boin: Early stopping value for BOIN

# shared$phi_1: Escalation probability for BOIN
# shared$phi_2: De-escalation probability for BOIN
