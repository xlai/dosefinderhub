library(shiny)

server <- function(input, output, session) {

  rand <- eventReactive(input$get_rating, {
    runif(1)
  })

  output$recommendations <- renderText({
    if (rand() > 0 & rand() < 1 / 3) {

      rating <- c("crm", "tpt", "other")
      "First choice is CRM, second choice is 3+3, third choice is other."

    } else if (rand() > 1 / 3 & rand() < 2 / 3) {

      rating <- c("tpt", "crm", "other")
      "First choice is 3+3, second choice is CRM, third choice is other."

    } else {

      rating <- c("other", "tpt", "crm")
      "First choice is other, second choice is 3+3, third choice is CRM."

    }
  })


}
