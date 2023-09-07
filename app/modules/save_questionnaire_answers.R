save_questionnaire_answers <- function(questions) {
  inputs_to_save <- questions$q_variable
  # Declare inputs
  inputs <- NULL
  # Append all inputs before saving to folder
  for (input.i in inputs_to_save){
    inputs <- append(inputs, input[[input.i]])
  }
  # Inputs data.frame
  inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)

  content <- write.csv(inputs_data_frame, file)
  filename <- paste("user_responses-", Sys.Date(), ".csv", sep = "")
  value <- list(content, filename)
  value
}


  save_questionnaire_answers <- function(questions, input) {
    inputs_to_save <- questions$q_variable
    # Declare inputs
    inputs <- NULL
    # Append all inputs before saving to folder
    for (input.i in inputs_to_save){
      inputs <- append(inputs, input[[input.i]])
    }
    # Inputs data.frame
    inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)

    content <- write.csv(inputs_data_frame, file)
    content
  }