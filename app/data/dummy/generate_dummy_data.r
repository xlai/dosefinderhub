
parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}

generate_dummy_data <- function(file_path) {
  # Read the CSV file
  questions <- read.csv(file_path, stringsAsFactors = FALSE)

  # Create an empty data frame for the dummy answers
  dummy_answers <- data.frame(answer = character())

  # Loop through each question and generate a dummy answer
  n_doses <- NULL

  for (i in seq_len(nrow(questions))) {
    q_variable <- questions$q_variable[i]
    answer_type <- questions$answer_type[i]
    params <- parse_params(questions$params[i])

    # Generate a dummy answer based on the type
    answer <- switch(answer_type,
      categorical = sample(strsplit(params[["choices"]], ",")[[1]], 1),
      numeric = runif(1, as.numeric(params[["min"]])),
      ##     numeric = ifelse(grepl("min", params), as.numeric(params[["min"]]), runif(1)),
      numeric_bounded = sample(
        as.numeric(params[["min"]]):as.numeric(params[["max"]]), 1
      ),
      "comma-separated list" = {
        # Make sure n_doses has been set
        if (!is.null(n_doses)) {
          # Generate a non-decreasing sequence
          sequence <- cummax(runif(n_doses, 0, 1))
          paste(sequence, collapse = ",")
        } else {
          # If n_doses has not been set yet, default to a random value
          paste(runif(4, 0, 1), collapse = ",")
        }
      }
    )

    if (q_variable == "n_doses") {
      n_doses <- as.integer(answer)
    }

    # Add to the dummy answers data frame
    dummy_answers <- rbind(dummy_answers,
      data.frame(answer = as.character(answer))
    )
  }
  rownames(dummy_answers) <- questions$q_variable
  return(dummy_answers)
}

# Example usage
dummy_data <- generate_dummy_data("app/data/questionnaire_inputs/dummy_database.csv")
print(dummy_data)
