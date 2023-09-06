
parse_params <- function(params_str) {
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, `[`, 2)
}

generate_dummy_data <- function(file_path_config, file_path_method) {
  # Read the CSV file
  config_questions <- read.csv(file_path_config, stringsAsFactors = FALSE)
  method_questions <- read.csv(file_path_method, stringsAsFactors = FALSE)

  # Create an empty data frame for the dummy answers
  dummy_answers <- data.frame(answer = character(), method = character())

  # Loop through each question and generate a dummy answer
  n_doses <- NULL

  for (i in config_questions[config_questions$config == "Y", ]$q_number) {
    q_variable <- config_questions[config_questions$q_number == i, ]$q_variable
    answer_type <- config_questions[config_questions$q_number == i, ]$answer_type
    params <- parse_params(config_questions[config_questions$q_number == i, ]$params)

    # Generate a dummy answer based on the type
    answer <- switch(answer_type,
      categorical = sample(strsplit(params[["choices"]], ",")[[1]], 1),
      numeric = sample(seq(from = as.numeric(params[["min"]]),
                           to = as.numeric(params[["dummy_max"]]),
                           by = as.numeric(params[["step"]])), 1),
      numeric_bounded = sample(seq(from = as.numeric(params[["min"]]),
                                   to = as.numeric(params[["max"]]),
                                   by = as.numeric(params[["step"]])), 1),
      "comma-separated list" = {
        # Make sure n_doses has been set
        if (!is.null(n_doses)) {
          # Generate a non-decreasing sequence
          sequence <- sort(runif(n_doses, 0, 1))
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

    if (q_variable == "start_dose" && (answer > n_doses)) {
      answer <- sample(
        as.numeric(params[["min"]]):n_doses, 1
      )
    }

    if (q_variable == "prior_mtd" && (answer > n_doses)) {
      answer <- sample(
        as.numeric(params[["min"]]):n_doses, 1
      )
    }

    # Add to the dummy answers data frame
    dummy_answers <- rbind(dummy_answers,
      data.frame(answer = as.character(answer), method = as.character("config"))
    )
  }

  for (i in seq_len(nrow(method_questions))) {
    q_variable <- method_questions$q_variable[i]
    answer_type <- method_questions$answer_type[i]
    params <- parse_params(method_questions$params[i])

    # Generate a dummy answer based on the type
    answer <- switch(answer_type,
      categorical = sample(strsplit(params[["choices"]], ",")[[1]], 1),
      numeric = sample(seq(from = as.numeric(params[["min"]]),
                           to = as.numeric(params[["dummy_max"]]),
                           by = as.numeric(params[["step"]])), 1),
      numeric_bounded = sample(seq(from = as.numeric(params[["min"]]),
                                   to = as.numeric(params[["max"]]),
                                   by = as.numeric(params[["step"]])), 1),
      "comma-separated list" = {
        sequence <- sort(runif(n_doses, 0, 1))
        paste(sequence, collapse = ",")
      }
    )

    # Add to the dummy answers data frame
    dummy_answers <- rbind(dummy_answers,
      data.frame(answer = as.character(answer),
                 method = as.character(method_questions$design[i]))
    )
  }

  rank_seq <- sample(c("tpt", "crm", "other"), 3, replace = FALSE)
  ranking <- paste(rank_seq, collapse = ",")
  dummy_answers <- rbind(dummy_answers,
    data.frame(answer = as.character(ranking), method = as.character("NA"))
  )


  rownames(dummy_answers) <- cbind(
    t(config_questions[config_questions$config == "Y", ]$q_variable),
    t(method_questions$q_variable),
    "ranking"
  )
  return(dummy_answers)
}





# Example usage
dummy_data <- generate_dummy_data(
  "app/data/questionnaire_inputs/q_database.csv",
  "app/data/questionnaire_inputs/method_q_database.csv"
)
print(dummy_data)
write.csv(dummy_data, 'dummy_data.csv')
