# Questionnaire recommendation (R/utils_questionnaire.R). Formerly the print-out
# script tests/test_recommendations.R. Answers are choices offered in
# Inputs/questions.csv.

recommend <- function(...) {
  # The function prints its warnings to the console; keep the test log quiet.
  utils::capture.output(result <- generate_intelligent_recommendation(list(...)))
  result
}

test_that("strong data and an experienced statistician recommend CRM", {
  result <- recommend(
    toxicity_confidence = "Very confident (good historical data)",
    statistical_support = "Yes experienced with complex modeling",
    decision_transparency = "Flexible adaptation preferred"
  )
  expect_equal(result$ranked_methods[1], "CRM")
})

test_that("moderate confidence and transparency recommend BOIN", {
  result <- recommend(
    toxicity_confidence = "Somewhat confident",
    statistical_support = "Yes but prefer simpler approaches",
    decision_transparency = "Very important (regulatory/reproducibility)"
  )
  expect_equal(result$ranked_methods[1], "BOIN")
})

test_that("limited data and limited support recommend 3+3", {
  result <- recommend(
    toxicity_confidence = "Not confident/limited data",
    statistical_support = "Limited statistical support",
    decision_transparency = "Simple fixed rules fine"
  )
  expect_equal(result$ranked_methods[1], "3+3")
})

test_that("mixed answers give close scores and low confidence", {
  # Rebuilt from the current weights, see
  # https://github.com/xlai/dosefinderhub/issues/93
  result <- recommend(
    toxicity_confidence = "Very confident (good historical data)",
    time_resources = "Some availability in time/resources",
    statistical_support = "Yes experienced with complex modeling",
    decision_transparency = "Very important (regulatory/reproducibility)",
    ttl = 0.25,
    cohort_size = 3
  )
  scores <- sort(result$scores, decreasing = TRUE)
  expect_lte(unname(scores[1] - scores[2]), 2)
  expect_equal(result$confidence, "Low")
})

test_that("missing answers still give a full ranking", {
  result <- recommend(toxicity_confidence = "Very confident (good historical data)")
  expect_setequal(result$ranked_methods, c("CRM", "BOIN", "3+3"))
  expect_true(nzchar(result$rationale))
})

# Every answer the questionnaire offers for the scored questions.
all_answers <- function() {
  choices <- function(var) {
    params <- parse_params(questions$params[questions$q_variable == var])
    strsplit(params[["choices"]], ",")[[1]]
  }
  questions <- read.csv(test_path("..", "..", "Inputs", "questions.csv"))
  expand.grid(
    ttl = seq(0, 0.8, by = 0.05),
    cohort_size = 1:6,
    toxicity_confidence = choices("toxicity_confidence"),
    time_resources = choices("time_resources"),
    statistical_support = choices("statistical_support"),
    decision_transparency = choices("decision_transparency"),
    stringsAsFactors = FALSE
  )
}

test_that("no statistician always recommends 3+3", {
  answers <- all_answers()
  answers <- answers[answers$statistical_support == "No statistician", ]
  results <- lapply(seq_len(nrow(answers)), function(i) do.call(recommend, as.list(answers[i, ])))
  expect_equal(unique(vapply(results, `[[`, "", "suitability_model")), "3+3")
  expect_equal(unique(vapply(results, function(r) r$ranked_methods[1], "")), "3+3")
})

test_that("every cohort size scores, and sizes other than 3 favour CRM and BOIN", {
  # Study Population weight is 4.
  expect_equal(recommend(cohort_size = 3)$scores, c(CRM = 4, BOIN = 4, "3+3" = 4))
  for (size in c(1, 2, 4, 5, 6)) {
    expect_equal(recommend(cohort_size = size)$scores, c(CRM = 8, BOIN = 8, "3+3" = 0),
                 label = paste("cohort size", size))
  }
})

test_that("every target the question allows scores, including the band edges", {
  # Performance Metrics weight is 6.
  middle <- c(CRM = 12, BOIN = 12, "3+3" = 6)
  expect_equal(recommend(ttl = 0.1)$scores, c(CRM = 6, BOIN = 12, "3+3" = 0))
  expect_equal(recommend(ttl = 0.15)$scores, middle)
  expect_equal(recommend(ttl = 0.34)$scores, middle)
  expect_equal(recommend(ttl = 0.35)$scores, c(CRM = 12, BOIN = 6, "3+3" = 0))
  for (ttl in seq(0, 0.8, by = 0.05)) {
    expect_gt(sum(recommend(ttl = ttl)$scores), 0, label = paste("target", ttl))
  }
})

test_that("the confidence, the close-scores note and the 3+3 warning agree with the scores", {
  answers <- all_answers()
  results <- lapply(seq_len(nrow(answers)), function(i) do.call(recommend, as.list(answers[i, ])))
  confidence <- vapply(results, `[[`, "", "confidence")
  rationale <- vapply(results, `[[`, "", "rationale")
  top <- vapply(results, function(r) r$ranked_methods[1], "")

  # The note says the scores are close exactly when the confidence is not High.
  expect_equal(grepl("scores are quite close", rationale), confidence != "High")
  # The warning says the answer favoured 3+3 only when 3+3 comes first.
  expect_false(any(grepl("in favour of 3\\+3", rationale) & top != "3+3"))
  # Every sentence ends with one full stop.
  expect_false(any(grepl("because\\.|\\.\\.", rationale)))
  expect_true(all(endsWith(rationale, ".")))
})
