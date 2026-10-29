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
