# Test script for intelligent recommendation system
# Load the functions
source('app_skeleton/R/utils_questionnaire.R')

# Test Case 1: CRM scenario
# High confidence + efficiency priority + statistical support → Should recommend CRM first
cat("=== Test Case 1: CRM Scenario ===\n")
crm_responses <- list(
  toxicity_confidence = "Very confident (good historical data)",
  trial_priorities = "Finding the best dose efficiently", 
  statistical_support = "Yes experienced with complex modeling",  
  decision_transparency = "Flexible adaptation preferred"
)

crm_result <- generate_intelligent_recommendation(crm_responses)
cat("Top recommendation:", crm_result$ranked_methods[1], "\n")
cat("Scores:", paste(names(crm_result$scores), crm_result$scores, sep="=", collapse=", "), "\n")
cat("Confidence:", crm_result$confidence, "\n")
cat("Expected: CRM should be first\n")
cat("Result:", if(crm_result$ranked_methods[1] == "CRM") "PASS" else "FAIL", "\n\n")

# Test Case 2: BOIN scenario
# Medium confidence + balance priority + transparency important → Should recommend BOIN first
cat("=== Test Case 2: BOIN Scenario ===\n")
boin_responses <- list(
  toxicity_confidence = "Somewhat confident",
  trial_priorities = "Balance of both",
  statistical_support = "Yes but prefer simpler approaches", 
  decision_transparency = "Very important (regulatory/reproducibility)"
)

boin_result <- generate_intelligent_recommendation(boin_responses)
cat("Top recommendation:", boin_result$ranked_methods[1], "\n")
cat("Scores:", paste(names(boin_result$scores), boin_result$scores, sep="=", collapse=", "), "\n")
cat("Confidence:", boin_result$confidence, "\n")
cat("Expected: BOIN should be first\n")
cat("Result:", if(boin_result$ranked_methods[1] == "BOIN") "PASS" else "FAIL", "\n\n")

# Test Case 3: 3+3 scenario
# Low confidence + simple priority + limited support → Should recommend 3+3 first
cat("=== Test Case 3: 3+3 Scenario ===\n")
three_plus_three_responses <- list(
  toxicity_confidence = "Not confident/limited data",
  trial_priorities = "Getting started quickly with simple rules",
  statistical_support = "Limited statistical support",
  decision_transparency = "Simple fixed rules fine"
)

three_result <- generate_intelligent_recommendation(three_plus_three_responses)
cat("Top recommendation:", three_result$ranked_methods[1], "\n")
cat("Scores:", paste(names(three_result$scores), three_result$scores, sep="=", collapse=", "), "\n")
cat("Confidence:", three_result$confidence, "\n")
cat("Expected: 3+3 should be first\n")
cat("Result:", if(three_result$ranked_methods[1] == "3+3") "PASS" else "FAIL", "\n\n")

# Test Case 4: Tie scenario
# Mixed responses that should result in close scores
cat("=== Test Case 4: Tie Scenario ===\n")
tie_responses <- list(
  toxicity_confidence = "Somewhat confident", # BOIN gets 3 points
  trial_priorities = "Finding the best dose efficiently", # CRM gets 3 points
  statistical_support = "Yes but prefer simpler approaches", # BOIN gets 3 points
  decision_transparency = "Flexible adaptation preferred" # CRM gets 3 points
)

tie_result <- generate_intelligent_recommendation(tie_responses)
cat("Top recommendation:", tie_result$ranked_methods[1], "\n")
cat("Scores:", paste(names(tie_result$scores), tie_result$scores, sep="=", collapse=", "), "\n")
cat("Confidence:", tie_result$confidence, "\n")
max_score <- max(tie_result$scores)
second_score <- sort(tie_result$scores, decreasing = TRUE)[2]
score_gap <- max_score - second_score
cat("Score gap:", score_gap, "\n")
cat("Expected: Close scores (gap <= 2), Low/Medium confidence\n")
cat("Result:", if(score_gap <= 2) "PASS" else "FAIL", "\n\n")

# Test Case 5: Missing responses
cat("=== Test Case 5: Missing Responses ===\n")
partial_responses <- list(
  toxicity_confidence = "Very confident (good historical data)",
  trial_priorities = "Finding the best dose efficiently"
  # Missing statistical_support and decision_transparency
)

partial_result <- generate_intelligent_recommendation(partial_responses)
cat("Top recommendation:", partial_result$ranked_methods[1], "\n")
cat("Scores:", paste(names(partial_result$scores), partial_result$scores, sep="=", collapse=", "), "\n")
cat("Confidence:", partial_result$confidence, "\n")
cat("Expected: Should handle gracefully, CRM likely favored\n")
cat("Result:", if(!is.null(partial_result$ranked_methods[1])) "PASS" else "FAIL", "\n\n")

# Display sample rationale
cat("=== Sample Rationale (CRM case) ===\n")
cat(crm_result$rationale, "\n")