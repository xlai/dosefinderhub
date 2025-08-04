# File: R/utils_questionnaire.R
# Helper functions for questionnaire module

#' Parse conditions string for conditional question navigation
#' @param condition_str String containing condition in format "operator;value"
#' @return Matrix with operator and value
parse_conditions <- function(condition_str) {
  if (is.null(condition_str) || is.na(condition_str) || condition_str == "") {
    return(cbind(NA, NA))
  }
  conditions <- strsplit(condition_str, ";")[[1]]
  cond_var <- conditions[1]
  cond_val <- conditions[2]
  cbind(cond_var, cond_val)
}

#' Parse parameter string into list (your existing function)
#' @param params_str String containing parameters separated by semicolons
#' @return Named list of parameters
parse_params <- function(params_str) {
  if (is.null(params_str) || is.na(params_str) || params_str == "") return(list())
  params <- strsplit(params_str, ";")[[1]]
  param_list <- lapply(params, function(p) strsplit(p, "=")[[1]])
  names(param_list) <- sapply(param_list, `[`, 1)
  sapply(param_list, function(x) if (length(x) > 1) x[2] else NA)
}

#' Generate questions UI (your existing function)
#' @param current_question Single row data frame containing question info
#' @return Shiny input UI element
generate_intelligent_recommendation <- function(user_responses) {
  
  # Define domain weights
  domain_weights <- list(
    "Performance Metrics" = 0.30,
    "Operational Constraints" = 0.25,
    "Study Population" = 0.20,
    "Infrastructure Capabilities" = 0.25
  )
  
  # Initialize domain-wise score containers
  domain_scores <- list(
    "Performance Metrics" = c(CRM = 0, BOIN = 0, "3+3" = 0),
    "Operational Constraints" = c(CRM = 0, BOIN = 0, "3+3" = 0),
    "Study Population" = c(CRM = 0, BOIN = 0, "3+3" = 0),
    "Infrastructure Capabilities" = c(CRM = 0, BOIN = 0, "3+3" = 0)
  )

  # --- Performance Metrics ---
 if (!is.null(user_responses$ttl)) {
  ttl <- as.numeric(user_responses$ttl)
  if (ttl > 0.28 && ttl < 0.38) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 1, "3+3" = 1)
  } else if (ttl < 0.28) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 2, "3+3" = 0)
  } else if (ttl > 0.38) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 2, BOIN = 1, "3+3" = 0)
  }
 }

  if (!is.null(user_responses$toxicity_confidence)) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] +
      switch(user_responses$toxicity_confidence,
             "Very confident (good historical data)" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             "Somewhat confident" = c(CRM = 1, BOIN = 3, "3+3" = 1),
             "Not confident/limited data" = c(CRM = 0, BOIN = 1, "3+3" = 3),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  if (!is.null(user_responses$dlt_accuracy)) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] +
      switch(user_responses$dlt_accuracy,
             "As much as possible" = c(CRM = 1, BOIN = 1, "3+3" = 0),
             "Other things are the priority" = c(CRM = 1, BOIN = 1, "3+3" = 1),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  if (!is.null(user_responses$decision_transparency)) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] +
      switch(user_responses$decision_transparency,
             "Very important (regulatory/reproducibility)" = c(CRM = 0, BOIN = 3, "3+3" = 1),
             "Flexible adaptation preferred" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             "Simple fixed rules fine" = c(CRM = 0, BOIN = 1, "3+3" = 3),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # --- Operational Constraints ---
  if (!is.null(user_responses$time_budget_availability)) {
    domain_scores[["Operational Constraints"]] <- domain_scores[["Operational Constraints"]] +
      switch(user_responses$time_budget_availability,
             "Not limited by time/budget" = c(CRM = 2, BOIN = 2, "3+3" = 1),
             "Some availability in time/budget" = c(CRM = 1, BOIN = 2, "3+3" = 2),
             "Limited by time/budget" = c(CRM = 0, BOIN = 0, "3+3" = 3),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # --- Study Population ---
  if (!is.null(user_responses$cohort_size)) {
    domain_scores[["Study Population"]] <- domain_scores[["Study Population"]] +
      switch(user_responses$cohort_size,
             "Small cohorts (1-2 patients)" = c(CRM = 2, BOIN = 2, "3+3" = 1),
             "Standard cohorts (3 patients)" = c(CRM = 1, BOIN = 2, "3+3" = 3),
             "Flexible cohort sizes" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # --- Infrastructure Capabilities ---
  if (!is.null(user_responses$statistical_support)) {
    domain_scores[["Infrastructure Capabilities"]] <- domain_scores[["Infrastructure Capabilities"]] +
      switch(user_responses$statistical_support,
             "Yes experienced with complex modeling" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             "Yes but prefer simpler approaches" = c(CRM = 1, BOIN = 3, "3+3" = 1),
             "Limited statistical support" = c(CRM = 0, BOIN = 1, "3+3" = 8),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # Apply domain weights
  weighted_scores <- Reduce(`+`, lapply(names(domain_scores), function(domain) {
    domain_scores[[domain]] * domain_weights[[domain]]
  }))

  # Determine top scoring model
  ranked_methods <- names(sort(weighted_scores, decreasing = TRUE))
  max_score <- weighted_scores[ranked_methods[1]]
  second_score <- weighted_scores[ranked_methods[2]]
  score_gap <- max_score - second_score

  confidence <- if (score_gap >= 3) "High" else if (score_gap >= 2) "Medium" else "Low"

  # Suitability logic
  suitability_model <- if (!is.null(user_responses$statistical_support) &&
                           user_responses$statistical_support == "Limited statistical support") {
    "3+3"
  } else {
    ranked_methods[1]
  }

  # Generate rationale
  rationale <- generate_rationale(user_responses, ranked_methods[1], weighted_scores)

  # Recommendation text
  recommendation_text <- sprintf(
    "Top-scoring model: %s (score: %.2f)\nMost suitable model: %s\n\nConfidence: %s\n\n%s",
    ranked_methods[1], weighted_scores[ranked_methods[1]],
    suitability_model, confidence, rationale
  )

  return(list(
    ranked_methods = ranked_methods,
    scores = weighted_scores,
    confidence = confidence,
    suitability_model = suitability_model,
    rationale = rationale,
    recommendation_text = recommendation_text
  ))
}


#' Generate recommendation (your existing function)
#' @param x Numeric score for recommendation
#' @return Character string with recommendation
generate_recommendation <- function(x) {
  if (x > 0 & x < 1 / 3) {
    "First choice is CRM, second choice is 3+3, third choice is BOIN."
  } else if (x > 1 / 3 & x < 2 / 3) {
    "First choice is 3+3, second choice is CRM, third choice is BOIN."
  } else {
    "First choice is BOIN, second choice is 3+3, third choice is CRM."
  }
}

#' Generate intelligent method recommendation based on user responses
#' @param user_responses Named list containing user responses to recommendation questions
#' @return List with ranked methods, scores, confidence, rationale, and display text

#' Generate explanatory rationale for recommendation
#' @param user_responses User responses to recommendation questions
#' @param top_method The highest-scoring method
#' @param scores Named vector of method scores
#' @return Character string explaining the recommendation
generate_rationale <- function(user_responses, top_method, scores) {
  
  rationale_parts <- c()
  
  method_intro <- switch(top_method,
    "CRM" = "CRM (Continual Reassessment Method) is recommended because",
    "BOIN" = "BOIN (Bayesian Optimal Interval) is recommended because", 
    "3+3" = "The 3+3 design is recommended because"
  )
  
  rationale_parts <- c(rationale_parts, method_intro)
  reasons <- c()

  # --- Performance Metrics ---
  if (!is.null(user_responses$toxicity_confidence)) {
    reasons <- c(reasons, switch(user_responses$toxicity_confidence,
      "Very confident (good historical data)" = if (top_method == "CRM") "you have strong historical toxicity data that CRM can leverage effectively",
      "Somewhat confident" = if (top_method == "BOIN") "BOIN provides a good balance when toxicity confidence is moderate",
      "Not confident/limited data" = if (top_method == "3+3") "with limited toxicity data, the 3+3 design provides a simple, well-understood approach",
      NULL))
  }

  if (!is.null(user_responses$dlt_accuracy)) {
    reasons <- c(reasons, switch(user_responses$dlt_accuracy,
      "As much as possible" = if (top_method == "CRM") "you prioritize accurate DLT estimation, which CRM supports",
      "Other things are the priority" = if (top_method == "3+3") "you prefer simplicity over detailed DLT modeling",
      NULL))
  }

  if (!is.null(user_responses$decision_transparency)) {
    reasons <- c(reasons, switch(user_responses$decision_transparency,
      "Very important (regulatory/reproducibility)" = if (top_method == "BOIN") "BOIN provides transparent, pre-specified decision boundaries",
      "Flexible adaptation preferred" = if (top_method == "CRM") "CRM allows flexible adaptation based on accumulating data",
      "Simple fixed rules fine" = if (top_method == "3+3") "the 3+3 design uses simple, fixed escalation rules",
      NULL))
  }

  # --- Operational Constraints ---
  if (!is.null(user_responses$trial_priorities)) {
    reasons <- c(reasons, switch(user_responses$trial_priorities,
      "Finding the best dose efficiently" = if (top_method == "CRM") "CRM is most efficient at finding the maximum tolerated dose",
      "Getting started quickly with simple rules" = if (top_method == "3+3") "the 3+3 design offers the simplest implementation with fixed decision rules",
      "Balance of both" = if (top_method == "BOIN") "BOIN balances efficiency with simplicity",
      NULL))
  }

  if (!is.null(user_responses$time_budget_availability)) {
    reasons <- c(reasons, switch(user_responses$time_budget_availability,
      "Limited by time/budget" = if (top_method == "3+3") "you are constrained by time or budget, making 3+3 a practical choice",
      "Not limited by time/budget" = if (top_method == "CRM") "you have resources to support more complex modeling",
      NULL))
  }

  # --- Study Population ---
  if (!is.null(user_responses$max_sample_size)) {
    reasons <- c(reasons, switch(user_responses$max_sample_size,
      "x > ?" = if (top_method == "CRM") "you have a large sample size, which CRM can utilize effectively",
      "x < ?" = if (top_method == "3+3") "your sample size is limited, making 3+3 more feasible",
      NULL))
  }

  if (!is.null(user_responses$cohort_size)) {
    reasons <- c(reasons, switch(user_responses$cohort_size,
      "Small cohorts (1-2 patients)" = if (top_method == "CRM") "CRM supports flexible cohort sizes including small cohorts",
      "Standard cohorts (3 patients)" = if (top_method == "3+3") "you prefer standard cohort sizes, which align with 3+3",
      "Flexible cohort sizes" = if (top_method == "CRM") "CRM accommodates flexible cohort sizes",
      NULL))
  }

  # --- Infrastructure Capabilities ---
  if (!is.null(user_responses$statistical_support)) {
    reasons <- c(reasons, switch(user_responses$statistical_support,
      "Yes experienced with complex modeling" = if (top_method == "CRM") "your statistical expertise enables effective use of CRM's adaptive modeling",
      "Yes but prefer simpler approaches" = if (top_method == "BOIN") "BOIN provides statistical rigor without excessive complexity",
      "Limited statistical support" = if (top_method == "3+3") "the 3+3 design requires minimal statistical expertise to implement",
      NULL))
  }

  # Combine reasons
  if (length(reasons) > 0) {
    reason_text <- paste(reasons[!is.null(reasons)], collapse = ", and ")
    rationale_parts <- c(rationale_parts, reason_text)
  }

  # Add method characteristics
  method_desc <- switch(top_method,
    "CRM" = "CRM continuously updates dose-toxicity estimates using Bayesian methods, making it highly efficient but requiring more statistical expertise.",
    "BOIN" = "BOIN uses pre-specified decision boundaries that balance efficiency with transparency, making it suitable for many regulatory contexts.",
    "3+3" = "The 3+3 design follows simple escalation rules (treat 3, escalate if 0/3 toxicities), making it the most straightforward to implement."
  )

  # Add caveat if scores are close
  max_score <- max(scores)
  second_score <- sort(scores, decreasing = TRUE)[2]
  caveat <- if (max_score - second_score <= 1) {
    sprintf("\n\nNote: The scores are quite close (difference of %.2f), so %s could also be a reasonable choice depending on your specific context.",
            max_score - second_score, names(sort(scores, decreasing = TRUE))[2])
  } else {
    ""
  }

  full_rationale <- paste(c(rationale_parts, method_desc, caveat), collapse = ". ")
  return(full_rationale)
}