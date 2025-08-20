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
    "Performance Metrics" = (0.30*20),
    "Operational Constraints" = (0.25*20),
    "Study Population" = (0.20*20),
    "Infrastructure Capabilities" = (0.25*20)
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
  if (ttl > 0.15 && ttl < 0.34) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 2, BOIN = 2, "3+3" = 1)
  } else if (ttl < 0.15) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 2, "3+3" = 0)
  } else if (ttl > 0.34) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 2, BOIN = 1, "3+3" = 0);
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

#if (!is.null(user_responses$dlt_crucial)) {
  #crucial <- as.numeric(user_responses$dlt_crucial)
  #if (crucial == 1) {
 #   domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 1, "3+3" = 1)
 # } else if (crucial == 2) {
 #   domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 2, "3+3" = 1)
 # } else if (crucial == 3) {
 #   domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 1, BOIN = 2, "3+3" = 0)
 # } else if (crucial == 4) {
 #   domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 2, BOIN = 2, "3+3" = 0)
 # } else if (crucial == 5) {
  #  domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] + c(CRM = 3, BOIN = 2, "3+3" = 0)
 # }
#}


  if (!is.null(user_responses$decision_transparency)) {
    domain_scores[["Performance Metrics"]] <- domain_scores[["Performance Metrics"]] +
      switch(user_responses$decision_transparency,
             "Very important (regulatory/reproducibility)" = c(CRM = 0, BOIN = 3, "3+3" = 1),
             "Flexible adaptation preferred" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             "Simple fixed rules fine" = c(CRM = 0, BOIN = 1, "3+3" = 3),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # --- Operational Constraints ---
  if (!is.null(user_responses$time_resources)) {
    domain_scores[["Operational Constraints"]] <- domain_scores[["Operational Constraints"]] +
      switch(user_responses$time_resources,
             "Not limited by time/resources" = c(CRM = 2, BOIN = 2, "3+3" = 1),
             "Some availability in time/resources" = c(CRM = 1, BOIN = 2, "3+3" = 2),
             "Limited by time/resources" = c(CRM = 0, BOIN = 0, "3+3" = 3),
             c(CRM = 0, BOIN = 0, "3+3" = 0))
  }

  # --- Study Population ---
  if (!is.null(user_responses$cohort_size)) {
    cohort_size <- as.numeric(user_responses$cohort_size)
    if (cohort_size == 3) {
      domain_scores[["Study Population"]] <- domain_scores[["Study Population"]] + c(CRM = 1, BOIN = 1, "3+3" = 1)
    } else if (cohort_size > 3 && cohort_size < 3) {
      domain_scores[["Study Population"]] <- domain_scores[["Study Population"]] + c(CRM = 2, BOIN = 2, "3+3" = 0)
    }
  }

  # --- Infrastructure Capabilities ---
  if (!is.null(user_responses$statistical_support)) {
    domain_scores[["Infrastructure Capabilities"]] <- domain_scores[["Infrastructure Capabilities"]] +
      switch(user_responses$statistical_support,
             "Yes experienced with complex modeling" = c(CRM = 3, BOIN = 1, "3+3" = 0),
             "Yes but prefer simpler approaches" = c(CRM = 1, BOIN = 3, "3+3" = 1),
             "Limited statistical support" = c(CRM = 0, BOIN = 5, "3+3" = 8),
              "No statistician" = c(CRM = 0, BOIN = 3, "3+3" = 15),
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

  confidence <- if (score_gap >= 5) "High" else if (score_gap >= 2) "Medium" else "Low"

  # Suitability logic
  suitability_model <- if (!is.null(user_responses$statistical_support) &&
                           user_responses$statistical_support == "Limited statistical support" && "No statistician" %in% ranked_methods) {
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
  reasons <- c()
  flag_message <- NULL

  # --- Intro ---
  method_intro <- switch(top_method,
    "CRM" = "CRM (Continual Reassessment Method) is recommended because",
    "BOIN" = "BOIN (Bayesian Optimal Interval) is recommended because", 
    "3+3" = "The 3+3 design is recommended because"
  )
  rationale_parts <- c(rationale_parts, method_intro)

  # --- Performance Metrics ---
  if (!is.null(user_responses$toxicity_confidence)) {
    reasons <- c(reasons, switch(user_responses$toxicity_confidence,
      "Very confident (good historical data)" = if (top_method == "CRM") "you have strong historical toxicity data that CRM can leverage effectively" else NULL,
      "Somewhat confident" = if (top_method == "BOIN") "BOIN provides a good balance when toxicity confidence is moderate" else NULL,
      "Not confident/limited data" = if (top_method == "3+3") "with limited toxicity data, the 3+3 design provides a simple, well-understood approach" else NULL,
      NULL))
  }

  if (!is.null(user_responses$decision_transparency)) {
    reasons <- c(reasons, switch(user_responses$decision_transparency,
      "Very important (regulatory/reproducibility)" = if (top_method == "BOIN") "BOIN provides transparent, pre-specified decision boundaries" else NULL,
      "Flexible adaptation preferred" = if (top_method == "CRM") "CRM allows flexible adaptation based on accumulating data" else NULL,
      "Simple fixed rules fine" = if (top_method == "3+3") "the 3+3 design uses simple, fixed escalation rules" else NULL,
      NULL))
  }

  # --- Operational Constraints ---
 #if (!is.null(user_responses$dlt_crucial)) {
 ## dlt_crucial <- suppressWarnings(as.numeric(user_responses$dlt_crucial))
 # if (!is.na(dlt_crucial)) {
 ##   if (dlt_crucial == 1 && top_method == "3+3") {
#reasons <- c(reasons, "you’re not prioritizing DLT estimation, so the simplicity of 3+3 is suitable")
 #   } else if (dlt_crucial == 2 && top_method == "BOIN") {
 #     reasons <- c(reasons, "you want some DLT estimation ability without complexity, which BOIN supports")
 #   } else if (dlt_crucial == 3 && top_method == "BOIN") {
 #     reasons <- c(reasons, "BOIN offers a balance between DLT estimation and simplicity")
  #  } else if (dlt_crucial == 4 && top_method == "CRM") {
 #     reasons <- c(reasons, "you value accurate DLT estimation, and CRM provides strong modeling capabilities")
 #   } else if (dlt_crucial == 5 && top_method == "CRM") {
 #     reasons <- c(reasons, "you highly prioritize DLT estimation, and CRM is best suited for that goal")
 #   }
 # }
#}
#question taken out of csv
#Y,8,slider,dlt_crucial,How crucial is DLT accuracy? (1 = not very, 5 = absolutely)?,numeric_bounded,min=1;max=5;step=1,

  if (!is.null(user_responses$time_resources)) {
    reasons <- c(reasons, switch(user_responses$time_resources,
      "Limited by time/resources" = if (top_method == "3+3") "you are constrained by time or resources, making 3+3 a practical choice" else NULL,
      "Some availability in time/resources" = switch(top_method,
        "BOIN" = "BOIN offers a balance between modeling and practicality under moderate resource constraints",
        "3+3" = "3+3 is simple and can work with moderate resources",
        NULL),
      "Not limited by time/resources" = switch(top_method,
        "CRM" = "you have resources to support more complex modeling",
        "BOIN" = "BOIN can be used effectively when resources are available",
        NULL),
      NULL))
  }

  # --- Study Population ---
  if (!is.null(user_responses$cohort_size)) {
    cohort_size <- as.numeric(user_responses$cohort_size)

    if (cohort_size == 3) {
      if (top_method == "3+3") {
        reasons <- c(reasons, "the cohort size of 3 is optimal for the 3+3 design")
      } else {
        reasons <- c(reasons, "While a cohort size of 3 is standard for 3+3, other designs like CRM and BOIN can still be used")
      }
    } else {
      if (top_method == "CRM") {
        reasons <- c(reasons, "CRM is well-suited for larger or variable cohort sizes")
      } else if (top_method == "BOIN") {
        reasons <- c(reasons, "BOIN can flexibly accommodate larger cohort sizes")
      } else if (top_method == "3+3") {
        reasons <- c(reasons, "the 3+3 design is typically used with cohorts of 3, so this size may be less optimal")
      }
    }
  }

  # --- Infrastructure Capabilities ---
if (!is.null(user_responses$statistical_support)) {
  stat_support <- user_responses$statistical_support

  if (stat_support == "Yes experienced with complex modeling" && top_method == "CRM") {
    reasons <- c(reasons, "your statistical expertise enables effective use of CRM's adaptive modeling")
  } else if (stat_support == "Yes but prefer simpler approaches") {
    if (top_method == "BOIN") {
      reasons <- c(reasons, "BOIN provides statistical rigor without excessive complexity")
    } else if (top_method == "3+3") {
      reasons <- c(reasons, "you prefer simpler approaches, and 3+3 offers the most straightforward implementation")
    }
  } else if (stat_support == "Limited statistical support") {
    if (top_method == "3+3") {
      reasons <- c(reasons, "the 3+3 design requires minimal statistical expertise to implement")
    } else if (top_method == "BOIN") {
      reasons <- c(reasons, "BOIN can be used with limited statistical support while maintaining rigor")
    }
  } else if (stat_support == "No statistician") {
    if (top_method == "3+3") {
      reasons <- c(reasons, "3+3 is the most feasible design when no statistician is available")
    } else if (top_method == "BOIN") {
      reasons <- c(reasons, "BOIN can still be implemented without a statistician, though support is recommended")
    }
  }

  # Simple flag message condition
  if (stat_support %in% c("Limited statistical support", "No statistician")) {
    flag_message <- "⚠️ Your answer to the question about statistical support heavily influenced the outcome of this recommendation, in favour of 3+3. Consider consulting a statistician to gain the benefits of the other trial designs."
  }
}

# Display the flag message
if (!is.null(flag_message)) {
  cat(flag_message)
}



  # --- Combine Reasoning ---
  if (length(reasons) > 0) {
    reason_text <- paste(reasons[!is.null(reasons)], collapse = ", and ")
    rationale_parts <- c(rationale_parts, reason_text)
  }

  # --- Method Description ---
  method_desc <- switch(top_method,
    "CRM" = "CRM continuously updates dose-toxicity estimates using Bayesian methods, making it highly efficient but requiring more statistical expertise.",
    "BOIN" = "BOIN uses pre-specified decision boundaries that balance efficiency with transparency, making it suitable for many regulatory contexts.",
    "3+3" = "The 3+3 design follows simple escalation rules (treat 3, escalate if 0/3 toxicities), making it the most straightforward to implement"
  )

  # --- Caveat if Scores Are Close ---
  max_score <- max(scores)
  second_score <- sort(scores, decreasing = TRUE)[2]
  caveat <- if (max_score - second_score <= 5) {
    sprintf("\n\nNote: The scores are quite close (difference of %.2f), so %s could also be a reasonable choice depending on your specific context",
            max_score - second_score, names(sort(scores, decreasing = TRUE))[2])
  } else {
    ""
  }

  # --- Final Output ---
  full_rationale <- paste(c(rationale_parts, method_desc, flag_message, caveat), collapse = ". ")
  return(full_rationale)
}
