generate_recommendation <- function(x){
  if (x > 0 & x < 1 / 3) {
      rating <- c("crm", "tpt", "boin")
      "First choice is CRM, second choice is 3+3, third choice is BOIN."
  } else if (x > 1 / 3 & x < 2 / 3) {
      rating <- c("tpt", "crm", "boin")
      "First choice is 3+3, second choice is CRM, third choice is BOIN."
  } else {
      rating <- c("boin", "tpt", "crm")
      "First choice is BOIN, second choice is 3+3, third choice is CRM."
  }    
}