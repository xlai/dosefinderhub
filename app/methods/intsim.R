

# setup piping. we want to avoid loading packages with library()
`%>%` <- magrittr::`%>%`

find_closest_element <- function(vec, num) {
  # Find the index of the closest element in the vector to the given number
  closest_index <- which.min(abs(vec - num))
  
  # Create a boolean vector with 1 for the closest index and 0 otherwise
  boolean_vec <- rep(0, length(vec))
  boolean_vec[closest_index] <- 1
  
  return(boolean_vec)
}

# create some sim data
create_dummy_sims <- function(n_doses) {

  n_sims <- sample(seq(20, 100, 10), 1)

  true_dlt_ss <- list()
  true_dlt_ss[[1]] <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss[[2]] <- sort(sample(seq(0, 1, 0.01), n_doses))
  true_dlt_ss[[3]] <- sort(sample(seq(0, 1, 0.01), n_doses))

  value <- list(n_sims = n_sims, true_dlt_ss = true_dlt_ss)

  saveRDS(value, "dummy_sims_data")
  dummy_sims_data <- readRDS("dummy_sims_data")
  return(dummy_sims_data)
}

# function that processes config into machine-readable format
process_config <- function(model_config, model_type){
  # model_config is a list of three elements - trial, method and ranking
  # subset config that contains only model_type
  config_method <- model_config$method %>% 
    dplyr::filter(design == model_type) %>% 
    dplyr::select(q_variable, value, answer_type)
  # the trial object only contains non-specific info, but we still need to subset the config-specific info
  config_trial <- model_config$trial %>% 
    dplyr::filter(config == "Y") %>% 
    dplyr::select(q_variable, value, answer_type)

  #config_processed <- dplyr::bind_rows(config_method, config_trial)
  config_processed <- data.frame(rbind(config_method, config_trial))  
  # convert this dataframe into a list, where each element is q_variable
  config_processed_list <- setNames(
    as.list(config_processed$value), 
    as.list(config_processed$q_variable)
  )

  # format all data
  for (i in 1:length(config_processed_list)){
      if (config_processed$answer_type[i] == "numeric"){
        config_processed_list[[i]] <- as.numeric(config_processed$value[i])}
      else if (config_processed$answer_type[i] == "numeric_bounded"){
        config_processed_list[[i]] <- as.numeric(config_processed$value[i])}
      else if (config_processed$value[i] == "No"){
        config_processed_list[[i]] <- FALSE}
      else if (config_processed$value[i] == "Yes"){
        config_processed_list[[i]] <- TRUE}
      else if (config_processed$answer_type[i] == "comma-separated list"){
        config_processed_list[[i]] <- as.numeric(unlist(strsplit(config_processed_list[[i]],",")))}
      }

return(config_processed_list)
}

# function for model creation
create_model <- function(model_config, model_type) {
  
  # pre-process config
  config <- process_config(model_config, model_type)

  #func_name <- get_function_from_package(config)
  model <- switch(model_type,
  tpt = escalation::get_three_plus_three(config$n_doses, config$tpt_allow_deesc) %>%
    escalation::stop_at_n(n = config$max_n),
  crm = escalation::get_dfcrm(skeleton = config$prior_ttp, target = config$ttl, scale = config$prior_var) %>% 
    escalation::dont_skip_doses(when_escalating = as.logical(1-config$skip_esc), when_deescalating = as.logical(1-config$skip_deesc)) %>% 
    escalation::stop_when_too_toxic(dose = 1, config$stop_tox_x + config$target, confidence = config$stop_tox_y) %>%
    escalation::stop_when_n_at_dose(n = config$stop_n_mtd, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n),
  boin = escalation::get_boin(
    num_doses = config$n_doses, target = config$ttl, 
    use_stopping_rule = TRUE, p.saf = config$p_saf,
    p.tox = config$p_tox, cutoff.eli = 0.95, extrasafe = FALSE, offset = 0.05) %>%
    escalation::stop_when_n_at_dose(n=config$n_earlystop, dose = "recommended") %>%
    escalation::stop_at_n(n = config$max_n)
  )

return(model)

}

# function for conducting sims and returning output
process_sims <- function(model_config, model_type, sim_data) {
  
  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
#  sim_data <- create_dummy_sims(config$n_doses)

  results <- lapply(seq_along(sim_data$true_dlt_ss), function(i) {
    dlt_ss <- sim_data$true_dlt_ss[[i]]
    sims <- model %>%
      escalation::simulate_trials(next_dose = config$start_dose, num_sims = sim_data$n_sims, true_prob_tox = dlt_ss)
    # find selection probs
    selection <- sims %>% escalation::prob_recommend()
    # find no. treated at dose
    treated <- colSums(sims %>% escalation::n_at_dose())
    treatedpct <- treated / sum(treated) 
    # coerce into long table with dose level and values of metrics
    dlt_df <- dlt_ss %>% data.frame(
        Dose = as.character(1:length(dlt_ss)), Value = .
      ) %>%
      dplyr::mutate(Type = 'True DLT')

    treatedpct_df <- treatedpct%>%
      data.frame(Dose = names(.), Value = .) %>%
      dplyr::mutate(Type = 'Pct. Patients Treated at Dose')
    selection_df <- selection %>%
      data.frame(Dose = names(.), Value = .) %>%
      dplyr::mutate(Type = 'Pct. Dose Selected as MTD')
    # Identify the true MTD from each simulation scenario
    true_mtd_boolean <- find_closest_element(vec = dlt_ss, num = config$ttl) %>%
      data.frame(Dose = as.character(1:length(dlt_ss)), Value = .) %>%
      dplyr::mutate(Type = 'True MTD')

    # metrics
    best_dose <- max(dlt_ss[dlt_ss <= config$ttl])
    best_dose_level <- match(best_dose, dlt_ss)
    # i) accuracy
    dist_accuracy <- escalation::recommended_dose(sims)
    mean_accuracy <- mean(dist_accuracy == best_dose_level)
    # ii) risk of overdosing
    dist_overdose <- escalation::num_tox(sims)
    mean_overdose <- mean(dist_overdose)
    # iii) trial length
    dist_length <- sims %>% escalation::trial_duration()
    mean_length <- mean(dist_length)

    # metrics combined
    metrics <- data.frame(
      "Dose" = NA,      
      "Value" = c(mean_accuracy, mean_overdose, mean_length),
      "Type" = c("Accuracy", "Risk of Overdose", "Trial Duration")
      )

    combined <- dplyr::bind_rows(
        metrics, treatedpct_df, selection_df, dlt_df,
        true_MTD = true_mtd_boolean
      ) %>%
      mutate(Design = model_type, Scenario = paste0("Scenario ", i))
    
  })


return(
  list(
    Configurations = data.frame(unlist(config)), 
    Sim.Configurations = sim_data, 
    Output = do.call(rbind, results)
    )
  )
}

# Create a function that generates histograms and returns them along with the plot
generate_histogram <- function(tidy_results, selected_type, selected_design) {
  
  true_mtd_dose <- tidy_results %>%
    dplyr::filter(Design %in% selected_design) %>%  
    dplyr::filter(Type == "True MTD") %>%
    dplyr::pull(Value)

  plot_data <- tidy_results %>% 
    dplyr::filter(Type == selected_type) %>%
    dplyr::filter(Design %in% selected_design)
  
  if (all(is.na(plot_data$Dose))) {
    # If all Dose values are NA, plot without x and alpha aesthetics
    p <- ggplot(plot_data, aes(x = Type, y = Value, fill = Design)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ Scenario) +
      labs(x = NULL, y = selected_type) +
      theme_minimal() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
    return(p)
  }

  plot_data <- plot_data %>% dplyr::mutate(alpha_value = ifelse(true_mtd_dose == 1, 1, 0.3))
  
  p <- ggplot2::ggplot(plot_data, aes(x = Dose, y = Value, fill = Design, alpha = factor(alpha_value))) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~Scenario, scales = "free", nrow = 1) +
    labs(
      title = paste("Histogram for", selected_type),
      x = "Dose Levels",
      y = selected_type,
      fill = "Design"
    ) +
    scale_alpha_manual(name = "True MTD",
                       values = c(1, 0.3),
                       breaks = c(1, 0.3),
                       labels = c("True", "False"))     
    theme_minimal() +
    theme(legend.position = "top")
  
  return(p)
}






