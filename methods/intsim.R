# read in .rData file
design <- "crm"
data <- readRDS("dummy_data1.RData")
attach(data)
# the data can now be called directly with trial, method, ranking subsets

# setup piping. we want to avoid loading packages with library()
`%>%` <- magrittr::`%>%`

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
    escalation::stop_at_n(n = config$max_n)
  )

  return(model)
}

# function for conducting sims and returning output


process_sims <- function(model_config, model_type, sim_data) {


  config <- process_config(model_config, model_type)
  model <- create_model(model_config, model_type)
  sim_data <- create_dummy_sims(4)


results <- lapply(sim_data$true_dlt_ss, function(dlt_ss) {
  sims <- model %>%
    escalation::simulate_trials(next_dose = config$start_dose, num_sims = sim_data$n_sims, true_prob_tox = dlt_ss)
  # find selection probs
  selection <- sims %>% escalation::prob_recommend()
  # find no. treated at dose
  treated <- colSums(sims %>% escalation::n_at_dose())
  treatedpct <- treated / sum(treated)
  # coerce into table
  selection_df <- data.frame(selection)
  treatedpct_df <- data.frame(treatedpct)
  selection_tab <- rbind(t(selection_df), c(NA, t(treatedpct_df)), c(NA, dlt_ss))
  rownames(selection_tab) <- c("% Dose Selected as MTD", "% Patients Treated at Dose", "True Toxicity Probabilities")

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
  metrics <- data.frame("Accuracy" = mean_accuracy, "Risk of Overdose" = mean_overdose, "Trial Duration" = mean_length)
  
  list(Table = selection_tab, Metrics = metrics)
})


return(
  list(Configurations = rbind(design, data.frame(unlist(config))), Sim.Configurations = sim_data, Output = results)
)
}

generate_histograms_and_plot_list <- function(data, design, sim_data) {
 
  o_sims <- process_sims(data, design, sim_data)
  
  # list of selection tables
  plot_list <- purrr::map(o_sims$Output, function(output) {

    # extract while preserving colnames
    df <- as.data.frame(output$Table, stringsAsFactors = FALSE)
    
    df <- df %>%
      dplyr::filter(row.names(df) == "% Dose Selected as MTD")
    
    # Reshape the data frame into a long format
    df_long <- df %>%
      tidyr::gather(key = "DoseLevel", value = "Percentage")
    
    # put NoDose on the left
    df_long$DoseLevel <- factor(df_long$DoseLevel, levels = unique(df_long$DoseLevel))
    
    # create bar plot
    graph <- ggplot2::ggplot(df_long, ggplot2::aes(x = DoseLevel, y = Percentage, fill = DoseLevel)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(x = "Dose Level", y = "% Selected as MTD", title = "Histogram") +
      ggplot2::theme_bw() 
    
    return(graph)
  })
  

  # Set up the layout
par( mfrow= c(1, length(plot_list)) )

  # Plot each graph
  for (i in 1:length(plot_list)) {
   a[[i]] <- print(plot_list[[i]])
  }
  return(a)
}

b <- generate_histograms_and_plot_list(data, design, sim_data)






