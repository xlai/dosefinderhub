#' Simulation Processing Utilities
#' 
#' This file contains shared functions for processing simulation results
#' and managing simulation configurations to eliminate code duplication
#' in the simulation UI.

# Define metric categories
TABLE_METRICS <- c("selection", "treatment", "accuracy", "overdose", "duration")
PLOT_METRICS <- c("selection", "treatment", "overdose", "duration")

# Metric display names
METRIC_NAMES <- list(
  selection = "% times dose was selected as MTD",
  treatment = "% participants treated at dose",
  accuracy = "Accuracy", 
  overdose = "Overdosing",
  duration = "Duration"
)

#' Get Scenario Names
#' 
#' Generate scenario names based on number of scenarios
#' 
#' @param n_scenarios Number of scenarios
#' @return Vector of scenario names
#' @export
get_scenario_names <- function(n_scenarios) {
  paste0("Scenario ", seq_len(n_scenarios))
}

#' Get Method Names
#' 
#' Get available simulation method names
#' 
#' @return Vector of method names
#' @export
get_method_names <- function() {
  c("3+3", "CRM", "BOIN")
}

#' Create Table Data Structure
#' 
#' Creates named list structure for table data (includes accuracy)
#' Structure: tables[["Scenario 1"]][["CRM"]][["accuracy"]] = table_data
#' 
#' @param scenario_names Vector of scenario names
#' @param method_names Vector of method names
#' @return Named list structure for table data
#' @export
create_table_structure <- function(scenario_names, method_names) {
  structure <- list()
  
  for (scenario in scenario_names) {
    structure[[scenario]] <- list()
    for (method in method_names) {
      structure[[scenario]][[method]] <- list()
      for (metric in TABLE_METRICS) {
        structure[[scenario]][[method]][[metric]] <- NULL
      }
    }
  }
  
  return(structure)
}

#' Create Plot Data Structure
#' 
#' Creates named list structure for plot data (excludes accuracy)
#' Structure: plots[["Scenario 1"]][["selection"]][["CRM"]] = plot_data
#' 
#' @param scenario_names Vector of scenario names
#' @param method_names Vector of method names
#' @return Named list structure for plot data
#' @export
create_plot_structure <- function(scenario_names, method_names) {
  structure <- list()
  
  for (scenario in scenario_names) {
    structure[[scenario]] <- list()
    for (metric in PLOT_METRICS) {
      structure[[scenario]][[metric]] <- list()
      for (method in method_names) {
        structure[[scenario]][[metric]][[method]] <- NULL
      }
    }
  }
  
  return(structure)
}

#' Get Simulation Method Configuration
#' 
#' Returns configuration for all supported simulation methods including
#' function references, parameter extractors, and display names.
#' 
#' @return List of method configurations
#' @export
get_simulation_methods <- function() {
  list(
    "3+3" = list(
      name = "3+3",
      sim_function = sim_tpt,
      title_prefix = "3+3 Simulation for ",
      param_extractor = function(shared, n_sims, true_dlts, scenario_index) {
        list(
          n_doses = shared$n_dosess(),
          ttl = shared$ttl(), 
          max_n = shared$max_size(),
          start_dose = shared$start_dose(),
          n_sims = n_sims,
          true_dlt_ss = unlist(true_dlts[scenario_index, ]),
          current_seed = 12345
        )
      }
    ),
    "CRM" = list(
      name = "CRM",
      sim_function = sim_crm,
      title_prefix = "CRM Simulation for ",
      param_extractor = function(shared, n_sims, true_dlts, scenario_index) {
        list(
          n_doses = shared$n_dosess(),
          ttl = shared$ttl(),
          max_n = shared$max_size(),
          start_dose = shared$start_dose(),
          n_sims = n_sims,
          true_dlt_ss = unlist(true_dlts[scenario_index, ]),
          skeleton = shared$skeleton_crm(),
          prior_var = shared$prior_var_crm(),
          skip_esc = shared$skip_esc_crm(),
          skip_deesc = shared$skip_deesc_crm(),
          stop_tox_x = shared$stop_tox_x_crm(),
          stop_tox_y = shared$stop_tox_y_crm(),
          stop_n_mtd = shared$stop_n_mtd_crm()
        )
      }
    ),
    "BOIN" = list(
      name = "BOIN",
      sim_function = sim_boin,
      title_prefix = "BOIN Simulation for ",
      param_extractor = function(shared, n_sims, true_dlts, scenario_index) {
        list(
          n_doses = shared$n_dosess(),
          ttl = shared$ttl(),
          max_n = shared$max_size(),
          start_dose = shared$start_dose(),
          n_sims = n_sims,
          true_dlt_ss = unlist(true_dlts[scenario_index, ]),
          cohort_size = shared$boin_cohorts(),
          stop_n_mtd = shared$stop_n_mtd_boin(),
          p_tox = shared$phi_2,
          p_saf = shared$phi_1,
          use_stopping_rule = TRUE,
          n_cohorts = 10
        )
      }
    )
  )
}

#' Process Simulation Results
#' 
#' Standardized processing of simulation results for any method.
#' Eliminates the duplicated processing logic found in the original code.
#' 
#' @param sim_result Raw simulation result from simulation function
#' @param method_name Name of the simulation method
#' @return List containing processed tables, means, medians, and plot data
#' @export
process_simulation_results <- function(sim_result, method_name) {
  if (is.null(sim_result)) {
    return(create_null_simulation_results())
  }
  
  # Remove unwanted columns (indices 4 and 6 as in original)
  modified_tab <- sim_result[-c(4, 6)]
  
  # Format metrics as data frames with proper row names
  modified_tab <- format_metrics_dataframes(modified_tab)
  
  # Extract mean and median values
  mean_values <- extract_mean_values(sim_result)
  median_values <- extract_median_values(sim_result)
  
  return(list(
    modified_tab = modified_tab,
    mean_values = mean_values,
    median_values = median_values,
    raw_result = sim_result  # Include raw result for plotting later
  ))
}

#' Create Null Simulation Results
#' 
#' Creates standardized null results structure when simulation is not run
#' 
#' @return List of NULL values matching expected structure
create_null_simulation_results <- function() {
  list(
    modified_tab = NULL,
    mean_values = list(
      mean_accuracy = NULL,
      mean_overdose = NULL,
      mean_length = NULL
    ),
    median_values = list(
      median_overdose = NULL,
      median_length = NULL
    ),
    raw_result = NULL
  )
}

#' Format Metrics Data Frames
#' 
#' Converts metrics to properly formatted data frames with row names
#' 
#' @param modified_tab List containing simulation metrics
#' @return Formatted list with proper data frame structure
format_metrics_dataframes <- function(modified_tab) {
  # Format accuracy metric
  if (!is.null(modified_tab$mean_accuracy)) {
    modified_tab$mean_accuracy <- as.data.frame(
      modified_tab$mean_accuracy, 
      row.names = "Mean Accuracy"
    )
    colnames(modified_tab$mean_accuracy) <- ""
  }
  
  # Format overdose metric  
  if (!is.null(modified_tab$mean_overdose)) {
    modified_tab$mean_overdose <- as.data.frame(
      modified_tab$mean_overdose,
      row.names = "Mean Overdose"
    )
    colnames(modified_tab$mean_overdose) <- ""
  }
  
  # Format trial length metric
  if (!is.null(modified_tab$mean_length)) {
    modified_tab$mean_length <- as.data.frame(
      modified_tab$mean_length,
      row.names = "Mean Trial Length"
    )
    colnames(modified_tab$mean_length) <- ""
  }
  
  return(modified_tab)
}

#' Extract Mean Values
#' 
#' Extracts mean values from simulation results
#' 
#' @param sim_result Raw simulation result
#' @return List of mean values
extract_mean_values <- function(sim_result) {
  list(
    mean_accuracy = sim_result$mean_accuracy,
    mean_overdose = sim_result$mean_overdose,
    mean_length = sim_result$mean_length
  )
}

#' Extract Median Values
#' 
#' Extracts median values from simulation distributions
#' 
#' @param sim_result Raw simulation result
#' @return List of median values
extract_median_values <- function(sim_result) {
  list(
    median_overdose = median(sim_result$dist_overdose),
    median_length = median(sim_result$dist_length)
  )
}

#' Run Simulation for Method
#' 
#' Generic function to run simulation for any configured method
#' 
#' @param method_name Name of simulation method ("3+3", "CRM", "BOIN")
#' @param shared Shared reactive values object
#' @param n_sims Number of simulations to run
#' @param true_dlts True DLT probabilities matrix
#' @param scenario_index Index of scenario to simulate
#' @return Processed simulation results or NULL if method not selected
#' @export
run_simulation_for_method <- function(method_name, shared, n_sims, true_dlts, scenario_index, selected_methods) {
  if (!(method_name %in% selected_methods)) {
    return(create_null_simulation_results())
  }
  
  methods_config <- get_simulation_methods()
  method_config <- methods_config[[method_name]]
  
  if (is.null(method_config)) {
    stop(paste("Unknown simulation method:", method_name))
  }
  
  # Extract parameters using method-specific extractor
  params <- method_config$param_extractor(shared, n_sims, true_dlts, scenario_index)
  
  # Run simulation
  sim_result <- do.call(method_config$sim_function, params)
  
  # Process results using standardized processor
  return(process_simulation_results(sim_result, method_name))
}

#' Build Simulation Titles
#' 
#' Creates standardized titles for simulation results
#' 
#' @param methods Vector of method names
#' @param scenarios Vector of scenario names  
#' @param metrics Vector of metric names
#' @return Vector of formatted titles
#' @export
build_simulation_titles <- function(methods, scenarios, metrics) {
  method_config <- get_simulation_methods()
  
  # Get title prefixes for each method
  title_prefixes <- sapply(methods, function(m) {
    if (m %in% names(method_config)) {
      method_config[[m]]$title_prefix
    } else {
      paste(m, "Simulation for ")
    }
  })
  
  # Metric suffixes
  metric_suffixes <- c(
    " - % Times Dose Was Selected as MTD",
    " - % Treated at Each Dose", 
    " - Mean Accuracy",
    " - Mean Overdose",
    " - Mean Trial Length"
  )
  
  # Build titles for all combinations
  titles <- c()
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[i]
    for (j in seq_along(methods)) {
      method <- methods[j]
      prefix <- title_prefixes[j]
      for (k in seq_along(metrics)) {
        if (metrics[k]) {  # Only include selected metrics
          title <- paste0(prefix, scenario, metric_suffixes[k])
          titles <- c(titles, title)
        }
      }
    }
  }
  
  return(titles)
}

#' Process Multiple Simulations
#' 
#' Runs and processes simulations for multiple methods and scenarios using named structures
#' 
#' @param selected_methods Vector of selected method names
#' @param selected_scenarios Logical vector of selected scenarios
#' @param scenarios Vector of scenario names
#' @param shared Shared reactive values
#' @param n_sims Number of simulations
#' @param true_dlts True DLT probabilities matrix
#' @param selected_metric Logical vector of selected metrics
#' @return List containing table_data and plot_data with named structures
#' @export
process_multiple_simulations <- function(selected_methods, selected_scenarios, scenarios, 
                                       shared, n_sims, true_dlts, selected_metric) {
  
  # Get actual names
  all_scenarios <- get_scenario_names(length(scenarios))
  scenario_names <- all_scenarios[selected_scenarios]
  method_names <- selected_methods  # Already filtered method names
  
  # Create named structures
  table_data <- create_table_structure(scenario_names, method_names)
  plot_data <- create_plot_structure(scenario_names, method_names)
  
  # Create median storage with named structure
  median_data <- list()
  for (scenario_name in scenario_names) {
    median_data[[scenario_name]] <- list(
      overdose = list(),
      duration = list()
    )
  }
  
  # Process each scenario
  scenario_indices <- which(selected_scenarios)
  for (i in seq_along(scenario_names)) {
    scenario_name <- scenario_names[i]
    scenario_idx <- scenario_indices[i]
    
    # Process each method
    for (method_name in method_names) {
      result <- run_simulation_for_method(method_name, shared, n_sims, true_dlts, scenario_idx, method_names)
      
      if (!is.null(result$modified_tab)) {
        # Store table data (includes accuracy)
        table_data[[scenario_name]][[method_name]][["selection"]] <- result$modified_tab$selection_tab
        table_data[[scenario_name]][[method_name]][["treatment"]] <- result$modified_tab$treatment_tab  
        table_data[[scenario_name]][[method_name]][["accuracy"]] <- result$modified_tab$mean_accuracy
        table_data[[scenario_name]][[method_name]][["overdose"]] <- result$modified_tab$mean_overdose
        table_data[[scenario_name]][[method_name]][["duration"]] <- result$modified_tab$mean_length
      }
      
      if (!is.null(result$raw_result)) {
        # Store plot data (no accuracy) and median values
        plot_result <- data_for_plotting(result$raw_result, shared$ttl())
        plot_data[[scenario_name]][["selection"]][[method_name]] <- plot_result$data_selection
        plot_data[[scenario_name]][["treatment"]][[method_name]] <- plot_result$data_treatment
        plot_data[[scenario_name]][["overdose"]][[method_name]] <- plot_result$overdose  
        plot_data[[scenario_name]][["duration"]][[method_name]] <- plot_result$length
        
        # Store median values
        median_data[[scenario_name]]$overdose[[method_name]] <- result$median_values$median_overdose
        median_data[[scenario_name]]$duration[[method_name]] <- result$median_values$median_length
      }
    }
  }
  
  return(list(
    table_data = table_data,
    plot_data = plot_data,
    median_data = median_data
  ))
}

#' Generate Simulation Tables
#' 
#' Creates table lists from named table data structure
#' 
#' @param table_data Named table data structure
#' @param selected_metric Logical vector of selected metrics
#' @return List with tables and titles
#' @export
generate_simulation_tables <- function(table_data, selected_metric) {
  tables <- list()
  titles <- list()
  
  # selected_metric is logical vector: [selection, treatment, accuracy, overdose, duration]
  selected_metrics <- TABLE_METRICS[selected_metric]
  
  for (scenario_name in names(table_data)) {
    for (method_name in names(table_data[[scenario_name]])) {
      for (metric in selected_metrics) {
        
        table <- table_data[[scenario_name]][[method_name]][[metric]]
        if (is.null(table)) next
        
        title <- sprintf("%s Simulation for %s - %s", method_name, scenario_name, METRIC_NAMES[[metric]])
        tables <- c(tables, list(table))
        titles <- c(titles, title)
      }
    }
  }
  
  return(list(
    tables = tables,
    titles = titles
  ))
}

#' Extract Scenario Results
#' 
#' Helper function to extract and organize results for a single scenario
#' 
#' @param method_results List of results from different methods
#' @param scenario_name Name of the current scenario
#' @param selected_metric Logical vector of selected metrics
#' @param shared Shared reactive values for TTL
#' @return Organized results for the scenario
extract_scenario_results <- function(method_results, scenario_name, selected_metric, shared) {
  tables <- list()
  titles <- list()
  median_overdose <- c()
  median_length <- c()
  
  # Initialize plot data structure: 4 metrics, each containing data for all methods
  plot_data_by_metric <- vector("list", 4)
  
  # Collect plot data for each method
  method_plot_data <- list()
  for (method_name in names(method_results)) {
    result <- method_results[[method_name]]
    
    if (!is.null(result$modified_tab)) {
      # Filter tables by selected metrics
      selected_tables <- result$modified_tab[which(selected_metric == TRUE)]
      tables <- c(tables, selected_tables)
      
      # Build titles for selected metrics only
      method_config <- get_simulation_methods()[[method_name]]
      all_titles <- paste0(method_config$title_prefix, scenario_name,
                          c(" - % Times Dose Was Selected as MTD",
                            " - % Treated at Each Dose", 
                            " - Mean Accuracy",
                            " - Mean Overdose", 
                            " - Mean Trial Length"))
      selected_titles <- all_titles[which(selected_metric == TRUE)]
      titles <- c(titles, selected_titles)
    }
    
    # Create plot data if raw result exists
    if (!is.null(result$raw_result)) {
      plot_data <- data_for_plotting(result$raw_result, shared$ttl())
    } else {
      plot_data <- rep(list(NULL), 5)
    }
    method_plot_data[[method_name]] <- plot_data
    
    median_overdose <- c(median_overdose, result$median_values$median_overdose)
    median_length <- c(median_length, result$median_values$median_length)
  }
  
  # Reorganize plot data to match original structure: plot_data[[metric]] = list of method data
  for (k in 1:4) {  # 4 metrics (excluding accuracy)
    metric_data <- list()
    for (method_name in names(method_results)) {
      if (!is.null(method_plot_data[[method_name]])) {
        metric_data <- c(metric_data, list(method_plot_data[[method_name]][[k]]))
      } else {
        metric_data <- c(metric_data, list(NULL))
      }
    }
    plot_data_by_metric[[k]] <- metric_data
  }
  
  return(list(
    tables = tables,
    titles = titles,
    plots = plot_data_by_metric,  # Now organized by metric, then by method
    median_overdose = median_overdose,
    median_length = median_length
  ))
}