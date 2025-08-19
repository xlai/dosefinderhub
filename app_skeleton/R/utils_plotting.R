#' Plotting Utilities for Simulation Results
#' 
#' This file contains shared functions for generating plots from simulation
#' results to eliminate code duplication in the simulation UI.
#' 
#' @import rlang

#' Generate Simulation Plots
#' 
#' Unified function to generate plots for different view types
#' 
#' @param view_type One of "Individually", "Comparatively by Design", "Comparatively by Scenario"
#' @param plot_data List of plot data for each scenario and method
#' @param selected_metric Logical vector of selected metrics
#' @param selected_scenarios Logical vector of selected scenarios
#' @param selected_models Logical vector of selected models
#' @param scenarios Vector of scenario names
#' @param models Vector of model names
#' @param median_overdose List of median overdose values
#' @param median_length List of median trial length values
#' @param ns Namespace function for generating input IDs
#' @return List containing filtered graphs and UI elements
#' @export
generate_simulation_plots <- function(view_type, plot_data, selected_metric, selected_scenarios, 
                                    selected_models, scenarios, models, median_overdose, 
                                    median_length, ns) {
  
  n_scen <- sum(selected_scenarios)
  n_models <- sum(selected_models)
  updated_scenarios <- scenarios[selected_scenarios]
  updated_models <- models[selected_models]
  
  if (view_type == "Individually") {
    return(generate_individual_plots(plot_data, selected_metric, selected_scenarios, 
                                   selected_models, updated_scenarios, updated_models, 
                                   median_overdose, median_length, ns, n_scen, n_models))
    
  } else if (view_type == "Comparatively by Design") {
    return(generate_comparative_by_design_plots(plot_data, selected_metric, selected_scenarios,
                                              selected_models, updated_scenarios, 
                                              median_overdose, median_length, ns, n_scen))
    
  } else if (view_type == "Comparatively by Scenario") {
    return(generate_comparative_by_scenario_plots(plot_data, selected_metric, selected_scenarios,
                                                selected_models, updated_models, 
                                                median_overdose, median_length, ns, n_models))
  }
  
  return(list(filtered_graphs = NULL, ui_element = NULL))
}

#' Generate Individual Plots
#' 
#' Creates individual plots for each method and scenario combination
#' 
#' @param plot_data List of plot data
#' @param selected_metric Logical vector of selected metrics
#' @param selected_scenarios Logical vector of selected scenarios  
#' @param selected_models Logical vector of selected models
#' @param updated_scenarios Vector of selected scenario names
#' @param updated_models Vector of selected model names
#' @param median_overdose List of median overdose values
#' @param median_length List of median trial length values
#' @param ns Namespace function
#' @param n_scen Number of selected scenarios
#' @param n_models Number of selected models
#' @return List with filtered graphs and UI element
generate_individual_plots <- function(plot_data, selected_metric, selected_scenarios, selected_models,
                                    updated_scenarios, updated_models, median_overdose, 
                                    median_length, ns, n_scen, n_models) {
  
  graphs <- vector("list", 4 * n_scen * n_models)
  metric_no_accuracy <- selected_metric[-3]  # Remove accuracy metric
  
  for (j in 1:n_scen) {
    data_scen <- plot_data[[j]]  # This is the plot data for scenario j
    
    # data_scen should be organized as data_scen[[metric]][[method]]
    # We need to reorganize it to data_mod[[method]][[metric]]
    
    for (i in 1:n_models) {
      for (k in 1:4) {
        if (!metric_no_accuracy[k]) next
        
        # Access data directly without plot_by_scenario for now
        # data_scen[[k]] should contain list of method data for metric k
        if (!is.null(data_scen[[k]]) && length(data_scen[[k]]) >= i && !is.null(data_scen[[k]][[i]])) {
          met <- as.data.frame(data_scen[[k]][[i]])
          
          mo <- median_overdose[[j]][i]
          ml <- median_length[[j]][i]
          index <- 4 * n_models * (j - 1) + 4 * (i - 1) + k
          
          graph <- create_individual_plot_for_metric(met, k, updated_models[[i]], 
                                                   updated_scenarios[[j]], mo, ml)
          if (!is.null(graph)) {
            graphs[[index]] <- graph
          }
        }
      }
    }
  }
  
  filtered_graphs <- Filter(Negate(is.null), graphs)
  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)
  
  ui_element <- renderUI({
    selectInput(ns("ind_graph"), "Select a plot to view",
                choices = names(filtered_graphs), 
                selected = names(filtered_graphs)[1], 
                multiple = FALSE, width = "100%")
  })
  
  return(list(filtered_graphs = filtered_graphs, ui_element = ui_element))
}

#' Generate Comparative by Design Plots
#' 
#' Creates comparative plots grouped by design/method
#' 
#' @param plot_data List of plot data
#' @param selected_metric Logical vector of selected metrics
#' @param selected_scenarios Logical vector of selected scenarios
#' @param selected_models Logical vector of selected models
#' @param updated_scenarios Vector of selected scenario names
#' @param median_overdose List of median overdose values
#' @param median_length List of median trial length values
#' @param ns Namespace function
#' @param n_scen Number of selected scenarios
#' @return List with filtered graphs and UI element
generate_comparative_by_design_plots <- function(plot_data, selected_metric, selected_scenarios,
                                               selected_models, updated_scenarios, median_overdose, 
                                               median_length, ns, n_scen) {
  
  graphs <- vector("list", 4 * n_scen)
  metric_no_accuracy <- selected_metric[-3]
  
  for (j in 1:n_scen) {
    data <- plot_data[[j]]
    mo <- median_overdose[[j]]
    ml <- median_length[[j]]
    
    for (k in 1:4) {
      if (!metric_no_accuracy[k]) next
      
      met <- data[[k]]
      if (is.null(met)) next
      
      index <- 4 * (j - 1) + k
      graph <- create_comparative_plot_for_metric(met, k, updated_scenarios[[j]], mo, ml, 
                                                model_picked = 1, selected_models, 
                                                selected_scenarios)
      if (!is.null(graph)) {
        graphs[[index]] <- graph
      }
    }
  }
  
  filtered_graphs <- Filter(Negate(is.null), graphs)
  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)
  
  ui_element <- renderUI({
    selectInput(ns("m_graph"), "Select a plot to view",
                choices = names(filtered_graphs),
                selected = names(filtered_graphs)[1],
                multiple = FALSE, width = "100%")
  })
  
  return(list(filtered_graphs = filtered_graphs, ui_element = ui_element))
}

#' Generate Comparative by Scenario Plots
#' 
#' Creates comparative plots grouped by scenario
#' 
#' @param plot_data List of plot data
#' @param selected_metric Logical vector of selected metrics
#' @param selected_scenarios Logical vector of selected scenarios
#' @param selected_models Logical vector of selected models
#' @param updated_models Vector of selected model names
#' @param median_overdose List of median overdose values
#' @param median_length List of median trial length values
#' @param ns Namespace function
#' @param n_models Number of selected models
#' @return List with filtered graphs and UI element
generate_comparative_by_scenario_plots <- function(plot_data, selected_metric, selected_scenarios,
                                                 selected_models, updated_models, median_overdose, 
                                                 median_length, ns, n_models) {
  
  # Reorganize data by scenario
  plot_tpt_full <- vector("list", length = 3)
  plot_crm_full <- vector("list", length = 3)
  plot_boin_full <- vector("list", length = 3)
  
  y <- 1
  z <- 1
  while (y < 4) {
    if (selected_scenarios[y]) {
      plot_tpt_full[[y]] <- plot_data[[z]][[1]]  # TPT plots
      plot_crm_full[[y]] <- plot_data[[z]][[2]]  # CRM plots
      plot_boin_full[[y]] <- plot_data[[z]][[3]]  # BOIN plots
      y <- y + 1
      z <- z + 1
    } else {
      plot_tpt_full[[y]] <- rep(list(NULL), 5)
      plot_crm_full[[y]] <- rep(list(NULL), 5)
      plot_boin_full[[y]] <- rep(list(NULL), 5)
      y <- y + 1
    }
  }
  
  # Reorganize by model
  tpt_by_scenario <- plot_by_scenario(plot_tpt_full, 5, 3, 4)
  crm_by_scenario <- plot_by_scenario(plot_crm_full, 5, 3, 4)
  boin_by_scenario <- plot_by_scenario(plot_boin_full, 5, 3, 4)
  
  plot_list_by_scenario <- list(tpt_by_scenario, crm_by_scenario, boin_by_scenario)
  
  median_ov_scen <- median_for_scen(median_overdose)
  median_len_scen <- median_for_scen(median_length)
  
  graphs <- vector("list", 4 * n_models)
  metric_no_accuracy <- selected_metric[-3]
  used_plots <- plot_list_by_scenario[selected_models]
  
  for (j in 1:n_models) {
    data <- used_plots[[j]]
    mo <- median_ov_scen[[j]]
    ml <- median_len_scen[[j]]
    
    for (k in 1:4) {
      if (!metric_no_accuracy[k]) next
      
      met <- data[[k]]
      if (is.null(met)) next
      
      index <- 4 * (j - 1) + k
      graph <- create_comparative_plot_for_metric(met, k, updated_models[j], mo, ml,
                                                model_picked = 2, selected_models, 
                                                selected_scenarios)
      if (!is.null(graph)) {
        graphs[[index]] <- graph
      }
    }
  }
  
  filtered_graphs <- Filter(Negate(is.null), graphs)
  names(filtered_graphs) <- sapply(filtered_graphs, function(x) x$labels$title)
  
  ui_element <- renderUI({
    selectInput(ns("s_graph"), "Select a plot to view",
                choices = names(filtered_graphs),
                selected = names(filtered_graphs)[1],
                multiple = FALSE, width = "100%")
  })
  
  return(list(filtered_graphs = filtered_graphs, ui_element = ui_element))
}

#' Create Individual Plot for Metric
#' 
#' Creates an individual plot for a specific metric
#' 
#' @param met Data frame containing metric data
#' @param metric_index Index of the metric (1-4)
#' @param model_name Name of the simulation method
#' @param scenario_name Name of the scenario
#' @param median_overdose Median overdose value
#' @param median_length Median trial length value
#' @return ggplot object or NULL
create_individual_plot_for_metric <- function(met, metric_index, model_name, scenario_name, 
                                            median_overdose, median_length) {
  
  plot_config <- get_individual_plot_config(metric_index)
  
  if (plot_config$type == "bar") {
    if (!is.null(met[[plot_config$data_col]])) {
      title <- sprintf(plot_config$title_template, model_name, scenario_name)
      if (metric_index == 1) {  # MTD selection
        return(plot_bar_ind(met, Dose, selection, title = title, y_title = plot_config$y_title, col = "blue"))
      } else if (metric_index == 2) {  # Treatment at dose
        return(plot_bar_ind(met, Dose_Level, treatment, title = title, y_title = plot_config$y_title, col = "blue"))
      }
    }
  } else if (plot_config$type == "distribution") {
    if (!is.null(met[[plot_config$data_col]])) {
      title <- sprintf(plot_config$title_template, model_name, scenario_name)
      median_val <- if (metric_index == 3) median_overdose else median_length
      if (metric_index == 3) {  # Overdose distribution
        return(plot_dist_ind(met, overdose, median_val, title = title, x_title = plot_config$x_title, col = "blue"))
      } else if (metric_index == 4) {  # Trial length distribution
        return(plot_dist_ind(met, length, median_val, title = title, x_title = plot_config$x_title, col = "blue"))
      }
    }
  }
  
  return(NULL)
}

#' Create Comparative Plot for Metric
#' 
#' Creates a comparative plot for a specific metric
#' 
#' @param met List of data for different methods/scenarios
#' @param metric_index Index of the metric (1-4)
#' @param group_name Name of the grouping (scenario or method)
#' @param median_overdose Vector of median overdose values
#' @param median_length Vector of median trial length values
#' @param model_picked Integer indicating grouping type
#' @param selected_models Logical vector of selected models
#' @param selected_scenarios Logical vector of selected scenarios
#' @return ggplot object or NULL
create_comparative_plot_for_metric <- function(met, metric_index, group_name, median_overdose, 
                                             median_length, model_picked, selected_models, 
                                             selected_scenarios) {
  
  plot_config <- get_comparative_plot_config(metric_index)
  
  # Check if any of the methods have this metric
  has_data <- any(sapply(met, function(x) !is.null(x[[plot_config$data_col]])))
  if (!has_data) return(NULL)
  
  title <- sprintf(plot_config$title_template, group_name)
  
  if (plot_config$type == "bar") {
    if (metric_index == 1) {  # MTD selection
      return(plot_bar(met, Dose, selection, title = title, y_title = plot_config$y_title, col = "blue",
                     model_picked = model_picked, models = selected_models, scenarios = selected_scenarios))
    } else if (metric_index == 2) {  # Treatment at dose  
      return(plot_bar(met, Dose_Level, treatment, title = title, y_title = plot_config$y_title, col = "blue",
                     model_picked = model_picked, models = selected_models, scenarios = selected_scenarios))
    }
  } else if (plot_config$type == "distribution") {
    median_vals <- if (metric_index == 3) median_overdose else median_length
    if (metric_index == 3) {  # Overdose distribution
      return(plot_dist(met, overdose, median_vals, title = title, x_title = plot_config$x_title, col = "blue",
                      model_picked = model_picked, models = selected_models, scenarios = selected_scenarios))
    } else if (metric_index == 4) {  # Trial length distribution
      return(plot_dist(met, length, median_vals, title = title, x_title = plot_config$x_title, col = "blue",
                      model_picked = model_picked, models = selected_models, scenarios = selected_scenarios))
    }
  }
  
  return(NULL)
}

#' Get Individual Plot Configuration
#' 
#' Returns configuration for individual plots based on metric index
#' 
#' @param metric_index Index of the metric (1-4)
#' @return List containing plot configuration
get_individual_plot_config <- function(metric_index) {
  configs <- list(
    list(  # MTD selection
      type = "bar",
      data_col = "selection",
      x_col = "Dose", 
      y_title = "% Times Dose Was Selected as MTD",
      title_template = "%% Times Dose Was Selected as MTD for %s %s"
    ),
    list(  # Treatment at dose
      type = "bar", 
      data_col = "treatment",
      x_col = "Dose_Level",
      y_title = "% Treated at Dose", 
      title_template = "%% Treated at Dose for %s %s"
    ),
    list(  # Overdose distribution  
      type = "distribution",
      data_col = "overdose",
      x_title = "Overdose",
      title_template = "Distribution of Overdoses for %s %s"
    ),
    list(  # Trial length distribution
      type = "distribution", 
      data_col = "length",
      x_title = "Trial Duration",
      title_template = "Distribution of Trial Duration for %s %s"
    )
  )
  
  return(configs[[metric_index]])
}

#' Get Comparative Plot Configuration
#' 
#' Returns configuration for comparative plots based on metric index
#' 
#' @param metric_index Index of the metric (1-4)
#' @return List containing plot configuration  
get_comparative_plot_config <- function(metric_index) {
  configs <- list(
    list(  # MTD selection
      type = "bar",
      data_col = "selection", 
      x_col = "Dose",
      y_title = "% Times Dose Was Selected as MTD",
      title_template = "%% Times Dose Was Selected as MTD for %s"
    ),
    list(  # Treatment at dose
      type = "bar",
      data_col = "treatment",
      x_col = "Dose_Level", 
      y_title = "% Treated at Dose",
      title_template = "%% Treated at Dose for %s"
    ),
    list(  # Overdose distribution
      type = "distribution",
      data_col = "overdose",
      x_title = "Overdose", 
      title_template = "Distribution of Overdoses for %s"
    ),
    list(  # Trial length distribution
      type = "distribution",
      data_col = "length",
      x_title = "Trial Duration",
      title_template = "Distribution of Trial Duration for %s" 
    )
  )
  
  return(configs[[metric_index]])
}