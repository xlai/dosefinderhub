# reads config from a .yaml file
#model_config <- yaml::yaml.load_file("~/dosefinderhub/methods/config.yaml")

# setup piping
`%>%` <- magrittr::`%>%`

# xiaoran's custom function for loading a function from a package
get_function_from_package <- function(full_func_name) {
  func_name <- unlist(strsplit(full_func_name, "::"))
  package_name <- func_name[1]
  function_name <- func_name[2]
  func <- getExportedValue(package_name, function_name)
  return(func)
}

# function that processes config into machine-readable format
process_config <- function(model_config, model_type){
  # assume model_config is a list of three elements - trial, method and ranking
  # subset config that contains only model_type
  # select only the q_vaiable and value (currently X (blank) and answer)
  config_method <- model_config %>% dplyr::filter(method == model_type) %>% dplyr::select(X, answer)
  config_trial <- model_config %>% dplyr::filter(method == "config") %>% dplyr::select(X, answer)
  config_processed <- rbind(config_method, config_trial)
  # change No to F, Yes to T
  config_processed[config_processed == "No"] <- FALSE 
  config_processed[config_processed == "Yes"] <- TRUE

  # convert this dataframe into a list, where each element is q_variable
  config_processed_list <- as.list(config_processed$answer)
  names(config_processed_list) <- as.list(config_processed$X)

return(config_processed_list)
}

test <- process_config(read.csv("methods/dummy_data1.csv"), "tpt")
