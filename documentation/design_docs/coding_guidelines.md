# R Shiny Coding Conventions

This document outlines the coding conventions for R Shiny applications to be followed by developers working on the DoseFinderHub project. Adhering to these guidelines ensures consistency, readability, and maintainability of the code.

## Package Management

### renv for Dependency Management
- **Always use renv** for package management to ensure reproducible environments
- Initialize renv at the start of your project: `renv::init()`
- Take snapshots after installing new packages: `renv::snapshot()`
- Commit both `renv.lock` and `renv/activate.R` to version control

```R
# Initialize renv (run once per project)
renv::init()

# Install packages normally
install.packages("shiny")
install.packages("DT")

# Take snapshot to lock versions
renv::snapshot()
```

### Package Loading
- Use `library()` statements at the top of your scripts
- Avoid `require()` in production code
- When possible, use explicit package calls with `::` for clarity

```R
# At top of app.R or global.R
library(shiny)
library(DT)

# Or use explicit calls
DT::datatable(data)
```

## Shiny App Structure

### Recommended App Organization
Follow the [official Shiny app formats](https://shiny.posit.co/r/articles/build/app-formats/) for structuring your application:

```
project/
├── renv.lock
├── renv/
├── app.R                 # Single-file app, OR
├── ui.R + server.R       # Multi-file app
├── global.R              # Use sparingly
├── R/                    # Custom functions
│   ├── data_processing.R
│   ├── plotting_utils.R
│   └── validation.R
├── data/
├── www/                  # Static assets
└── tests/
```

### Function Organization
- **Never use `source()`** to load functions
- Place all custom functions in the `R/` directory
- Functions in `R/` are automatically loaded by Shiny
- Use `global.R` only for:
  - Global variables that need to be shared between ui and server
  - One-time expensive computations
  - Database connections

```R
# ❌ Don't do this
source("helper_functions.R")

# ✅ Do this instead - place in R/data_processing.R
process_dose_data <- function(raw_data) {
  # Function implementation
}
```

### global.R Usage
Use `global.R` sparingly and only for truly global elements:

```R
# global.R - use minimally
library(shiny)
library(DT)

# Global constants
APP_VERSION <- "1.0.0"

# One-time data loading
reference_data <- readRDS("data/reference.rds")

# Database connection pool (if needed)
# pool <- pool::dbPool(...)
```

## Naming Conventions

### Variables
- Use lowercase letters and separate words with underscores
- Make variable names descriptive and unambiguous

```R
dose_response_data <- read.csv("data.csv")
patient_count <- nrow(dose_response_data)
```

### Functions
- Function names should use lowercase letters and underscores
- Include a verb in the function name to describe what it does
- Place custom functions in appropriate R/ files

```R
# In R/dose_calculations.R
calculate_effective_dose <- function(response_data) {
  # Function implementation
}

validate_dose_inputs <- function(dose_values) {
  # Function implementation
}
```

## Code Style

### Indentation and Spacing
- Use two spaces for indentation, no tabs
- Place spaces around operators and after commas

```R
calculate_dose_metrics <- function(dose_data, patient_id) {
  filtered_data <- dose_data %>%
    filter(id == patient_id) %>%
    mutate(adjusted_dose = dose * 1.2)
  
  return(filtered_data)
}
```

### Comments
- Use comments to explain the "why" and "how" rather than the "what"
- Start comments with a single space

```R
# Calculate dose adjustment based on patient weight
adjusted_dose <- base_dose * (patient_weight / 70)
```

## Functions

### Function Structure
- Break long function calls into multiple lines, indenting appropriately
- Provide default values for function arguments when appropriate

```R
create_dose_plot <- function(data,
                            dose_column = "dose",
                            response_column = "response",
                            title = "Dose-Response Relationship") {
  # Function body
}
```

### Libraries
Explicitly list the package when calling functions from libraries for clarity:

```R
# Preferred approach
ggplot2::ggplot(data, ggplot2::aes(x = dose, y = response)) +
  ggplot2::geom_point()

# Alternative if library is loaded
ggplot(data, aes(x = dose, y = response)) +
  geom_point()
```

## Documentation

### Function Documentation
Document functions with Roxygen comments, including `@param` for parameters and `@return` for return values:

```R
#' Calculate Effective Dose
#'
#' Calculates the effective dose based on response data and patient characteristics.
#'
#' @param response_data A data frame containing dose-response data
#' @param patient_weight Numeric value representing patient weight in kg
#' @param adjustment_factor Numeric adjustment factor (default: 1.0)
#' @return A numeric value representing the effective dose
#' @export
calculate_effective_dose <- function(response_data, 
                                   patient_weight,
                                   adjustment_factor = 1.0) {
  # Function implementation
}
```

## Version Control

### Commit Practices
- Break work into logical commits with clear and descriptive commit messages
- Include issue numbers in commit messages when relevant
- Always commit `renv.lock` when package dependencies change

```bash
git add renv.lock
git commit -m "Add DT package for interactive tables (#123)"
```

### renv Files
- Commit `renv.lock` and `renv/activate.R` to version control
- Add `renv/library/` to `.gitignore`

## Testing

### Testing Structure
- Place tests in the `tests/` directory
- Use `testthat` for unit testing functions in `R/`
- Test both UI components and server logic separately

```R
# tests/testthat/test-dose_calculations.R
test_that("calculate_effective_dose works correctly", {
  test_data <- data.frame(dose = c(1, 2, 3), response = c(10, 20, 30))
  result <- calculate_effective_dose(test_data, patient_weight = 70)
  expect_numeric(result)
  expect_gt(result, 0)
})
```

## Best Practices Summary

1. **Use renv** for all package management
2. **Organize functions** in the `R/` directory, never use `source()`
3. **Minimize global.R** usage - only for truly global elements
4. **Follow Shiny app structure** guidelines from Posit
5. **Document functions** with Roxygen comments
6. **Use explicit package calls** when possible
7. **Test your functions** in the `tests/` directory

Remember, the goal is to write code that is easy to read, understand, and maintain for all team members while following Shiny best practices for scalable applications.