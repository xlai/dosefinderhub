# R Coding Conventions

This document outlines the coding conventions for R to be followed by the developers working on the DoseFinderHub project. Adhering to these guidelines ensures consistency, readability, and maintainability of the code.

## Naming Conventions

### Variables

- Use lowercase letters and separate words with underscores.
- Make variable names descriptive and unambiguous.

```R
correct_name <- "value"
wrongname <- "value"
```

### Functions

- Function names should also use lowercase letters and underscores.
- Include a verb in the function name to describe what it does.

```R
calculate_mean <- function(x) { mean(x) }
```

### Indentation and Spacing

- Use two spaces for indentation, no tabs.
- Place spaces around operators and after commas.

```R
add_numbers <- function(a, b) {
  result <- a + b
  return(result)
}
```

## Comments

- Use comments to explain the "why" and "how" rather than the "what."
- Start comments with a single space.

```R
 # Correct way to comment
```

## Functions

- Break long function calls into multiple lines, indenting appropriately.
- Provide default values for function arguments when appropriate.

```R
long_function_name <- function(arg1 = "value1",
                               arg2 = "value2") {
  # Function body
}
```

## Libraries

If you can, try to explicitly list the package when calling a function from a library.

```R
ggplot2::ggplot()
```

## Version Control

 Break the work into logical commits with clear and descriptive commit messages.
 Include issue numbers in commit messages when relevant.

## Documentation

Document functions with Roxygen comments, including `@param` for parameters and `@return` for return values.

```R
#' Calculate Mean
#'
#' @param x A numeric vector.
#' @return The mean of the vector.
calculate_mean <- function(x) { mean(x) }
```

Please adhere to these guidelines and consult this document if you have any questions or need clarification.

Remember, the goal is to write code that is easy to read, understand, and maintain for all team members.
