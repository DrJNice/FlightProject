# Regressions like a statistician

# Approach 1 

# As a statistician, I would pick the variables that have the strongest relationships with the outcome, don't have too much multicollinearity, and a theoretical reason to influence Price.
# From below, we can see that the number of stops significantly predicts Price when controlling for other variables, as does duration, departure day, and most of the arrival cities.
# This model predicts about 52% of the variance in total Price, which isn't bad! But I think it could be better.

regression.flight <- lm(Price ~ Total_Stops + Duration_Hours +
                          airport_code_departure + Route, data = flight_data7)

# Create a tidy summary
regression_summary <- tidy(regression.flight) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

regression_summary 

# Model summary statistics
glance(regression.flight) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df, nobs) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) 


# Regressions like a Data Scientist

# If I free myself from needing to rely on theory to make predictions, I would go with an All Subsets regression approach, which essentially tests every combination of possible variables, calculates the variance predicted and various model fit statistics (AIC, BIC, etc.) and selects the model with the highest predicted variances and lowest model fit statistics.
# While I've done this analysis before in SPSS, I had not yet done it in R, so I had Claude help me write the code.
## Preparing Data for All Subsets Regression


flight_data8 <- flight_data7 %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))


# Now I'm going to add a random categorical and a random continuous feature to my variable

flight_data8$random_categorical <- sample(c('red', 'orange', 'yellow', 'green', 'blue'), nrow(flight_data8), replace=TRUE)

flight_data8$random_continuous <- sample(1:9, nrow(flight_data8), replace = TRUE)/10


# First, I identify the variables in the model.# 

# I'm going to do that by running a regular standardized linear regression
# Whichever variables have higher standardized values than the random variables will not be included
# first scale the items and then run it

glimpse(flight_data8)

regression.flight <- lm(Price ~ 
                          # Continuous
                          scale(Departure_Day) + scale(Departure_Month) + scale(Departure_Hour) +
                          scale(number_of_stops) + scale(Duration_Hours) +
                          scale(random_continuous) +
                          # Dicotomous
                          weekend +
                          # Categorical
                          Airline + random_categorical +
                          airport_code_departure + airport_code_first_stop + airport_code_second_stop + airport_code_third_stop + airport_code_arrival +
                          # Dates 
                          departure_date_time  + arrival_date_time, 
                        data = flight_data8)

summary(regression.flight)

# Let's look at those regression coefficients
regression_coefficients <- data.frame(regression.flight$coefficients)

options(scipen = 999)
#options(scipen = 0)

# 
regression_coefficients %>% 
  mutate(abs = abs(regression.flight.coefficients)) %>%
  filter(!is.na(abs)) %>%
  arrange(abs)

# for my continuous variables
# departure_date_time 
# did a worse job predicting than the random continuous variable

# for my categorical variables
# nothing did worse


## 

predictor_variables <- c(# Continuous
  "Departure_Day", "Departure_Month", "Departure_Hour",
  "number_of_stops", "Duration_Hours",
  # Dicotomous
  "weekend",
  # Categorical
  "Airline", "airport_code_departure", "airport_code_first_stop", "airport_code_second_stop", "airport_code_third_stop", "airport_code_arrival")

outcome_variable <- "Price"


#############################################
# Flight Data Analysis Setup - STREAMLINED
#############################################

# Your streamlined predictor variables (avoiding high-cardinality categoricals)
predictor_variables <- c(# Continuous
  "Departure_Day", "Departure_Month", "Departure_Hour",
  "number_of_stops", "Duration_Hours",
  # Dichotomous
  "weekend",
  # Lower-cardinality Categorical (keeping only essential ones)
  "Airline", "airport_code_departure", "airport_code_first_stop", "airport_code_second_stop", "airport_code_third_stop", "airport_code_arrival")

# NOTE: Removed "Route", "airport_code_first_stop", "airport_code_second_stop" 
# to avoid linear dependency issues

outcome_variable <- "Price"

cat("=== STREAMLINED ALL SUBSETS REGRESSION ===\n")
cat("Testing", length(predictor_variables), "predictor variables\n")
cat("This will test up to", 2^length(predictor_variables) - 1, "different model combinations\n")
cat("Variables included:", paste(predictor_variables, collapse = ", "), "\n\n")

#############################################
# Data exploration function to check cardinality
#############################################

check_variable_cardinality <- function(data = flight_data8, vars = predictor_variables) {
  if (!exists("flight_data8")) {
    cat("flight_data8 not found\n")
    return(NULL)
  }
  
  cat("=== VARIABLE CARDINALITY CHECK ===\n")
  for (var in vars) {
    if (var %in% names(data)) {
      unique_vals <- length(unique(data[[var]], na.rm = TRUE))
      cat(var, ": ", unique_vals, " unique values\n")
      if (unique_vals > 50) {
        cat("  WARNING: High cardinality - may cause linear dependency issues\n")
      }
    } else {
      cat(var, ": NOT FOUND in dataset\n")
    }
  }
  cat("\n")
}

#############################################
# Method 1: Using the leaps package (most efficient)
#############################################

flight_all_subsets_leaps <- function(data = flight_data8, outcome_var = "Price", 
                                     predictor_vars = predictor_variables, method = "exhaustive") {
  
  # Check if data exists
  if (!exists("flight_data8")) {
    stop("flight_data8 dataset not found. Please load your data first.")
  }
  
  # Check variable cardinality first
  check_variable_cardinality(data, predictor_vars)
  
  # Remove rows with missing values in key variables
  analysis_data <- data %>%
    select(all_of(c(outcome_var, predictor_vars))) %>%
    na.omit()
  
  cat("Analysis dataset created with", nrow(analysis_data), "complete observations\n")
  cat("Original dataset had", nrow(data), "observations\n\n")
  
  # Run all subsets regression with really.big=TRUE for safety
  cat("Running all subsets regression...\n")
  regsubsets_result <- regsubsets(formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))), 
                                  data = analysis_data, 
                                  nbest = 1,        # Keep best model of each size
                                  nvmax = length(predictor_vars),  # Max variables
                                  method = method,   # "exhaustive", "forward", "backward"
                                  really.big = TRUE) # Handle large searches
  
  # Extract results
  summary_results <- summary(regsubsets_result)
  
  # Create results data frame
  results_df <- data.frame(
    n_variables = 1:length(summary_results$rsq),
    r_squared = summary_results$rsq,
    adj_r_squared = summary_results$adjr2,
    cp = summary_results$cp,
    bic = summary_results$bic,
    variables_included = apply(summary_results$which[,-1], 1, function(x) {
      paste(names(x)[x], collapse = ", ")
    })
  )
  
  # Find best models by different criteria
  best_rsq_idx <- which.max(results_df$r_squared)
  best_adj_rsq_idx <- which.max(results_df$adj_r_squared)
  best_cp_idx <- which.min(results_df$cp)
  best_bic_idx <- which.min(results_df$bic)
  
  # Add indicator columns
  results_df$best_rsq <- 1:nrow(results_df) == best_rsq_idx
  results_df$best_adj_rsq <- 1:nrow(results_df) == best_adj_rsq_idx
  results_df$best_cp <- 1:nrow(results_df) == best_cp_idx
  results_df$best_bic <- 1:nrow(results_df) == best_bic_idx
  
  return(list(
    results = results_df,
    regsubsets_object = regsubsets_result,
    analysis_data = analysis_data,
    best_models = list(
      highest_rsq = results_df[best_rsq_idx, ],
      best_adj_rsq = results_df[best_adj_rsq_idx, ],
      best_cp = results_df[best_cp_idx, ],
      best_bic = results_df[best_bic_idx, ]
    )
  ))
}

#############################################
# Alternative: Forward/Backward Selection (faster and handles dependencies better)
#############################################

flight_stepwise_selection <- function(data = flight_data8, outcome_var = "Price", 
                                      predictor_vars = predictor_variables, method = "both") {
  
  # Check if data exists
  if (!exists("flight_data8")) {
    stop("flight_data8 dataset not found. Please load your data first.")
  }
  
  # Remove rows with missing values in key variables
  analysis_data <- data %>%
    select(all_of(c(outcome_var, predictor_vars))) %>%
    na.omit()
  
  cat("Stepwise selection dataset created with", nrow(analysis_data), "complete observations\n")
  cat("Original dataset had", nrow(data), "observations\n\n")
  
  # Create full model formula
  full_formula <- formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
  
  # Fit full model
  full_model <- lm(full_formula, data = analysis_data)
  
  # Perform stepwise selection
  cat("Performing stepwise selection (method =", method, ")...\n")
  if (method == "forward") {
    # Start with null model for forward selection
    null_model <- lm(formula(paste(outcome_var, "~ 1")), data = analysis_data)
    step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                       direction = "forward", trace = FALSE)
  } else if (method == "backward") {
    # Start with full model for backward selection
    step_model <- step(full_model, direction = "backward", trace = FALSE)
  } else {
    # Both directions
    step_model <- step(full_model, direction = "both", trace = FALSE)
  }
  
  # Get model summary
  step_summary <- summary(step_model)
  
  cat("\n=== STEPWISE SELECTION RESULTS ===\n")
  cat("Final model R-squared:", round(step_summary$r.squared, 4), "\n")
  cat("Final model Adjusted R-squared:", round(step_summary$adj.r.squared, 4), "\n")
  cat("Variables selected:", length(step_model$coefficients) - 1, "out of", length(predictor_vars), "\n")
  cat("Variables in final model:", paste(names(step_model$coefficients)[-1], collapse = ", "), "\n\n")
  
  return(list(
    model = step_model,
    summary = step_summary,
    aic = AIC(step_model),
    bic = BIC(step_model),
    formula = formula(step_model),
    variables_selected = names(step_model$coefficients)[-1]
  ))
}

flight_all_subsets_manual <- function(data = flight_data6, outcome_var = "Price", 
                                      predictor_vars = predictor_variables) {
  
  # Check if data exists
  if (!exists("flight_data6")) {
    stop("flight_data6 dataset not found. Please load your data first.")
  }
  
  # Remove rows with missing values in key variables
  analysis_data <- data %>%
    select(all_of(c(outcome_var, predictor_vars))) %>%
    na.omit()
  
  cat("Testing all possible combinations of flight variables...\n")
  cat("This will test", 2^length(predictor_vars) - 1, "different models\n\n")
  
  n_vars <- length(predictor_vars)
  all_combinations <- list()
  results_list <- list()
  
  # Generate all possible combinations (2^n - 1, excluding empty set)
  for (i in 1:n_vars) {
    combinations_of_size_i <- combn(predictor_vars, i, simplify = FALSE)
    all_combinations <- c(all_combinations, combinations_of_size_i)
  }
  
  # Test each combination
  pb <- txtProgressBar(min = 0, max = length(all_combinations), style = 3)
  
  for (i in seq_along(all_combinations)) {
    setTxtProgressBar(pb, i)
    current_vars <- all_combinations[[i]]
    
    # Create formula
    formula_str <- paste(outcome_var, "~", paste(current_vars, collapse = " + "))
    
    # Fit model
    tryCatch({
      model <- lm(as.formula(formula_str), data = analysis_data)
      model_summary <- summary(model)
      
      results_list[[i]] <- data.frame(
        combination_id = i,
        n_variables = length(current_vars),
        variables = paste(current_vars, collapse = ", "),
        r_squared = model_summary$r.squared,
        adj_r_squared = model_summary$adj.r.squared,
        f_statistic = model_summary$fstatistic[1],
        p_value = pf(model_summary$fstatistic[1], 
                     model_summary$fstatistic[2], 
                     model_summary$fstatistic[3], 
                     lower.tail = FALSE),
        aic = AIC(model),
        bic = BIC(model)
      )
    }, error = function(e) {
      # Handle cases where model fails (e.g., perfect multicollinearity)
      results_list[[i]] <<- data.frame(
        combination_id = i,
        n_variables = length(current_vars),
        variables = paste(current_vars, collapse = ", "),
        r_squared = NA,
        adj_r_squared = NA,
        f_statistic = NA,
        p_value = NA,
        aic = NA,
        bic = NA
      )
    })
  }
  
  close(pb)
  
  # Combine results
  results_df <- do.call(rbind, results_list)
  
  # Remove failed models
  results_df <- results_df[!is.na(results_df$r_squared), ]
  
  # Sort by R-squared (descending)
  results_df <- results_df[order(-results_df$r_squared), ]
  
  return(results_df)
}

#############################################
# Visualization function (matches your ggplot style)
#############################################

plot_flight_model_comparison <- function(results_df, top_n = 15) {
  
  # Get top models
  top_models <- head(results_df, top_n)
  
  # Create the plot
  p <- top_models %>%
    mutate(variables_wrapped = str_wrap(variables_included, width = 50)) %>%  # Fix: use variables_included column
    mutate(variables_wrapped = fct_reorder(variables_wrapped, r_squared)) %>%
    ggplot(aes(x = variables_wrapped, y = r_squared)) +
    geom_bar(stat = 'identity', fill = 'steelblue', alpha = 0.7) +
    geom_text(aes(label = paste0(round(r_squared, 3))), 
              hjust = -.1, size = 3.5, color = "black", fontface = 'bold') +
    coord_flip() +
    theme(plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size = 10, color = "#666666"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = paste("Top", top_n, "Flight Price Prediction Models by R-squared"),
         x = "\nVariable Combinations", 
         y = "R-squared\n",
         caption = paste("Total combinations tested:", nrow(results_df)))
  
  return(p)
}

#############################################
# Helper function to get detailed results for best flight price model
#############################################

get_best_flight_model_details <- function(data = flight_data8, outcome_var = "Price", model_variables_string) {
  
  # Parse the variable string to extract original variable names
  # This handles the dummy variables created by R for categorical variables
  
  # Extract original variable names from the dummy variable names
  original_vars <- c()
  
  # Check for each of our streamlined predictor variables
  if (grepl("Airline", model_variables_string)) {
    original_vars <- c(original_vars, "Airline")
  }
  if (grepl("Departure_Day", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Day")
  }
  if (grepl("Departure_Month", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Month")
  }
  if (grepl("Departure_Hour", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Hour")
  }
  if (grepl("weekend", model_variables_string)) {
    original_vars <- c(original_vars, "weekend")
  }
  if (grepl("Source", model_variables_string)) {
    original_vars <- c(original_vars, "Source")
  }
  if (grepl("Destination", model_variables_string)) {
    original_vars <- c(original_vars, "Destination")
  }
  if (grepl("number_of_stops", model_variables_string)) {
    original_vars <- c(original_vars, "number_of_stops")
  }
  if (grepl("Duration_Hours", model_variables_string)) {
    original_vars <- c(original_vars, "Duration_Hours")
  }
  
  # Remove rows with missing values using original variable names
  analysis_data <- data %>%
    select(all_of(c(outcome_var, original_vars))) %>%
    na.omit()
  
  # Create formula with original variables
  formula_str <- paste(outcome_var, "~", paste(original_vars, collapse = " + "))
  
  # Fit the model
  best_model <- lm(as.formula(formula_str), data = analysis_data)
  
  # Get detailed summary
  model_summary <- summary(best_model)
  
  cat("=== BEST STREAMLINED FLIGHT PRICE PREDICTION MODEL ===\n")
  cat("Original Variables Used (", length(original_vars), "total):", paste(original_vars, collapse = ", "), "\n")
  cat("R-squared:", round(model_summary$r.squared, 4), "\n")
  cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
  cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
  cat("P-value:", format.pval(pf(model_summary$fstatistic[1], 
                                 model_summary$fstatistic[2], 
                                 model_summary$fstatistic[3], 
                                 lower.tail = FALSE)), "\n")
  cat("Number of observations:", nrow(analysis_data), "\n\n")
  
  print(model_summary)
  
  return(best_model)
}

#############################################
# Run the analysis on your flight data
#############################################

# Check if flight_data8 exists before running
if (exists("flight_data8")) {
  
  cat("=== RUNNING STREAMLINED ALL SUBSETS REGRESSION ON FLIGHT PRICE DATA ===\n\n")
  
  # First, check the cardinality of variables to identify potential issues
  check_variable_cardinality()
  
  # Method 1: Try exhaustive search with streamlined variables
  cat("Attempting exhaustive search with streamlined variables...\n")
  tryCatch({
    leaps_results <- flight_all_subsets_leaps()
    
    cat("\n=== STREAMLINED RESULTS SUMMARY ===\n")
    cat("All models ranked by number of variables:\n")
    print(leaps_results$results)
    
    cat("\n=== BEST MODELS BY DIFFERENT CRITERIA ===\n")
    cat("Highest R-squared:\n")
    print(leaps_results$best_models$highest_rsq)
    
    cat("\nBest Adjusted R-squared:\n")
    print(leaps_results$best_models$best_adj_rsq)
    
    cat("\nBest BIC (Bayesian Information Criterion):\n")
    print(leaps_results$best_models$best_bic)
    
    # Get detailed results for the model with highest R-squared
    cat("\n=== DETAILED RESULTS FOR HIGHEST R-SQUARED MODEL ===\n")
    best_model_rsq <- get_best_flight_model_details(model_variables_string = leaps_results$best_models$highest_rsq$variables_included)
    
    # Get detailed results for the model with best BIC (often better for prediction)
    cat("\n=== DETAILED RESULTS FOR BEST BIC MODEL (Recommended) ===\n")
    best_model_bic <- get_best_flight_model_details(model_variables_string = leaps_results$best_models$best_bic$variables_included)
    
    # Create visualization
    cat("\nCreating visualization...\n")
    plot_comparison <- plot_flight_model_comparison(leaps_results$results, top_n = min(15, nrow(leaps_results$results)))
    print(plot_comparison)
    
    # Option to run manual method (now more feasible with fewer variables)
    cat("\n=== OPTIONAL: MANUAL EXHAUSTIVE SEARCH ===\n")
    cat("With", length(predictor_variables), "variables, manual method would test", 2^length(predictor_variables) - 1, "models.\n")
    cat("This is now computationally feasible. Uncomment to run:\n")
    cat("# manual_results <- flight_all_subsets_manual()\n")
    cat("# print(head(manual_results, 15))\n")
    cat("# plot_manual_comparison <- plot_flight_model_comparison(manual_results, top_n = 15)\n")
    cat("# print(plot_manual_comparison)\n")
    
  }, error = function(e) {
    cat("Exhaustive search still failed:", e$message, "\n")
    cat("Switching to stepwise selection method...\n\n")
    
    # Method 2: Stepwise selection (more robust for categorical variables)
    cat("=== RUNNING STEPWISE SELECTION (ALTERNATIVE METHOD) ===\n")
    
    # Forward selection
    cat("1. Forward Selection:\n")
    forward_results <- flight_stepwise_selection(method = "forward")
    
    # Backward selection  
    cat("\n2. Backward Selection:\n")
    backward_results <- flight_stepwise_selection(method = "backward")
    
    # Both directions
    cat("\n3. Bidirectional Selection:\n") 
    both_results <- flight_stepwise_selection(method = "both")
    
    # Compare the three methods
    cat("\n=== COMPARISON OF STEPWISE METHODS ===\n")
    comparison_df <- data.frame(
      Method = c("Forward", "Backward", "Both"),
      R_squared = c(summary(forward_results$model)$r.squared,
                    summary(backward_results$model)$r.squared, 
                    summary(both_results$model)$r.squared),
      Adj_R_squared = c(summary(forward_results$model)$adj.r.squared,
                        summary(backward_results$model)$adj.r.squared,
                        summary(both_results$model)$adj.r.squared),
      AIC = c(forward_results$aic, backward_results$aic, both_results$aic),
      BIC = c(forward_results$bic, backward_results$bic, both_results$bic),
      N_Variables = c(length(forward_results$variables_selected),
                      length(backward_results$variables_selected),
                      length(both_results$variables_selected))
    )
    print(comparison_df)
    
    # Recommend best model
    best_method_idx <- which.max(comparison_df$Adj_R_squared)
    cat("\nRECOMMENDED MODEL: ", comparison_df$Method[best_method_idx], 
        " (Highest Adjusted R-squared: ", round(comparison_df$Adj_R_squared[best_method_idx], 4), ")\n")
    
    # Print the recommended model details
    if (comparison_df$Method[best_method_idx] == "Forward") {
      cat("\n=== RECOMMENDED MODEL DETAILS ===\n")
      print(summary(forward_results$model))
    } else if (comparison_df$Method[best_method_idx] == "Backward") {
      cat("\n=== RECOMMENDED MODEL DETAILS ===\n")
      print(summary(backward_results$model))
    } else {
      cat("\n=== RECOMMENDED MODEL DETAILS ===\n")
      print(summary(both_results$model))
    }
  })
  
} else {
  cat("ERROR: flight_data8 dataset not found!\n")
  cat("Please load your dataset first, then re-run this code.\n")
  cat("For example: flight_data8 <- read.csv('your_file.csv')\n")
}

