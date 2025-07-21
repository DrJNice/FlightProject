## Packages

# Accessing and Reading Data
library(googleCloudStorageR) # let's me pull in the data from GCP
library(readxl) # read data

# Data Manipulation
library(tidyverse) # Basic data manipulation function
library(janitor) # Advanced data manipulation function

# Dealing with Dates
library(lubridate) 
library(hms) 

# Graphing and Analysis
library(corrplot) # graph correlation plots
library(leaps) # fancy regressions
library(broom) # fancy regressions


## Data

flight_data <- read_excel("Data_Train.xlsx")
flight_data <- as.data.frame(flight_data)

glimpse(flight_data)


# Data Cleaning

## Removing Missing Data and Data with No Variance

#Amount of Missingness by Variable

flight_data %>% 
  summarise_all(~ sum(is.na(.))) 

# Only one flight has 4 stops

flight_data %>% 
  filter(Total_Stops == "4 stops")

# Only one flight has 4 stops

flight_data %>% 
  filter(is.na(Total_Stops))

# the row with 4 stops is an outlier
# the row with missing data can't be extrapolated
# removing both

flight_data <- flight_data %>% 
  filter(Total_Stops != "4 stops") %>%
  na.omit()

## Updating Data Formats

flight_data$Airline <- as.factor(flight_data$Airline)
flight_data$Source <- as.factor(flight_data$Source)
flight_data$Destination <- as.factor(flight_data$Destination)
flight_data$Total_Stops <- factor(flight_data$Total_Stops, levels = c("non-stop", "1 stop", "2 stops", "3 stops", "4 stops"))
flight_data$Additional_Info <- as.factor(flight_data$Additional_Info)

## Dates

# Arrival time is date and time but departure time is just the time, so let's get that fixed.

flight_data1 <- flight_data %>%
  mutate(departure_date_time = dmy_hms(paste0(Date_of_Journey, " ", Dep_Time, ":00"))) 

# There is only a date for arrival if it was different than the departure date. 
# Let's get that fixed so that both departure and arrival have a full date and time set up.


flight_data2 <- flight_data1 %>%
  mutate(Arrival_Time_Only = str_extract(Arrival_Time, "^\\d{2}:\\d{2}"), # Pull out just time
         Dep_Time = paste0(Dep_Time, ":00"),
         Arrival_Time_Only = paste0(Arrival_Time_Only, ":00"),
         Arrival_Date = str_extract(Arrival_Time, "\\d{1,2} \\w{3}$") # Pull out just date
  ) %>%
  separate(Arrival_Date, into = c("Arrival_Day", "Arrival_Month"), sep = " ", remove = FALSE) %>%
  mutate(Arrival_Month_Num = case_when(Arrival_Month == "Jan" ~ 01, # replacing the words with numbers
                                       Arrival_Month == "Feb" ~ 02,
                                       Arrival_Month == "Mar" ~ 03,
                                       Arrival_Month == "Apr" ~ 04,
                                       Arrival_Month == "May" ~ 05,
                                       Arrival_Month == "Jun" ~ 06,
                                       Arrival_Month == "Jul" ~ 07,
                                       Arrival_Month == "Aug" ~ 08,
                                       Arrival_Month == "Sep" ~ 09,
                                       Arrival_Month == "Oct" ~ 10,
                                       Arrival_Month == "Nov" ~ 11,
                                       Arrival_Month == "Dec" ~ 12)) %>%
  separate(Date_of_Journey, # The dates don't have years, so pulling out the departure years as reference
           into = c("Departure_Day", "Departure_Month", "Departure_Year"), 
           sep = "/", 
           remove = FALSE) %>%
  mutate(Arrival_Day = ifelse(!is.na(Arrival_Day), Arrival_Day, as.numeric(Departure_Day)), 
         Arrival_Month_Num = ifelse(!is.na(Arrival_Month_Num), Arrival_Month_Num, as.numeric(Departure_Month)),
         Arrival_Date_combined = paste0(Departure_Year, "-", Arrival_Month_Num, "-", Arrival_Day)) %>%
  mutate(arrival_date_time = ymd_hms(paste0(Arrival_Date_combined, " ", Arrival_Time_Only))) %>%
  select(-Arrival_Month, -Arrival_Time) %>%
  rename(Departure_Time = Dep_Time,
         Arrival_Month = Arrival_Month_Num, 
         Arrival_Time = Arrival_Time_Only) %>%
  select(Airline, Date_of_Journey, 
         departure_date_time, Departure_Day, Departure_Month, Departure_Year, Departure_Time,
         arrival_date_time, Arrival_Day, Arrival_Month, Arrival_Time,
         Duration, 
         Source, Destination, Route, Total_Stops, Price, Additional_Info)

flight_data3 <- flight_data2


# Making sure all the dates are formatted correctly

flight_data3$Departure_Day <- as.numeric(flight_data3$Departure_Day)
flight_data3$Departure_Month <- as.numeric(flight_data3$Departure_Month)
flight_data3$Departure_Year <- as.numeric(flight_data3$Departure_Year)
flight_data3$Arrival_Day <- as.numeric(flight_data3$Arrival_Day)

flight_data3$Departure_Time <- as_hms(flight_data3$Departure_Time)
flight_data3$Arrival_Time <- as_hms(flight_data3$Arrival_Time)

flight_data4 <- flight_data3


# Let's add features for day of week and hour of departure

flight_data4$departure_day_of_week <- weekdays(flight_data4$departure_date_time)
flight_data4$Departure_Hour = hour(flight_data4$Departure_Time)


# I don't need to care about time zones since all the cities are in the same time zone.

unique(flight_data4$Source)
unique(flight_data4$Destination)


## Duration

# First calculating duration

flight_data4 <- flight_data4 %>%
  mutate(
    Duration_Hours = replace_na(as.numeric(str_extract(Duration, "\\d+(?=h)")), 0) + 
      replace_na(as.numeric(str_extract(Duration, "\\d+(?=m)")), 0) / 60
  )


# Then examining if calculated duration is the same as provided duration.
#  There are a handful of flights that appear to be different. For some, it seems the arrival is before the departure.

flight_data4 %>%
  rowwise() %>%
  mutate(calculated_duration = arrival_date_time - departure_date_time,
         calculated_duration_hours = as.numeric(calculated_duration, units = "hours")
  ) %>%
  mutate(difference_between_calculation_file = Duration_Hours - calculated_duration_hours,
         abs_difference_between_calculation_file = abs(difference_between_calculation_file)) %>%
  filter(difference_between_calculation_file > 1) %>%
  select(departure_date_time, arrival_date_time, calculated_duration_hours, Duration_Hours, difference_between_calculation_file) %>%
  head(10)

# No time-based patterns to wonky durations.


flight_data4 %>%
  rowwise() %>%
  mutate(calculated_duration = arrival_date_time - departure_date_time,
         calculated_duration_hours = as.numeric(calculated_duration, units = "hours")
  ) %>%
  mutate(difference_between_calculation_file = Duration_Hours - calculated_duration_hours,
         abs_difference_between_calculation_file = abs(difference_between_calculation_file)) %>%
  filter(difference_between_calculation_file > 1) %>%
  ggplot(aes(x = departure_date_time)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Flights with Duration Discrepancies",
       x = "Departure Date/Time",
       y = "Count") +
  theme_minimal()

# Remove data where the arrival was before the departure.# 

flight_data4 <- flight_data4 %>%
  rowwise() %>%
  mutate(wonky_times = case_when(arrival_date_time > departure_date_time ~ "keep",
                                 TRUE ~ "remove"
  )) %>%
  filter(wonky_times == "keep") %>%
  select(-wonky_times) %>%
  ungroup() 

# Let's look at route

unique(flight_data4$Route)

flight_data5 <- flight_data4 %>%
  separate_wider_delim(
    cols = Route, 
    delim = " → ", 
    names = c("airport_code_departure", "airport_code_first_stop", 
              "airport_code_second_stop", "airport_code_arrival"),
    too_few = "align_start",  # Fill missing values with NA on the right
    too_many = "drop",        # Drop extra stops beyond 4 airports
    cols_remove = FALSE
  ) %>%
  # Count the actual number of stops in each route
  mutate(num_airports = str_count(Route, " → ") + 1) %>%
  # Fix the column assignments based on actual number of airports
  mutate(
    # For routes with only 2 airports (non-stop)
    airport_code_arrival = case_when(
      num_airports == 2 ~ airport_code_first_stop,  # Move second position to arrival
      num_airports == 3 ~ airport_code_second_stop, # Move third position to arrival  
      TRUE ~ airport_code_arrival                   # Keep fourth position as arrival
    ),
    # For routes with only 3 airports (1 stop)
    airport_code_second_stop = case_when(
      num_airports == 2 ~ NA_character_,            # No second stop for non-stop
      num_airports == 3 ~ NA_character_,            # No second stop for 1-stop
      TRUE ~ airport_code_second_stop               # Keep second stop for 2+ stops
    ),
    # For routes with only 2 airports (non-stop)  
    airport_code_first_stop = case_when(
      num_airports == 2 ~ NA_character_,            # No first stop for non-stop
      TRUE ~ airport_code_first_stop                # Keep first stop for 1+ stops
    )
  ) %>%
  select(-num_airports)

# Looking into the stops
# These are all in the same time zone, so I don't need to worry about that
# whew

unique(flight_data5$Source)
unique(flight_data5$Destination)

unique(flight_data5$airport_code_first_stop)

# Exploratory Data Analysis

## Price Outliers

# Number of Price Outliers # 
  
nrow(rstatix::identify_outliers(data = flight_data5, variable = "Price"))

# Visualizations of outliers# 
  
flight_data5 %>%
  ggplot(aes(x = Price)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution of Flight Prices",
       x = "Price") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# An outlier is a value 1.5 times that of the IQR. Below, we see outliers are prices above $23,170.

outlier_summary <- rstatix::identify_outliers(data = flight_data5, variable = "Price")

# Create a more readable summary table
outlier_summary %>%
  select(Airline, Date_of_Journey, departure_date_time, Price, is.outlier, is.extreme) %>%
  head(10) 

# Removing the outliers.# 

flight_data6 <- flight_data5 %>%
  filter(Price < 23170)


## Correlation Matrix

# Including the variables in my correlation Matrix# 

colnames(flight_data6)

corr.data <- flight_data6 %>%
  mutate(weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"),
         number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         premium_economy = str_detect(Airline, "Premium")) %>%
  select(Airline, premium_economy, departure_date_time, Departure_Day, Departure_Month, Departure_Time,
         departure_day_of_week, arrival_date_time, Arrival_Day, Arrival_Month, Arrival_Time,
         weekend, Source, Destination, Route, number_of_stops, airport_code_first_stop, airport_code_second_stop,
         Duration_Hours, Price) %>%
  data.matrix()

corr_matrix <- round(cor(corr.data, use="pairwise.complete.obs", method="pearson"), 2)

corrplot(corr_matrix, method="circle", type="lower", tl.col = "black", tl.cex = .75,
         tl.srt = 45, title = "Correlation Matrix of Flight Variables")


# Let's look at just the Price relationships.# 

corr_matrix_df <- rstatix::cor_gather(corr_matrix)

price_correlations <- corr_matrix_df %>%
  filter(var1 == 'Price') %>%
  arrange(desc(abs(cor))) %>%
  mutate(cor = round(cor, 3))

price_correlations

# number of stop and duration are easy enough to figure out
# Route and second airport code and destination are all pretty up there
# There's something about these variables that has some influence but I'm not sure what 

## Graphing Strongest Relationships

# Price by total stops# 

flight_data6 %>%
  filter(!is.na(Total_Stops)) %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4)) %>%
  ggplot(aes(x = number_of_stops, y = Price)) +
  geom_point(position = 'jitter', alpha = 0.6) +
  geom_smooth(method = 'lm', color = "red", se = TRUE) +
  labs(title = "Flight Price by Number of Stops",
       x = "Number of Stops",
       y = "Price ($)") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:3, labels = c("Non-stop", "1 stop", "2 stops", "3 stops"))

# Duration of flight# 

flight_data6 %>%
  filter(!is.na(Duration_Hours)) %>%
  ggplot(aes(x = Duration_Hours, y = Price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', color = "red", se = TRUE) +
  labs(title = "Flight Price by Duration",
       x = "Duration (Hours)",
       y = "Price ($)") +
  theme_minimal()


# Both number of stops and duration# 

flight_data6 %>%
  filter(!is.na(Duration_Hours)) %>%
  filter(!is.na(Total_Stops)) %>%
  ggplot(aes(x = Duration_Hours, y = Price, color = Total_Stops)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', se = TRUE) +
  labs(title = "Flight Price by Duration and Number of Stops",
       x = "Duration (Hours)",
       y = "Price ($)",
       color = "Total Stops") +
  theme_minimal() +
  theme(legend.position = "right")


# Route # 
# Well there is a clear impact of route

flight_data6 %>%
  filter(!is.na(Route)) %>%
  ggplot(aes(x = reorder(Route, Price), y = Price)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Flight Price by Route",
       x = "Route",
       y = "Price ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() 


# Bringing multiple variables together# 

flight_data6 %>%
  ggplot(aes(x = Duration_Hours, y = Price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', color = "black", se = TRUE) +
  facet_grid(rows = vars(Destination),
             cols = vars(Total_Stops)) +
  labs(title = "Flight Price by Duration, Stops, and Destination",
       x = "Duration (Hours)",
       y = "Price ($)",
       color = "Destination") +
  theme_minimal() +
  theme(legend.position = "right",
        strip.text = element_text(size = 9))


# Regressions like a statistician

# Approach 1 

# As a statistician, I would pick the variables that have the strongest relationships with the outcome, don't have too much multicollinearity, and a theoretical reason to influence Price.
# From below, we can see that the number of stops significantly predicts Price when controlling for other variables, as does duration, departure day, and most of the arrival cities.
# This model predicts about 52% of the variance in total Price, which isn't bad! But I think it could be better.

regression.flight <- lm(Price ~ Total_Stops + Duration_Hours +
                       Departure_Day + Destination, data = flight_data6)

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


flight_data7 <- flight_data6 %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))


# Now I'm going to add a random categorical and a random continuous feature to my variable

flight_data7$random_categorical <- sample(c('red', 'orange', 'yellow', 'green', 'blue'), nrow(flight_data7), replace=TRUE)

flight_data7$random_continuous <- sample(1:9, nrow(flight_data7), replace = TRUE)/10

  
# First, I identify the variables in the model.# 

# I'm going to do that by running a regular standardized linear regression
# Whichever variables have higher standardized values than the random variables will not be included
# first scale the items and then run it

glimpse(flight_data7)

regression.flight <- lm(Price ~ 
                        # Continuous
                          scale(Departure_Day) + scale(Departure_Month) + scale(Departure_Hour) +
                          scale(number_of_stops) + scale(Duration_Hours) +
                          scale(random_continuous) +
                        # Dicotomous
                          weekend +
                        # Categorical
                          Airline + Route + Source + Destination + airport_code_first_stop + airport_code_second_stop + 
                        # Dates 
                          departure_date_time  + arrival_date_time, 
                        data = flight_data7)

summary(regression.flight)

# Let's look at those regression coefficients
regression_coefficients <- data.frame(regression.flight$coefficients)

# 
regression_coefficients %>% 
  mutate(abs = abs(regression.flight.coefficients)) %>%
  filter(!is.na(abs)) %>%
  arrange(abs)


## 

predictor_variables <- c("Airline", 
                         "departure_date_time", "Departure_Day",  "Departure_Month",  "Departure_Hour", 
                         "arrival_date_time", 
                         "weekend",
                         "Route", "Source", "Destination", "airport_code_first_stop", "airport_code_second_stop",
                         "number_of_stops", "Duration_Hours",
                         "random_categorical", "random_continuous")

outcome_variable <- "Price"



#############################################
# Alternative: Forward/Backward Selection (faster and handles dependencies better)
#############################################

flight_stepwise_selection <- function(data = flight_data7, outcome_var = "Price", 
                                      predictor_vars = predictor_variables, method = "both") {
  
  # Check if data exists
  if (!exists("flight_data7")) {
    stop("flight_data7 dataset not found. Please load your data first.")
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

flight_all_subsets_manual <- function(data = flight_data5, outcome_var = "Price", 
                                      predictor_vars = predictor_variables) {
  
  # Check if data exists
  if (!exists("flight_data5")) {
    stop("flight_data5 dataset not found. Please load your data first.")
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

get_best_flight_model_details <- function(data = flight_data7, outcome_var = "Price", model_variables_string) {
  
  # Parse the variable string to extract original variable names
  # This handles the dummy variables created by R for categorical variables
  
  # Extract original variable names from the dummy variable names
  original_vars <- c()
  
  # Check for each of our expanded predictor variables
  if (grepl("Airline", model_variables_string)) {
    original_vars <- c(original_vars, "Airline")
  }
  if (grepl("departure_date_time", model_variables_string)) {
    original_vars <- c(original_vars, "departure_date_time")
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
  if (grepl("arrival_date_time", model_variables_string)) {
    original_vars <- c(original_vars, "arrival_date_time")
  }
  if (grepl("weekend", model_variables_string)) {
    original_vars <- c(original_vars, "weekend")
  }
  if (grepl("Route", model_variables_string)) {
    original_vars <- c(original_vars, "Route")
  }
  if (grepl("Source", model_variables_string)) {
    original_vars <- c(original_vars, "Source")
  }
  if (grepl("Destination", model_variables_string)) {
    original_vars <- c(original_vars, "Destination")
  }
  if (grepl("airport_code_first_stop", model_variables_string)) {
    original_vars <- c(original_vars, "airport_code_first_stop")
  }
  if (grepl("airport_code_second_stop", model_variables_string)) {
    original_vars <- c(original_vars, "airport_code_second_stop")
  }
  if (grepl("number_of_stops", model_variables_string)) {
    original_vars <- c(original_vars, "number_of_stops")
  }
  if (grepl("Duration_Hours", model_variables_string)) {
    original_vars <- c(original_vars, "Duration_Hours")
  }
  if (grepl("random_categorical", model_variables_string)) {
    original_vars <- c(original_vars, "random_categorical")
  }
  if (grepl("random_continuous", model_variables_string)) {
    original_vars <- c(original_vars, "random_continuous")
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
  
  cat("=== BEST EXPANDED FLIGHT PRICE PREDICTION MODEL ===\n")
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

# Check if flight_data7 exists before running
if (exists("flight_data7")) {
  
  cat("=== RUNNING EXPANDED ALL SUBSETS REGRESSION ON FLIGHT PRICE DATA ===\n\n")
  
  # Method 1: Using leaps package (recommended for this many variables)
  cat("Running expanded analysis with leaps package...\n")
  leaps_results <- flight_all_subsets_leaps()
  
  cat("\n=== EXPANDED RESULTS SUMMARY ===\n")
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
  plot_comparison <- plot_flight_model_comparison(leaps_results$results, top_n = 15)
  print(plot_comparison)
  
  # Optional: Run manual method for comparison (WARNING: very slow with 16 variables)
  cat("\n=== OPTIONAL: MANUAL METHOD ===\n")
  cat("WARNING: Manual method would test", 2^length(predictor_variables) - 1, "models (65,535 combinations!)\n")
  cat("This would take a very long time. The leaps package method is strongly recommended.\n")
  cat("Uncomment the lines below ONLY if you have significant computing time:\n\n")
  cat("# manual_results <- flight_all_subsets_manual()\n")
  cat("# print(head(manual_results, 20))\n")
  
} else {
  cat("ERROR: flight_data7 dataset not found!\n")
  cat("Please load your dataset first, then re-run this code.\n")
  cat("For example: flight_data7 <- read.csv('your_file.csv')\n")
}

#############################################
# Summary of what each variable might tell us about flight prices:
#############################################

cat("\n=== INTERPRETATION GUIDE FOR FLIGHT PRICE VARIABLES ===\n")
cat("Airline: Different airlines have different pricing strategies\n")
cat("departure_date_time: Specific date/time can affect demand and pricing\n")
cat("Departure_Day: Day of month might affect pricing (end/beginning of month)\n")
cat("departure_day_of_week_numeric: Weekdays vs weekends have different pricing\n")
cat("Departure_Month: Seasonal effects on flight pricing\n")
cat("Departure_Hour: Time of day affects convenience and pricing\n")
cat("Source: Origin airport affects base pricing and competition\n")
cat("Destination: Destination affects demand and pricing\n")
cat("number_of_stops: More stops usually means lower prices\n")
cat("Duration_Hours: Longer flights generally cost more\n")

