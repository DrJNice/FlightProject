############################################
# Libraries
############################################

library(readxl) # read data
library(tidyverse) # most of what I use
library(lubridate) # helps with the dates
library(janitor) # advanced data cleaning
library(hms) # another date function
library(googleCloudStorageR) # let's me pull in the data from GCP
library(corrplot) # graph correlation plots
library(leaps) # fancy regressions
library(broom) # fancy regressions

############################################
# Data
############################################

gcs_get_object("Data_Train.xlsx", bucket = bucket_name, saveToDisk = "Data_Train.xlsx")

flight_data <- read_excel("Data_Train.xlsx")

flight_data <- as.data.frame(flight_data)

############################################
# Data cleaning
############################################ 

# Missing data 
flight_data %>% summarise_all(~ sum(is.na(.)))

# it's only 1 row of missing data, let's just remove it

flight_data <- flight_data %>% na.omit()

# also removing the one flight with 4 stops

flight_data <- flight_data %>% filter(Total_Stops != "4 stops")

# Summary
glimpse(flight_data)

# Making the right data formats
flight_data$Airline <- as.factor(flight_data$Airline)
flight_data$Source <- as.factor(flight_data$Source)
flight_data$Destination <- as.factor(flight_data$Destination)
flight_data$Total_Stops <- factor(flight_data$Total_Stops, levels = c("non-stop", "1 stop", "2 stops", "3 stops", "4 stops"))
flight_data$Additional_Info <- as.factor(flight_data$Additional_Info)


############################################
# dates are the most fun :/
############################################

# arrival time is date and time
# but departure time is just the time
# So let's get that fixed

flight_data1 <- flight_data %>%
  mutate(departure_date_time = dmy_hms(paste0(Date_of_Journey, " ", Dep_Time, ":00"))) 

# Let's fix the arrival times too 
# If arrival time is blank, it's the same as departure day

flight_data2 <- flight_data1 %>%
  mutate(Arrival_Time_Only = str_extract(Arrival_Time, "^\\d{2}:\\d{2}"),  # Pull out just time
         Dep_Time = paste0(Dep_Time, ":00"),
         Arrival_Time_Only = paste0(Arrival_Time_Only, ":00"),
         Arrival_Date = str_extract(Arrival_Time, "\\d{1,2} \\w{3}$") # Pull out just date
  ) %>%
  separate(Arrival_Date, into = c("Arrival_Day", "Arrival_Month"),  sep = " ", remove = FALSE) %>% # separate month and date
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
  separate(Date_of_Journey, # The dates don't have years, so pulling out the departure years as well  
           into = c("Departure_Day", "Departure_Month", "Departure_Year"), 
           sep = "/", 
           remove = FALSE) %>%
  mutate(Arrival_Day = ifelse(!is.na(Arrival_Day), Arrival_Day, as.numeric(Departure_Day)), # replacing blanks with departures
         Arrival_Month_Num = ifelse(!is.na(Arrival_Month_Num), Arrival_Month_Num, as.numeric(Departure_Month)), # replacing blanks with departures
         Arrival_Date_combined = paste0(Departure_Year, "-", Arrival_Month_Num, "-", Arrival_Day)) %>% # putting it all together
  mutate(arrival_date_time = ymd_hms(paste0(Arrival_Date_combined, " ", Arrival_Time_Only))) %>% # making it the same format 
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

# Making the right data formats
flight_data3$Departure_Day <- as.numeric(flight_data3$Departure_Day)
flight_data3$Departure_Month <- as.numeric(flight_data3$Departure_Month)
flight_data3$Departure_Year <- as.numeric(flight_data3$Departure_Year)
flight_data3$Arrival_Day <- as.numeric(flight_data3$Arrival_Day)


flight_data3$Departure_Time <- as_hms(flight_data3$Departure_Time)
flight_data3$Arrival_Time <- as_hms(flight_data3$Arrival_Time)

# Do I need to care about time zones?

unique(flight_data4$Source)
unique(flight_data4$Destination)

# Claude says these are all the same! Even easier
  
############################################
# tackle this duration nonsense
############################################

# get duration into hours

flight_data4 <- flight_data3 %>%
  mutate(
    Duration_Hours = replace_na(as.numeric(str_extract(Duration, "\\d+(?=h)")), 0) + 
      replace_na(as.numeric(str_extract(Duration, "\\d+(?=m)")), 0) / 60
  )

flight_data4

############################################
# is duration the same as difference between start time and end time
############################################

# Yes it is with a few exceptions!

test <- flight_data4 %>%
  rowwise() %>%
  mutate(calculated_duration = arrival_date_time - departure_date_time,
         calculated_duration_hours = as.numeric(calculated_duration, units = "hours")
         ) %>%
  mutate(difference_between_calculation_file = Duration_Hours - calculated_duration_hours) %>%
  arrange(difference_between_calculation_file) 

flight_data4 %>%
  rowwise() %>%
  mutate(calculated_duration = arrival_date_time - departure_date_time,
         calculated_duration_hours = as.numeric(calculated_duration, units = "hours")
  ) %>%
  mutate(difference_between_calculation_file = Duration_Hours - calculated_duration_hours) %>%
  ggplot(aes(x = difference_between_calculation_file)) +
  geom_histogram(width = .5)

# 92 responses where the calculated duration is more than 1 hour off 
# And no pattern to the count 

flight_data4 %>%
  rowwise() %>%
  mutate(calculated_duration = arrival_date_time - departure_date_time,
         calculated_duration_hours = as.numeric(calculated_duration, units = "hours")
  ) %>%
  mutate(difference_between_calculation_file = Duration_Hours - calculated_duration_hours,
         abs_difference_between_calculation_file = abs(difference_between_calculation_file)) %>%
  filter(difference_between_calculation_file > 1) %>%
  ggplot(aes(x = departure_date_time)) +
  geom_histogram(bins = 30)

# Let's add a day of week variable

flight_data4$departure_day_of_week <- weekdays(flight_data4$departure_date_time)

# Round time down to the hour

flight_data4$Departure_Hour = hour(flight_data4$Departure_Time)


############################################
# Summary
############################################

glimpse(flight_data4)

summary(flight_data4)

############################################
# First let's look at those really high prices
############################################

# Got some outliers

flight_data4 %>%
  ggplot(aes(x = Price)) +
  geom_histogram(bins = 30)

flight_data4 %>%
  ggplot(aes(x = Price)) +
  geom_boxplot()

# Got about 94 outliers
# Let's just remove them for now

nrow(rstatix::identify_outliers(data = flight_data4,
                           variable = "Price"))

# is outlier is more than 1.5 times the IQR
# is extreme is more than 3 times the IQR

summary(rstatix::identify_outliers(data = flight_data4,
                           variable = "Price"))

# Prices above 23170 are considered outliers, so let's just remove them for now

flight_data5 <- flight_data4 %>%
  filter(Price < 23170)

############################################
# Correlation matrix
############################################

unique(flight_data5$Total_Stops)

flight_data5 %>%
  ggplot(aes(x = Departure_Time)) +
  geom_histogram()

# Airline obviously could influence price
# The overall date time of departure is meant to capture when the flight has left relative to the rest of the data set
# departure day tells me if when in the month matters
# departure day of week tells me if Sunday vs Thursday matters
# departure month tells if February vs March matters
# departure hour tells me if 9am vs 6pm matters
# Source and destination tell me if city matters
# number of stops and duration are obvious
# price is the outcome

colnames(flight_data5)

unique(flight_data5$Airline)

corr.data <- flight_data5 %>%
  mutate(departure_day_of_week_numeric = case_when(departure_day_of_week == "Sunday" ~ 1,
                                           departure_day_of_week == "Monday" ~ 2,
                                           departure_day_of_week == "Tuesday" ~ 3,
                                           departure_day_of_week == "Wednesday" ~ 4,
                                           departure_day_of_week == "Thursday" ~ 5,
                                           departure_day_of_week == "Friday" ~ 6,
                                           departure_day_of_week == "Saturday" ~ 7),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"),
         number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                 Total_Stops == "1 stop" ~ 1,
                                 Total_Stops == "2 stops" ~ 2,
                                 Total_Stops == "3 stops" ~ 3,
                                 Total_Stops == "4 stops" ~ 4),
         premium_economy = str_detect(Airline, "Premium")) %>%
  select(Airline, premium_economy, departure_date_time, Departure_Day, Departure_Month, Departure_Time,
         departure_day_of_week_numeric, arrival_date_time, Arrival_Day, Arrival_Month, Arrival_Time,
         weekend, Source, Destination, Route, number_of_stops, 
        Duration_Hours, Price) %>%
  data.matrix()


#corr.data

corr_matrix <- round(cor(corr.data, use="pairwise.complete.obs", method="pearson"), 2)

corrplot(corr_matrix, method="circle", type="lower", tl.col = "black", tl.cex = .75,
         tl.srt = 45)

corr_matrix_df <- rstatix::cor_gather(corr_matrix)

# Number of Stop and Duration hours are the only variables that seem to have any level of influence on price
# I'm surprised Airline is so poorly correlated cuz there def seem to be price trends based on the second graph

corr_matrix_df %>%
  filter(var1 == 'Price')

flight_data5 %>%
  filter(Price < 20000) %>%
  ggplot(aes(x = Price)) +
  geom_density() +
  facet_wrap(~Airline)


############################################
# Ignoring the duration issues, just graphing stuff now
############################################

# There do seem to be pretty solid linear relationships here 

flight_data5 %>%
  filter(!is.na(Total_Stops)) %>%
  ggplot(aes(x = Price)) +
  geom_density() +
  facet_wrap(~Total_Stops)

flight_data5 %>%
  filter(!is.na(Total_Stops)) %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4)) %>%
  ggplot(aes(x = number_of_stops, y = Price)) +
  geom_point(position = 'jitter')+
  geom_smooth(method = 'lm')


# duration

flight_data5 %>%
  filter(!is.na(Duration_Hours)) %>%
  ggplot(aes(x = Duration_Hours, y = Price)) +
  geom_point()+
  geom_smooth(method = 'lm')

# Let's add them together
# well that's a more complicated relationships, haha

flight_data5 %>%
  filter(!is.na(Duration_Hours)) %>%
  filter(!is.na(Total_Stops)) %>%
  ggplot(aes(x = Duration_Hours, y = Price, fill = Total_Stops)) +
  geom_point()+
  geom_smooth(method = 'lm')


# Let's also look at departure day and destination, just for funsies
# price seem to get lower as the month goes on

flight_data5 %>%
  filter(!is.na(Departure_Day)) %>%
  ggplot(aes(x = Departure_Day, y = Price)) +
  geom_point(position = 'jitter')+
  geom_smooth(method = 'lm')

# okay I do see some relationship with price here too 

flight_data5 %>%
  filter(!is.na(Destination)) %>%
  ggplot(aes(x = Destination, y = Price)) +
  geom_boxplot()

# Let's put it all together
# for some destinations, there were only non-stop flights
# so harder to make sense of the data

flight_data5 %>%
  mutate(Departure_Day_categorical = case_when(Departure_Day <= 10 ~ "First third of the month",
                                               Departure_Day >= 20 ~ "Last third of the month",
                                               TRUE ~ "Middle third of the month"),
         Departure_Day_categorical = factor(Departure_Day_categorical, levels = c("First third of the month",
                                                                                     "Middle third of the month",
                                                                                     "Last third of the month"))) %>%
  ggplot(aes(x = Duration_Hours, y = Price)) +
  geom_point(aes(color = Destination)) +
  geom_smooth(method = 'lm') +
  facet_grid(rows = vars(Departure_Day_categorical),
             cols = vars(Total_Stops))



############################################
# Let's run a regression, eh
# First I'll start with how statistics would do it
# Which is I take my variables with the highest correlations and include them
############################################

regression.flight <- lm(Price ~ Total_Stops + Duration_Hours +
                          Departure_Day + Destination, data = flight_data5)

summary(regression.flight)

# number of stops is significant
# duration and departure day are significant
# most of the arrival cities are significant
# Accounts for 52% variance which ain't bad!


############################################
# Now let's run it like a data scientist
# which is probably closer to an all subsets regression
############################################

flight_data6 <- flight_data5 %>%
  mutate(departure_day_of_week_numeric = case_when(departure_day_of_week == "Sunday" ~ 1,
                                                 departure_day_of_week == "Monday" ~ 2,
                                                 departure_day_of_week == "Tuesday" ~ 3,
                                                 departure_day_of_week == "Wednesday" ~ 4,
                                                 departure_day_of_week == "Thursday" ~ 5,
                                                 departure_day_of_week == "Friday" ~ 6,
                                                 departure_day_of_week == "Saturday" ~ 7),
       number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                   Total_Stops == "1 stop" ~ 1,
                                   Total_Stops == "2 stops" ~ 2,
                                   Total_Stops == "3 stops" ~ 3,
                                   Total_Stops == "4 stops" ~ 4))

colnames(flight_data6)

predictor_variables <- c("Airline", "departure_date_time", "Departure_Day", 
                         "departure_day_of_week_numeric", "Departure_Month", 
                         "Departure_Hour", "Source", "Destination", 
                         "number_of_stops", "Duration_Hours")

outcome_variable <- "Price"


flight_all_subsets_leaps <- function(data = flight_data6, outcome_var = "Price", 
                                     predictor_vars = predictor_variables, method = "exhaustive") {
  
  # Check if data exists
  if (!exists("flight_data6")) {
    stop("flight_data6 dataset not found. Please load your data first.")
  }
  
  # Remove rows with missing values in key variables
  analysis_data <- data %>%
    select(all_of(c(outcome_var, predictor_vars))) %>%
    na.omit()
  
  cat("Analysis dataset created with", nrow(analysis_data), "complete observations\n")
  cat("Original dataset had", nrow(data), "observations\n\n")
  
  # Create formula
  formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Run all subsets regression
  regsubsets_result <- regsubsets(formula_obj, 
                                  data = analysis_data, 
                                  nbest = 1,        # Keep best model of each size
                                  nvmax = length(predictor_vars),  # Max variables
                                  method = method)   # "exhaustive", "forward", "backward"
  
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

get_best_flight_model_details <- function(data = flight_data6, outcome_var = "Price", model_variables_string) {
  
  # Parse the variable string to extract original variable names
  # This handles the dummy variables created by R for categorical variables
  
  # Extract original variable names from the dummy variable names
  original_vars <- c()
  
  # Check for each of our original predictor variables
  if (grepl("Airline", model_variables_string)) {
    original_vars <- c(original_vars, "Airline")
  }
  if (grepl("departure_date_time", model_variables_string)) {
    original_vars <- c(original_vars, "departure_date_time")
  }
  if (grepl("Departure_Day", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Day")
  }
  if (grepl("departure_day_of_week_numeric", model_variables_string)) {
    original_vars <- c(original_vars, "departure_day_of_week_numeric")
  }
  if (grepl("Departure_Month", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Month")
  }
  if (grepl("Departure_Hour", model_variables_string)) {
    original_vars <- c(original_vars, "Departure_Hour")
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
  
  cat("=== BEST FLIGHT PRICE PREDICTION MODEL ===\n")
  cat("Original Variables Used:", paste(original_vars, collapse = ", "), "\n")
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

if (exists("flight_data6")) {
  
  cat("=== RUNNING ALL SUBSETS REGRESSION ON FLIGHT PRICE DATA ===\n\n")
  
  # Method 1: Using leaps package (recommended)
  cat("Running analysis with leaps package...\n")
  leaps_results <- flight_all_subsets_leaps()
  
  cat("\n=== RESULTS SUMMARY ===\n")
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
  plot_comparison <- plot_flight_model_comparison(leaps_results$results, top_n = 10)
  print(plot_comparison)
  
} else {
  cat("ERROR: flight_data6 dataset not found!\n")
  cat("Please load your dataset first, then re-run this code.\n")
  cat("For example: flight_data6 <- read.csv('your_file.csv')\n")
}

# Best model for predicting price would be:

ds.regression.flight <- lm(Price ~ Airline + departure_date_time +
                          Departure_Day + Departure_Month + Departure_Hour +
                          Destination + number_of_stops, data = flight_data6)

summary(ds.regression.flight)
