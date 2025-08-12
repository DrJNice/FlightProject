## Packages

# Data Manipulation
library(tidyverse) # Basic data manipulation function
library(janitor) # Advanced data manipulation function

# Graphing and Analysis
library(glmnet)

## data

flight_data7 <- read_csv("flight_data_train.csv")
flight_data7 <- as.data.frame(flight_data7)

flight_data8 <- flight_data7 %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))

# Initial Regression Model

# Regression

regression.flight <- lm(Price ~ Departure_Day + Departure_Month + Departure_Hour + Duration_Hours +
                          weekend +
                          Airline + airport_code_departure + airport_code_first_stop + airport_code_second_stop + airport_code_third_stop + airport_code_arrival,
                          data = flight_data8)
summary(regression.flight)

# LASSO Implementation

# Since I have to use complete data, I'm going to change what's included in my model


# Prepare your feature matrix - glmnet requires matrix format
X <- model.matrix(Price ~ Departure_Day + Departure_Month + Departure_Hour + Duration_Hours + number_of_stops +
                    weekend +
                    Airline + airport_code_arrival - 1, 
                  data = flight_data8)

# Extract target variable
y <- flight_data8$Price

# Fit LASSO regression across lambda values
lasso_flight <- glmnet(X, y, alpha = 1)

# Use cross-validation to find optimal lambda
cv_lasso_flight <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# Plot cross-validation results
plot(cv_lasso_flight)

# Extract optimal lambda values
lambda_min <- cv_lasso_flight$lambda.min      
lambda_1se <- cv_lasso_flight$lambda.1se      

# Fit final model with optimal lambda
final_flight_model <- glmnet(X, y, alpha = 1, lambda = lambda_min)

# View selected features (non-zero coefficients)
selected_features <- coef(final_flight_model)
print(selected_features[selected_features[,1] != 0, ])

# Compare number of features: original vs LASSO-selected
cat("Original model features:", ncol(X), "\n")
cat("LASSO selected features:", sum(coef(final_flight_model)[,1] != 0) - 1, "\n")  # -1 for intercept
