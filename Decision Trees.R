# library

library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)

# data

# training data

flight_data7 <- read_csv("flight_data_train.csv")
flight_data7 <- as.data.frame(flight_data7)

glimpse(flight_data7)

flight_data8 <- flight_data7 %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))

# Prediction Data

flight_data_test <- read_csv("flight_data_test.csv")
flight_data_test <- as.data.frame(flight_data_test)

flight_data_test <- flight_data_test %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))

# analysis

set.seed(42)


# initial decision tree model that mirrors the regression

colnames(flight_data8)

?rpart

model0 <- rpart(Price ~ Departure_Day + Departure_Month + Departure_Hour + Duration_Hours + number_of_stops +
                 weekend +
                 Airline + airport_code_arrival + airport_code_departure + airport_code_first_stop + 
                 airport_code_second_stop + airport_code_third_stop,
               data = flight_data8, method = "anova")


# Seems like a lot of error? 
# But it's still a pretty shallow tree so that makes sense
# Will def have more error than a regular linear regression

summary(model0)
printcp(model0)

# rules of decision tree

rpart.rules(model)

# Graph of data
rpart.plot(model0)

# Coefficient of Variation

plotcp(model0)


## More complicated model

model1 <- rpart(Price ~ Departure_Day + Departure_Month + Departure_Hour + Duration_Hours + number_of_stops +
                 weekend + Date_of_Journey + departure_date_time + arrival_date_time + 
                 Airline + airport_code_arrival + airport_code_departure + airport_code_first_stop + 
                 airport_code_second_stop + airport_code_third_stop,
               data = flight_data8, method = "anova")

plotcp(model1)

# Graph of data
rpart.plot(model1)


# Predict

p <- predict(model1, flight_data_test)

summary(p)

# RMSEA
# Quite High

sqrt(mean((flight_data_test$Price-p)^2))

# R squared
# 72% R squared

(cor(flight_data_test$Price,p))^2

# Test how well my model did

flight_data_test$predicted_price <- predict(model1, flight_data_test)

# Look at the correlation between predicted and actual price

cor(flight_data_test$Price, flight_data_test$predicted_price)

flight_data_test %>%
  ggplot(aes(x = Price, y = predicted_price)) +
  geom_point(size = 3, position = 'jitter') +
  geom_smooth(method = 'lm')

# Let's add more depth!

model3 <- rpart(Price ~ Departure_Day + Departure_Month + Departure_Hour + Duration_Hours + number_of_stops +
                  weekend + Date_of_Journey + departure_date_time + arrival_date_time + 
                  Airline + airport_code_arrival + airport_code_departure + airport_code_first_stop + 
                  airport_code_second_stop + airport_code_third_stop,
                data = flight_data8, method = "anova",
               minsplit = 8,  minbucket = 2, maxdepth = 30)


# changing the depth upped the tree a little, but not much
plotcp(model3)

# Graph of data
rpart.plot(model3)

