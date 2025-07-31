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

# Exploratory Data Analysis

## Routes

flight_data6 %>%
  mutate(row = row_number()) %>%
  select(row, Total_Stops, airport_code_departure, airport_code_first_stop, airport_code_second_stop, airport_code_third_stop, airport_code_arrival) %>%
  pivot_longer(!row:Total_Stops, names_to = "stop_on_trip", values_to = "airport_code") %>%
  group_by(Total_Stops, stop_on_trip, airport_code) %>%
  mutate(count = n()) %>%
  mutate(stop_on_trip = factor(stop_on_trip, levels = c("airport_code_departure", 
                                                        "airport_code_first_stop", 
                                                        "airport_code_second_stop", 
                                                        "airport_code_third_stop",
                                                        "airport_code_arrival"))) %>%
  filter(!is.na(airport_code)) %>%
  ggplot(aes(x = stop_on_trip,  y = reorder(airport_code,count), group = row)) +
  geom_point() +
  geom_line(aes(color = count)) +
  labs(x = "Stop on trip", y = "airport code", color = "count of flights\non that path") +
  facet_wrap(.~Total_Stops) +
  theme(axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8))


## Price Outliers

# Number of Price Outliers # 

nrow(rstatix::identify_outliers(data = flight_data6, variable = "Price"))

# Visualizations of outliers# 

flight_data6 %>%
  ggplot(aes(x = Price)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution of Flight Prices",
       x = "Price") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# An outlier is a value 1.5 times that of the IQR. Below, we see outliers are prices above $23,170.

outlier_summary <- rstatix::identify_outliers(data = flight_data6, variable = "Price")

# Create a more readable summary table
outlier_summary %>%
  select(Airline, Date_of_Journey, departure_date_time, Price, is.outlier, is.extreme) %>%
  head(10) 

# Removing the outliers.# 

flight_data7 <- flight_data6 %>%
  filter(Price < 23170)


## Correlation Matrix

# Including the variables in my correlation Matrix# 

colnames(flight_data7)

corr.data <- flight_data7 %>%
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
         weekend, Route, number_of_stops, airport_code_departure, airport_code_first_stop, airport_code_second_stop, airport_code_third_stop, airport_code_arrival,
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

flight_data7 %>%
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

flight_data7 %>%
  filter(!is.na(Duration_Hours)) %>%
  ggplot(aes(x = Duration_Hours, y = Price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', color = "red", se = TRUE) +
  labs(title = "Flight Price by Duration",
       x = "Duration (Hours)",
       y = "Price ($)") +
  theme_minimal()

# Both number of stops and duration# 

flight_data7 %>%
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
# But not necessarily correlated with number of stops

flight_data7 %>%
  filter(!is.na(Route)) %>%
  ggplot(aes(x = reorder(Route, Price), y = Price, fill = Total_Stops)) +
  geom_boxplot( alpha = 0.7) +
  labs(title = "Flight Price by Route",
       x = "Route",
       y = "Price ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() 

# Bringing multiple variables together# 

flight_data7 %>%
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
