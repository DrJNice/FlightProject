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


## Pull in Data

flight_data <- read_excel("Data_Train.xlsx")
flight_data <- as.data.frame(flight_data)

# Get a scan 

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
  separate(Arrival_Date, into = c("Arrival_Day", "Arrival_Month"), sep = " ", remove = FALSE) %>% # separate out the string
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
  mutate(Arrival_Day = ifelse(!is.na(Arrival_Day), Arrival_Day, as.numeric(Departure_Day)), # saying that if no 
         Arrival_Month_Num = ifelse(!is.na(Arrival_Month_Num), Arrival_Month_Num, as.numeric(Departure_Month)),
         Arrival_Date_combined = paste0(Departure_Year, "-", Arrival_Month_Num, "-", Arrival_Day)) %>%
  mutate(arrival_date_time = ymd_hms(paste0(Arrival_Date_combined, " ", Arrival_Time_Only))) %>%
  select(-Arrival_Month, -Arrival_Time) %>%
  rename(Departure_Time = Dep_Time, # rename my variables
         Arrival_Month = Arrival_Month_Num, 
         Arrival_Time = Arrival_Time_Only) %>%
  select(Airline, Date_of_Journey, # select the ones I want
         departure_date_time, Departure_Day, Departure_Month, Departure_Year, Departure_Time,
         arrival_date_time, Arrival_Day, Arrival_Month, Arrival_Time,
         Duration, 
         Source, Destination, Route, Total_Stops, Price, Additional_Info)

# rename the dataset

flight_data3 <- flight_data2

# Making sure all the dates are formatted correctly

flight_data3$Departure_Day <- as.numeric(flight_data3$Departure_Day)
flight_data3$Departure_Month <- as.numeric(flight_data3$Departure_Month)
flight_data3$Departure_Year <- as.numeric(flight_data3$Departure_Year)
flight_data3$Arrival_Day <- as.numeric(flight_data3$Arrival_Day)

flight_data3$Departure_Time <- as_hms(flight_data3$Departure_Time)
flight_data3$Arrival_Time <- as_hms(flight_data3$Arrival_Time)

# replicate data set

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
              "airport_code_second_stop", "airport_code_third_stop", "airport_code_arrival"),
    too_few = "align_start",  # Fill missing values with NA on the right
    too_many = "drop",        # Drop extra stops beyond 5 airports
    cols_remove = FALSE
  ) %>%
  # Count the actual number of stops in each route
  mutate(num_airports = str_count(Route, " → ") + 1) %>%
  # Fix the column assignments based on actual number of airports
  mutate(
    # Fix arrival column - always the last airport
    airport_code_arrival = case_when(
      num_airports == 2 ~ airport_code_first_stop,   # 2 airports: departure → arrival
      num_airports == 3 ~ airport_code_second_stop,  # 3 airports: departure → stop → arrival  
      num_airports == 4 ~ airport_code_third_stop,   # 4 airports: departure → stop → stop → arrival
      TRUE ~ airport_code_arrival                    # 5 airports: departure → stop → stop → stop → arrival
    ),
    
    # Fix third stop column
    airport_code_third_stop = case_when(
      num_airports <= 4 ~ NA_character_,             # No third stop for routes with 4 or fewer airports
      TRUE ~ airport_code_third_stop                 # Keep third stop only for 5-airport routes
    ),
    
    # Fix second stop column  
    airport_code_second_stop = case_when(
      num_airports <= 3 ~ NA_character_,             # No second stop for routes with 3 or fewer airports
      TRUE ~ airport_code_second_stop                # Keep second stop for 4+ airport routes
    ),
    
    # Fix first stop column
    airport_code_first_stop = case_when(
      num_airports <= 2 ~ NA_character_,             # No first stop for non-stop flights
      TRUE ~ airport_code_first_stop                 # Keep first stop for 3+ airport routes
    )
  ) %>%
  select(-num_airports)

# Verify the results with some examples
flight_data5 %>%
  select(Route, airport_code_departure, airport_code_first_stop, 
         airport_code_second_stop, airport_code_third_stop, airport_code_arrival) %>%
  # Show examples of different route types
  slice(c(1:5, 
          which(str_count(flight_data5$Route, " → ") == 1)[1:2],  # 2 airports
          which(str_count(flight_data5$Route, " → ") == 2)[1:2],  # 3 airports  
          which(str_count(flight_data5$Route, " → ") == 3)[1:2],  # 4 airports
          which(str_count(flight_data5$Route, " → ") == 4)[1:2]   # 5 airports (if any)
  )) %>%
  arrange(str_count(Route, " → "))

# Looking into the stops
# These are all in the same time zone, so I don't need to worry about that
# whew

unique(flight_data5$Source)
unique(flight_data5$Destination)

unique(flight_data5$airport_code_first_stop)

# I wonder
# Does City and airport code match up perfectly
# yes it does for departure
# and arrival

flight_data5 %>%
  group_by(Source, airport_code_departure) %>%
  summarise(count = n())

# but I'm also realizing that the Destination has New Delhi instead of Deli, so need to change that first

flight_data5 <- flight_data5 %>%
  mutate(Destination = ifelse(Destination == "New Delhi", "Delhi", Destination)) 

flight_data5 %>%
  group_by(Destination, airport_code_arrival) %>%
  summarise(count = n())

flight_data6 <- flight_data5

# Making numeric variables

flight_data6 <- flight_data6 %>%
  mutate(number_of_stops = case_when(Total_Stops == "non-stop" ~ 0,
                                     Total_Stops == "1 stop" ~ 1,
                                     Total_Stops == "2 stops" ~ 2,
                                     Total_Stops == "3 stops" ~ 3,
                                     Total_Stops == "4 stops" ~ 4),
         weekend = case_when(departure_day_of_week == "Sunday" | departure_day_of_week == "Saturday" ~ "Weekend",
                             TRUE ~ "Weekday"))

# make my train test val data random variable

flight_data6$data_set_division <- sample(c("Train", "Test", "Val"), nrow(flight_data6), replace=TRUE, prob = c(0.8, .10, .10))

# Double checking that it split right

flight_data6 %>%
  group_by(data_set_division) %>%
  summarise(count = n()) %>%
  mutate(perc = 100 * count/sum(count))
  

flight_data6_train <- flight_data6 %>% filter(data_set_division == "Train")

flight_data6_test <- flight_data6 %>% filter(data_set_division == "Test")

flight_data6_val <- flight_data6 %>% filter(data_set_division == "Val")


write_csv(flight_data6_train, "flight_data_train.csv", na = "")  
  
write_csv(flight_data6_test, "flight_data_test.csv", na = "") 

write_csv(flight_data6_val, "flight_data_val.csv", na = "") 

