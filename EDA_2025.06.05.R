############################################
# Libraries
############################################

library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(hms)

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

# Summary
glimpse(flight_data)

# Making the right data formats
flight_data$Airline <- as.factor(flight_data$Airline)
flight_data$Source <- as.factor(flight_data$Source)
flight_data$Destination <- as.factor(flight_data$Destination)
flight_data$Total_Stops <- as.factor(flight_data$Total_Stops)
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

############################################
# Summary
############################################

glimpse(flight_data4)

summary(flight_data4)


############################################
# Igonring the duration issues, just graphing stuff now
############################################

# Prices by airline 

flight_data4 %>%
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Airline)

# Prices by number of stops 

flight_data4 %>%
  filter(!is.na(Total_Stops)) %>%
  ggplot(aes(x = Price)) +
  geom_histogram() +
  facet_wrap(~Total_Stops)


