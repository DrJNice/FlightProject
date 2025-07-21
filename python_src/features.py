import pandas as pd
import numpy as np
from datetime import datetime


# Import Data
flight_data = pd.read_excel('../Data_Train.xlsx')

# Convert categorical features to 'category' dtype
flight_data = flight_data.astype({
    'Airline': 'category',
    'Source': 'category', 
    'Destination': 'category',
    'Total_Stops': 'category',
    'Additional_Info': 'category'
})

###############################
# Update date features
###############################

# Step 1: Create departure datetime
flight_data1 = flight_data.copy()
flight_data1['departure_date_time'] = pd.to_datetime(
    flight_data1['Date_of_Journey'] + ' ' + flight_data1['Dep_Time'] + ':00',
    format='%d/%m/%Y %H:%M:%S'
)

# Step 2: Process arrival times
flight_data2 = flight_data1.copy()

# Extract time and date components from Arrival_Time
flight_data2['Arrival_Time_Only'] = flight_data2['Arrival_Time'].str.extract(r'^(\d{2}:\d{2})')
flight_data2['Dep_Time'] = flight_data2['Dep_Time'] + ':00'
flight_data2['Arrival_Time_Only'] = flight_data2['Arrival_Time_Only'] + ':00'
flight_data2['Arrival_Date'] = flight_data2['Arrival_Time'].str.extract(r'(\d{1,2} \w{3})$')

# Separate arrival date into day and month
flight_data2[['Arrival_Day', 'Arrival_Month']] = flight_data2['Arrival_Date'].str.split(' ', expand=True)

# Map month abbreviations to numbers
month_mapping = {
    'Jan': 1, 'Feb': 2, 'Mar': 3, 'Apr': 4, 'May': 5, 'Jun': 6,
    'Jul': 7, 'Aug': 8, 'Sep': 9, 'Oct': 10, 'Nov': 11, 'Dec': 12
}
flight_data2['Arrival_Month_Num'] = flight_data2['Arrival_Month'].map(month_mapping)

# Separate departure date
flight_data2[['Departure_Day', 'Departure_Month', 'Departure_Year']] = flight_data2['Date_of_Journey'].str.split('/', expand=True)

# Convert to numeric for calculations
flight_data2['Departure_Day'] = pd.to_numeric(flight_data2['Departure_Day'])
flight_data2['Departure_Month'] = pd.to_numeric(flight_data2['Departure_Month'])
flight_data2['Arrival_Day'] = pd.to_numeric(flight_data2['Arrival_Day'])

# Fill missing arrival dates with departure dates
flight_data2['Arrival_Day'] = flight_data2['Arrival_Day'].fillna(flight_data2['Departure_Day'])
flight_data2['Arrival_Month_Num'] = flight_data2['Arrival_Month_Num'].fillna(flight_data2['Departure_Month'])

# Create combined arrival date string
flight_data2['Arrival_Date_combined'] = (
    flight_data2['Departure_Year'].astype(str) + '-' + 
    flight_data2['Arrival_Month_Num'].astype(int).astype(str).str.zfill(2) + '-' + 
    flight_data2['Arrival_Day'].astype(int).astype(str).str.zfill(2)
)

# Create arrival datetime
flight_data2['arrival_date_time'] = pd.to_datetime(
    flight_data2['Arrival_Date_combined'] + ' ' + flight_data2['Arrival_Time_Only'],
    format='%Y-%m-%d %H:%M:%S'
)

# Clean up columns
flight_data2 = flight_data2.drop(columns=['Arrival_Month', 'Arrival_Time'])
flight_data2 = flight_data2.rename(columns={
    'Dep_Time': 'Departure_Time',
    'Arrival_Month_Num': 'Arrival_Month',
    'Arrival_Time_Only': 'Arrival_Time'
})

# Select final columns
flight_data3 = flight_data2[[
    'Airline', 'Date_of_Journey', 'departure_date_time', 'Departure_Day', 
    'Departure_Month', 'Departure_Year', 'Departure_Time', 'arrival_date_time',
    'Arrival_Day', 'Arrival_Month', 'Arrival_Time', 'Duration', 'Source',
    'Destination', 'Route', 'Total_Stops', 'Price', 'Additional_Info'
]]

# Numeric conversions
flight_data3 = flight_data3.astype({
    'Departure_Day': 'int64',
    'Departure_Month': 'int64', 
    'Departure_Year': 'int64',
    'Arrival_Day': 'int64'
})

# Time conversions
flight_data3[['Departure_Time', 'Arrival_Time']] = flight_data3[['Departure_Time', 'Arrival_Time']].apply(
    lambda x: pd.to_datetime(x, format='%H:%M:%S').dt.time
)

# No regex approach using string operations
flight_data4 = flight_data3.copy()

def parse_duration(duration_str):
    """Convert duration string like '2h 30m' to decimal hours"""
    if pd.isna(duration_str):
        return 0
    
    hours = 0
    minutes = 0
    
    # Split by space and process each part
    parts = str(duration_str).split()
    for part in parts:
        if 'h' in part:
            hours = float(part.replace('h', ''))
        elif 'm' in part:
            minutes = float(part.replace('m', ''))
    
    return hours + (minutes / 60)


flight_data4['Duration_Hours'] = flight_data4['Duration'].apply(parse_duration)

print(flight_data4.head())