# Google Data Analytics Capstone Project

*prepared by Zoë Collins*

This is part of the Google Data Analytics Capstone project, and this notebook was created to document my data analysis process.

# Scenario

The director of marketing at Cyclistic, a bike-share company in Chicago, believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. 

# Ask

The key objective for the marketing team is to encourage casual members to purchase annual memberships, as maximizing the number of annual members will be key to future growth.
How casual and annual members use the service differently, why casual riders would buy a membership, and how digital media could affect their marketing tactics need to be identified to design a new marketing strategy that will convert casual riders into annual members.

# Load necessary libraries

install.packages("tidyverse")
install.packages("skimr")
install.packages("geosphere")

library(tidyverse)
library(skimr)
library(geosphere)

# Prepare

#To prepare, I have downloaded 6 months of data from Cyclistic's database. In Excel, I created a categorical column "ride_length" for each file and ensured that all 6 files had the same columns and column names to later combine properly in R or SQL. These were then exported via Excel (.csv) files.

#Here we can see that some variables have rows with missing data. For the integrity of our analysis, I will remove them using the drop_na function.

#Reading the csv files
file_2310 <- read_csv("202310-divvy-tripdata.csv")
file_2311 <- read_csv("202311-divvy-tripdata.csv")

file_2312 <- read_csv("202312-divvy-tripdata.csv")
file_2401 <- read_csv("202401-divvy-tripdata.csv")
file_2402 <- read_csv("202402-divvy-tripdata.csv")
file_2403 <- read_csv("202403-divvy-tripdata.csv")

#Combining the 6 months of data into one data frame to be analyzed "combined_bike_data"
bike_data <- bind_rows(file_2310, file_2311, file_2312, file_2401, file_2402, file_2403,.id="ride_id")

head(bike_data)
str(bike_data)
skim(bike_data)
spec(bike_data)

#cleaned data by removing rows with missing values.
bike_data_v2 <- drop_na(bike_data)
str(bike_data_v2)
skim(bike_data_v2)
head(bike_data_v2$started_at)


#converting the data type from character to datettime.

bike_data_v2$started_at = parse_date_time(bike_data_v2$started_at, orders = "mdy HM")
bike_data_v2$ended_at = parse_date_time(bike_data_v2$ended_at, orders = "mdy HM")
bike_data_v2$ride_length = hms::as_hms(bike_data_v2$ride_length)

# Process
#Creating categorical columns
#Now that the data types are updated, I will separate the start and end times into their own columns for better analysis.

#Creating separate columns for both starting and ending times and dates.
bike_data_v2 <- bike_data_v2 %>%
  mutate(
    start_date = as.Date(started_at),
    end_date = as.Date(ended_at),
    start_time = format(started_at, "%H:%M:%S"),
    end_time = format(ended_at, "%H:%M:%S")
  )


# making sure the start and end time's show as numeric types.
bike_data_v2$start_time = hms::as_hms(bike_data_v2$start_time)
bike_data_v2$end_time = hms::as_hms(bike_data_v2$end_time)


**distGeo()**

**distance_miles**

##The data frame also includes starting and ending coordinates. 
#**distance_miles** column represents the distance between the starting and ending stations for each rider's trip in miles.

bike_data_v2 <- bike_data_v2 %>%
  mutate (
    distance_meters = distGeo(cbind(start_lng, start_lat), cbind(end_lng, end_lat)),
    distance_miles = round(distance_meters / 1609.344, 1)
  )


#Hour of day
bike_data_v2 <- bike_data_v2 %>%
  mutate ( start_hour = hour(start_time),
           end_hour = hour(end_time),
           month_start = month(start_date)
  )

###GRAPHS###


#histogram
ggplot(data = bike_data_v2) +
  geom_histogram(mapping = aes(x = start_time))

#bar
ggplot(data = bike_data_v2) + 
  geom_bar(mapping = aes(x = rideable_type, fill=member_casual), position = "dodge")

#What is the average start hour per member type?
ggplot(data = bike_data_v2) +
  geom_bar(mapping = aes(x = start_hour, fill=member_casual), stat="count", position = "dodge") +
  scale_x_continuous (breaks = seq(0, 24, by = 1)) +
  labs(title = "Bar Plot of Start Hour by Member Type",
     x = "Start Hour",
     y = "Count")  


#What is the average length of trips per member type?
ggplot(data = bike_data_v2) +
  geom_bar(mapping = aes(x = distance_miles, fill=member_casual), position = "dodge") + 
  scale_x_continuous(limits = c(0,8), breaks = seq(0, 8, by = 0.5)) + 
  scale_y_continuous(limits = c(0,1000))


For better readability in ggplot() I will create time categories of by hour_of_day.
