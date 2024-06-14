# Load necessary libraries

install.packages("tidyverse")
install.packages("skimr")
install.packages("geosphere")

library(tidyverse)
library(skimr)
library(geosphere)

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

#Creating separate columns for both starting and ending times and dates.
bike_data_v2 <- bike_data_v2 %>%
  mutate(
    start_date = as.Date(started_at),
    end_date = as.Date(ended_at),
    start_time = format(started_at, "%H:%M:%S"),
    end_time = format(ended_at, "%H:%M:%S")
  )


print(bike_data_v2)
skim(bike_data_v2)
str(bike_data_v2)

#The data frame also includes starting and ending coordinates. I'm creating a column to calculate the distance between starting and ending stations for each rider's trip using distGeo(). 
#This function outputs in meters which I will convert to miles.
bike_data_v2 <- bike_data_v2 %>%
  mutate (
    distance_meters = distGeo(cbind(start_lng, start_lat), cbind(end_lng, end_lat)),
    distance_miles = distance_meters / 1609.344
    )

bike_data_v2$start_time = hms::as_hms(bike_data_v2$start_time)
bike_data_v2$end_time = hms::as_hms(bike_data_v2$end_time)

ggplot(data = bike_data_v2) + 
  geom_bar(mapping = aes(x = rideable_type, fill=member_casual)) 

ggplot(data = bike_data_v2) +
  geom_histogram(mapping = aes(x = start_time))

ggplot(data = bike_data_v2) +
  geom_bar(mapping = aes(distance_miles, fill=member_casual))

For better readability in ggplot() I will create time categories of by hour_of_day.
