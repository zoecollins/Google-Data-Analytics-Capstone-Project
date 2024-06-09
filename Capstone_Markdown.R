install.packages("tidyverse")
install.packages("skimr")
install.packages("geosphere")

library(tidyverse)
library(skimr)
library(geosphere)


file_2310 <- read_csv("202310-divvy-tripdata.csv")
file_2311 <- read_csv("202311-divvy-tripdata.csv")

file_2312 <- read_csv("202312-divvy-tripdata.csv")
file_2401 <- read_csv("202401-divvy-tripdata.csv")
file_2402 <- read_csv("202402-divvy-tripdata.csv")
file_2403 <- read_csv("202403-divvy-tripdata.csv")

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


#The "start_at" and "ended_at" columns have both the date and the time aggregated together. 
#Next we convert the "started_at" and "ended_at" columns from a character type to date-time (POSIXlt).

bike_data_v2$started_at = as.POSIXlt(bike_data_v2$started_at,format="%m/%d/%Y %M:%S")
bike_data_v2$ended_at = as.POSIXlt(bike_data_v2$ended_at,format="%m/%d/%Y %M:%S")
bike_data_v2$ride_length = hms::as_hms(bike_data_v2$ride_length)

#Now that the data types are updated, I will separate the start and end times into their own columns for better analysis.

bike_data_v2$start_date = as.Date(bike_data_v2$started_at, format="%m/%d/%Y")
bike_data_v2$end_date = as.Date(bike_data_v2$started_at, format="%m/%d/%Y")
bike_data_v2$start_time = format(bike_data_v2$started_at, "%H:%M:%S")
bike_data_v2$end_time = format(bike_data_v2$ended_at, "%H:%M:%S")
 

distance <- distGeo(c("start_lng", "start_lat"), c("end_lng", "end_lat"))
combined_bike_data_v2$distance_meters <- distance





ggplot(data = combined_bike_data) + 
  geom_bar(mapping = aes(x = rideable_type)) + facet_wrap(~member_casual)

ggplot(data = combined_bike_data) +
  geom_point(mapping = aes(x= member_casual, y= start_station_name))
