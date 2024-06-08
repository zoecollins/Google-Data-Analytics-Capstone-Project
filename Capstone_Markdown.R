install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("geosphere")
library(tidyverse)
library(skimr)
library(janitor)
library(geosphere)


file_2310 <- read_csv("202310-divvy-tripdata.csv")
file_2311 <- read_csv("202311-divvy-tripdata.csv")

file_2312 <- read_csv("202312-divvy-tripdata.csv")
file_2401 <- read_csv("202401-divvy-tripdata.csv")
file_2402 <- read_csv("202402-divvy-tripdata.csv")
file_2403 <- read_csv("202403-divvy-tripdata.csv")

combined_bike_data <- bind_rows(file_2310, file_2311, file_2312, file_2401, file_2402, file_2403,.id="ride_id")

head(combined_bike_data)




combined_bike_data_v2 <- drop_na(combined_bike_data)


 
distance <- distGeo(c("start_lng", "start_lat"), c("end_lng", "end_lat"))

combined_bike_data_v2$distance_meters <- distance



skim(combined_bike_data)
skim(combined_bike_data_v2)


ggplot(data = combined_bike_data) + 
  geom_bar(mapping = aes(x = rideable_type)) + facet_wrap(~member_casual)

ggplot(data = combined_bike_data) +
  geom_point(mapping = aes(x= member_casual, y= start_station_name))
