# Google Data Analytics Capstone Project - SQL

*prepared by Zoë Collins*

This is part of the Google Data Analytics Capstone project, and this notebook was created to document my data analysis process.

# Scenario

The director of marketing at Cyclistic, a bike-share company in Chicago, believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. 

# Ask

The key objective for the marketing team is to encourage casual members to purchase annual memberships, as maximizing the number of annual members will be key to future growth.
How casual and annual members use the service differently, why casual riders would buy a membership, and how digital media could affect their marketing tactics need to be identified to design a new marketing strategy that will convert casual riders into annual members.


#First, I uploaded my cleaned CSV files to BigQuery and combined six months of data into one table for analysis.

SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202310`
UNION ALL
SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202311`
UNION ALL
SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202312`
UNION ALL
SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202401`
UNION ALL
SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202402`
UNION ALL
SELECT * FROM `zoe-my-first-sandbox-project.trip_data.data_202403`


#Basic Queries

SELECT member_casual, COUNT(ride_id) as member_count
FROM `zoe-my-first-sandbox-project.trip_data.combined`
GROUP BY member_casual; 

