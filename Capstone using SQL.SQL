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
