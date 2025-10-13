#.............................
# Author: Alexander Staub
# Description: export the spotify IDs related to the chart data
# Date created: 13.10.2025
# dependencies: 2510_chart_song_spotify_merge.R 
## 
#.............................

# loading packages
library(readr)
library(here)
library(dplyr)

#loading in the datasets
charts_with_spotify_ids <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/country_chart_data/all_charts_songs_1980_2000_spotify_ids.csv")

#extract unique spotify IDs
unique_spotify_id_export <- charts_with_spotify_ids %>%
  distinct(spotify_track_id, .keep_all = TRUE)

#save the dataset
write.csv(unique_spotify_id_export, "//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Khwan_Kim/2025_10_chart_ids_spotify_export.csv")
