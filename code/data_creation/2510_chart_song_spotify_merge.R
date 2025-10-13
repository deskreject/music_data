#.............................
# Author: Alexander Staub
# Description: merging the spotify IDs to the chart data
# Date created: 13.10.2025
# dependencies: 
## 
#.............................


# loading packages
library(readr)
library(here)
library(dplyr)

#loading in the datasets

first_sp_fetch <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Spotify/1980_2000_songs_artists/final_charted_from_spotify_1980_2000.csv")
second_sp_fetch <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Spotify/1980_2000_songs_artists/all_charted_with_spotify_info_till_step_8_dedup_with_IDs_cleaned.csv")

charts_data <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Spotify/1980_2000_songs_artists/all_charts_songs_1980_2000_complete_clean.csv")


#filter out the second fetch data to only include observations with valid spotify IDs

second_sp_fetch_filtered <- second_sp_fetch %>%
  filter(spotify_track_title != "")

#create the merging column
first_sp_fetch <- first_sp_fetch %>%
  mutate(merge_col_song_artist = paste(name_recording, name_artist_credit, sep = "_"))

second_sp_fetch <- second_sp_fetch_filtered %>%
  mutate(merge_col_song_artist = paste(name_recording, name_artist_credit, sep = "_"))

charts_data <- charts_data %>%
  mutate(merge_col_song_artist = paste(name_recording, name_artist_credit, sep = "_"))

#collect the duplicates of the merge column
duplicates_merge_col_second <- second_sp_fetch %>%
  group_by(merge_col_song_artist) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(merge_col_song_artist)

duplicates_merge_col_first <- first_sp_fetch %>%
  group_by(merge_col_song_artist) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(merge_col_song_artist)

duplicates_merge_col_charts <- charts_data %>%
  group_by(merge_col_song_artist) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(merge_col_song_artist)


