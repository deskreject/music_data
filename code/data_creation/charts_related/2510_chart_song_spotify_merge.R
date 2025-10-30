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



#### ----------- joining the spotify IDs to the charts data -------------------- #####

second_sp_fetch_join <- second_sp_fetch %>% 
  dplyr::select(spotify_track_title,
                spotify_artist_name,
                spotify_track_id,
                spotify_url,
                merge_col_song_artist)

first_sp_fetch_join <- first_sp_fetch %>%
  dplyr::select(spotify_track_title,
                spotify_artist_name,
                spotify_track_id,
                spotify_url,
                merge_col_song_artist)

# left join the second_sp_fetch_join observations to charts_data based on merge_col_song_artist

charts_data_joined_second <- charts_data %>%
  left_join(second_sp_fetch_join, by = "merge_col_song_artist", suffix = c("", "_second"))

# create two datasets - one with spotify IDs and one without

charts_data_no_id <- charts_data_joined_second %>% 
  filter(spotify_track_id == "" | is.na(spotify_track_id))

charts_data_with_id_second <- charts_data_joined_second %>%
  filter(spotify_track_id != "" & !is.na(spotify_track_id))


# left join the first_sp_fetch_join observations to charts_data_no_id based on merge_col_song_artist

charts_data_joined_first <- charts_data_no_id %>%
  # remove the columns spotify_track_title spotify_artist_name spotify_track_id spotify_url from "charts_data_no_id" 
  dplyr::select(-spotify_track_title, -spotify_artist_name, -spotify_track_id, -spotify_url) %>%
  left_join(first_sp_fetch_join, by = "merge_col_song_artist", suffix = c("", "_first"))

# create two datasets - one with spotify IDs and one without for the joined_first dataset

charts_data_no_id_first <- charts_data_joined_first %>% 
  filter(spotify_track_id == "" | is.na(spotify_track_id))

charts_data_with_id_first <- charts_data_joined_first %>%
  filter(spotify_track_id != "" & !is.na(spotify_track_id))

# merege charts_data_no_id_first, charts_data_with_id_first and chrats_data_with_id_second to one dataset by row
chart_data_with_spotify_ids <- bind_rows(charts_data_no_id_first, charts_data_with_id_first, charts_data_with_id_second)

#### ------------ Save the resulting file ------####

write.csv(chart_data_with_spotify_ids, "//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/country_chart_data/all_charts_songs_1980_2000_spotify_ids.csv")

#### ------------ CHECKS -----------------------####

####
#CHECK: collect the duplicates of the merge column
####

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

####Random checks

# draw a random sample of 100 observations from the chart_data_with_spotify_ids dataframe
random_data <- chart_data_with_spotify_ids %>%
  sample_n(100)

      



