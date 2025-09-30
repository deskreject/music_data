#.............................
# Author: Alexander Staub
# Description: examining the spotify responses to 2nd spotify scrape (more flexible)
# Date created: 30-09-2025
# dependencies:
## 
#.............................


# loading packages
library(readr)
library(here)
library(dplyr)

# load in the datasets

first_sp_fetch <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Spotify/1980_2000_songs_artists/final_charted_from_spotify_1980_2000.csv")
second_sp_fetch <- read.csv("//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/Spotify/1980_2000_songs_artists/all_charted_with_spotify_info_till_step_4_dedup.csv")

#check the second fetch
glimpse(second_sp_fetch)

glimpse(first_sp_fetch)


#filter down to the observations with a spotify track_id
second_sp_fetch_filtered <- second_sp_fetch %>%
  filter(spotify_track_title != "")

# do an antijoin between first and second sp_fetch based on "spotify_track_id"

antijoin_first <- first_sp_fetch %>%
  anti_join(second_sp_fetch_filtered, by = "spotify_track_id")

antijoin_second <- second_sp_fetch_filtered %>%
  anti_join(first_sp_fetch, by = "spotify_track_id")

#create the antion_second just with the variables of relevance
antijoin_second_inspect <- antijoin_second %>%
  dplyr::select(name_recording,
     spotify_track_title,
    name_artist_credit,
  spotify_artist_name) %>%
  # filter out all the observations where name_artist_credit and spotify_artist_name are the same
  filter(name_artist_credit != spotify_artist_name)

# take a random 100 unit subset from the antijoin_second_inspect
random_antijoin_second <- antijoin_second_inspect[sample(nrow(antijoin_second_inspect), 100), ]


