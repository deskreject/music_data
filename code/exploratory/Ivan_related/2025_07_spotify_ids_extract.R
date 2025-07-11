#.............................
# Author: Alexander Staub
# Description: Script to extract the spotify IDs for Ivan
# Date created: 06-07-2025
# dependencies:
## 
#.............................


# loading packages
library(readr)
library(dplyr)
library(here)

#data directory
data_dir <- ("//bigdata.wu.ac.at/delpero/Data_alexander/data/")

#load in the musicbrainz data with the spotify IDs

musicbrainz_data <- read_csv(paste0(data_dir, "raw_data/Spotify/1980_2000_songs_artists/musicbrainz_spotify_combined_track_artist_final.csv"))

#load in the charts data:
# run 1 - the whole charts
chart_songs <- read_csv(paste0(data_dir, "raw_data/Spotify/1980_2000_songs_artists/final_charted_from_spotify_1980_2000.csv"))

#extract the spotify IDs from both
spotify_ids_musicbrainz <- musicbrainz_data %>% 
  dplyr::select(spotify_track_id)

spotify_ids_charts <- chart_songs %>%
  dplyr::select(spotify_track_id)


#row_bind them and remove the duplicates
spotify_ids_combined <- bind_rows(spotify_ids_musicbrainz, spotify_ids_charts) %>%
  distinct()

#save the combined spotify ids
write_csv(spotify_ids_combined, 
          paste0(data_dir, "incidental/spotify_related/2025_07_ivan_spotify_ids_mb_charts.csv"))
