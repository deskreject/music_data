#.............................
# Author: Alexander Staub
# Description: Looking into false positives and negatives of the spotify response to artist+song_name and artist+release searches
# Date created: 05-06-2025
# dependencies:
## 
#.............................


# loading packages
library(readr)
library(here)

#load the datsets of relevance
spotify_aritst_songs_df <- read_csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/musicbrainz_data_refined/for_spotify_track_artist/musicbrainz_spotify_combined_track_artist.csv")

spotify_artist_release_df <- read_csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/musicbrainz_data_refined/for_spotify_info_artist_album/musicbrainz_combined.csv")

#create 4 random sample dataframes of 100 observations from the artist_song_df
random_song_artist_1 <- spotify_aritst_songs_df[sample(nrow(spotify_aritst_songs_df), 100), ]
random_song_artist_2 <- spotify_aritst_songs_df[sample(nrow(spotify_aritst_songs_df), 100), ]
random_song_artist_3 <- spotify_aritst_songs_df[sample(nrow(spotify_aritst_songs_df), 100), ]
random_song_artist_4 <- spotify_aritst_songs_df[sample(nrow(spotify_aritst_songs_df), 100), ]

# create 4 random sample dataframes of the 100 observations from the artist_release_df
random_artist_release_1 <- spotify_artist_release_df[sample(nrow(spotify_artist_release_df), 100), ]
random_artist_release_2 <- spotify_artist_release_df[sample(nrow(spotify_artist_release_df), 100), ]
random_artist_release_3 <- spotify_artist_release_df[sample(nrow(spotify_artist_release_df), 100), ]
random_artist_release_4 <- spotify_artist_release_df[sample(nrow(spotify_artist_release_df), 100), ]

#save them all as csv files
write_csv(random_song_artist_1, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_song_artist_1.csv")
write_csv(random_song_artist_2, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_song_artist_2.csv")
write_csv(random_song_artist_3, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_song_artist_3.csv")
write_csv(random_song_artist_4, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_song_artist_4.csv")
write_csv(random_artist_release_1, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_artist_release_1.csv")
write_csv(random_artist_release_2, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_artist_release_2.csv")
write_csv(random_artist_release_3, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_artist_release_3.csv")
write_csv(random_artist_release_4, "Z:/Data_alexander/data/incidental/spotify_related/2025_06_random_artist_release_4.csv")
