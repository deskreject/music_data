#......................
# Description: Aimed at checking whether the results of the spotify api access worked better than the last try
# author: Alexander Staub
# date creation: 27.11.2023
#...............................

spotify_audio_characteristics_df <- spotify_audio_characteristics_df %>%
  rename(Spotify.ID = `Spotify ID`)

# Assuming spotify_audio_characteristics_df is already loaded and each element is a list, change the types
spotify_audio_characteristics_df[,c("ISRC",
                                    "Spotify.ID",
                                    "Title",
                                    "Artist",
                                    "id",
                                    "uri",
                                    "track_href")] <- as.character(spotify_audio_characteristics_df[,c("ISRC",
                                                                                                       "Spotify.ID",
                                                                                                       "Title",
                                                                                                       "Artist",
                                                                                                       "id",
                                                                                                       "uri",
                                                                                                       "track_href")])

# investigate the data
glimpse(spotify_audio_characteristics_df)

#check for NAs - none
colSums(is.na(spotify_audio_characteristics_df))

# unique values of the columns
apply(spotify_audio_characteristics_df, 2, function(x){length(unique(x))})

#get the left join of artist names and song titles
mb_spotify_join_unique <- left_join(spotify_audio_characteristics_df, df_musicbrainz_v6_isrcs[,1:4], by=c("ISRC" = "isrc"))%>%
  #remove duplicates
  distinct(Spotify.ID, .keep_all = T)



#make a check between song/artist names
glimpse(mb_spotify_join_unique)

mb_spotify_join_songs_artists <- mb_spotify_join_unique %>%
  select(Title, track_title, Artist, artist_name)

#check - antijoin song name
antijoin_song_names <- anti_join(mb_spotify_join_songs_artists, mb_spotify_join_songs_artists,
                                 by=c("Title" = "track_title"))

#check - antijoin artist name
antijoin_artist_names <- anti_join(mb_spotify_join_songs_artists, mb_spotify_join_songs_artists,
                                 by=c("Artist" = "artist_name"))

####------------------ Plotting data -----------------------------####


# pivotting data
long_df <- pivot_longer(spotify_acoustic_char_v2,
                        cols = 3:13,
                        names_to = "variable",
                        values_to = "value")


#plot histograms
P

#### ------------------- save subset of data to investigate the spotify id ----------####

#get a random sample of rows from the test data
random_isrcs <- unique_isrcs[sample(nrow(unique_isrcs), 10),]

#filter the df by the randomly selected ISRCs
sample_spotify <- spotify_acoustic_char_v2 %>%
  filter(ISRC %in% random_isrcs$ISRC)

# just choose ISRC and the ID column
sample_spotify <- sample_spotify[,1:2]

# rename the column to exclude space
sample_spotify <- sample_spotify %>%
  rename(spotify_ID = `Spotify ID`)

# save the randome sample
write.csv(sample_spotify, here::here("data", "incidental", "spotify_related", "random_sample_ISRC_URI.csv"))
