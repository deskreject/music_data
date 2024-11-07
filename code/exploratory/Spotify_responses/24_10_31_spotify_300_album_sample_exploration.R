#.............................
# Author: Alexander Staub
# Description: Looking into false positives and negatives of the spotify response to the 300 album sample
# Date created: 31.10.2024
# dependencies:
## 
#.............................


# loading packages
library(jsonlite)

#### --------------------------------- importing data --------------------------------- ####

# import the csv, with all columns as strings 
spotify_response_300_songs <- read.csv("Z:/Data_alexander/data/interim_data/random_300_album_recording_samples/24_10_30_spotify_response_300_albums_AD.csv",
                                       stringsAsFactors = FALSE)

spotify_response_300_albums <- read.csv("Z:/Data_alexander/data/interim_data/random_300_album_recording_samples/24_11_07_spotify_response_300_albums_AD.csv")

#### --------------------------------- Data cleaning - album level ------------------------------------------- ####

# limit to the release ID only in order to get an overview of album level matches + count number of songs
spotify_response_300_albums_filtered <- spotify_response_300_albums %>% 
  select(artist_credit_name, name_release,
         spotify_artist_name, spotify_album_name) %>% 
  group_by(artist_credit_name, name_release, spotify_artist_name, spotify_album_name, id_release) %>% 
  summarise(song_count = n())

#get a list of only albums from the songs dataset in order to find unmatched albums
albums_musicbrainz <- spotify_response_300_songs %>% 
  group_by(artist_credit_name, name_release) %>% 
  summarise(song_count = n())

#create an antijoin between the two to find the unmatched albums
unmatched_albums <- anti_join(albums_musicbrainz, spotify_response_300_albums_filtered, by = c("name_release"))


#### --------------------------------- OLD VERSION - data cleaning - album level --------------------------------- ####

#replace "" with NA in spotify_response_300_albums

spotify_response_300_albums <- as.data.frame(apply(spotify_response_300_albums, 2, function(x) ifelse(x == "", NA, x)))

#keep only distinct values of columne "album_data"
spotify_album_data <- spotify_response_300_albums %>% 
  filter(is.na(album_data) == FALSE) %>%
  select(name_release, album_data)

#create the album matching data:
albums_spotify_response <- spotify_response_300_albums %>% 
  group_by(artist_credit_name, name_release) %>% 
  summarise(song_count = n())

#merge in the "album_data" column based on the "name_release" field
albums_spotify_response <- left_join(albums_spotify_response,
                                     spotify_album_data,
                                     by = c("name_release" = "name_release"))

#create the function to extract the relevant artist credit data

# Function to process each album_data entry
process_album_data <- function(album_data_str) {
  # Check for NA or empty strings
  if (is.na(album_data_str) || album_data_str == "") {
    return(list(artist_credit_name_spotify = NA, number_tracks_spotify = NA))
  } else {
    # Replace single quotes with double quotes for valid JSON
    album_data_json_str <- gsub("'", "\"", album_data_str)
    # Remove any backslashes
    album_data_json_str <- gsub("\\\\", "", album_data_json_str)
    
    # Parse JSON string safely
    parsed_data <- tryCatch({
      fromJSON(album_data_json_str)
    }, error = function(e) {
      return(list(artist_credit_name_spotify = NA, number_tracks_spotify = NA))
    })
    
    # Check if 'album_tracks' is present and non-empty
    if (!is.null(parsed_data$album_tracks) && length(parsed_data$album_tracks) > 0) {
      tracks <- parsed_data$album_tracks
      number_tracks <- length(tracks)
      
      # Handle different possible structures of 'tracks'
      if (is.data.frame(tracks)) {
        # If 'tracks' is a data frame
        artist_names <- unique(tracks$spotify_artist_name)
      } else if (is.list(tracks)) {
        # If 'tracks' is a list
        artist_names <- sapply(tracks, function(track) {
          if (is.list(track)) {
            # If 'track' is a list
            track[["spotify_artist_name"]]
          } else if (is.atomic(track)) {
            # If 'track' is an atomic vector
            track["spotify_artist_name"]
          } else {
            NA
          }
        })
        artist_names <- unique(artist_names)
      } else {
        artist_names <- NA
      }
      
      # Return list of unique artist names and track count
      return(list(artist_credit_name_spotify = artist_names, number_tracks_spotify = number_tracks))
    } else {
      return(list(artist_credit_name_spotify = NA, number_tracks_spotify = NA))
    }
  }
}

# Apply the function to each element in the 'album_data' column
results <- map(albums_spotify_response$album_data, process_album_data)

# Extract the results into new dataframe columns
albums_spotify_response$artist_credit_name_spotify <- I(lapply(results, function(res) res$artist_credit_name_spotify))
albums_spotify_response$number_tracks_spotify <- sapply(results, function(res) res$number_tracks_spotify)