#.............................
# Author: Alexander Staub
# Description: Scripts for creating (and exporting) processed dataframes - for further analysis or processing
#.............................

####----------------------- musicbrainz_exp: prepare a csv in order to collect additional song data ------#### 

# limit to period of relevance

if (!require(lubridate)) install.packages("lubridate"); library(lubridate) # package for dealing with dates

df_songs_relevant_time_period <- df %>%
  filter(year(Week) >= 1999 & year(Week) < 2005) %>%
  group_by(Artist, Track) %>%
  summarise(count = n())

# create the column of song names without any bracket information

df_songs_relevant_time_period$Track_no_brackets <- gsub("\\(.*\\)", "", df_songs_relevant_time_period$Track)

# check for any further special characters

df_spec_char <- df_songs_relevant_time_period %>% filter(str_detect(Track_no_brackets, "\\W"))


# remove if necessary

# add a column with only the first word of the artist

df_songs_relevant_time_period$artist_first_word <- str_extract(df_songs_relevant_time_period$Artist,
                                                               "\\b\\w+\\b")

# add a column with only the last word of the artist

df_songs_relevant_time_period$artist_last_word <- str_extract(df_songs_relevant_time_period$Artist,
                                                               "\\b\\w+\\b$")

#replace NAs with the value from the other column

df_songs_relevant_time_period$artist_last_word <- ifelse(is.na(df_songs_relevant_time_period$artist_last_word) == T,
                                                          df_songs_relevant_time_period$Artist,
                                                          df_songs_relevant_time_period$artist_last_word)

# Export the new df as a csv to the path "data", "processed"

write.csv(df_songs_relevant_time_period,
          here("C:/R work/Research/music_data/data/interim_data/df_songs_relevant_time.csv"), row.names=FALSE)

##### -------------- joined_dfs: generating the dataframes that show which songs did and didn't match ----------------- #####


# remove all duplicate rows
df_musicbrainz_distinct <- unique(df_musicbrainz[,2:4])

## make all columns all lowercase
# hot100
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)

# musicbrainz
df_musicbrainz_distinct$track_lower <- tolower(df_musicbrainz_distinct$recording.title)
df_musicbrainz_distinct$artist_lower <- tolower(df_musicbrainz_distinct$recording.artist.credit.phrase)

##track based alterations

# remove all song info in brackets
df_musicbrainz_distinct$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_distinct$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all special characteristics 
df_musicbrainz_distinct$track_lower_no_schar <- gsub("[^a-zA-Z0-9 ]", " ", df_musicbrainz_distinct$track_lower_no_brackets)
df_hh_proc$track_lower_no_schar <- str_replace_all(df_hh_proc$track_lower_no_brackets, "[^[:alnum:]]", " ")


# remove all the whitespace from the no schar column in both df_s to allow for partial matches
df_musicbrainz_distinct$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_musicbrainz_distinct$track_lower_no_schar)
df_hh_proc$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_schar)

# the same, no white space but excluding anything in brackets
df_musicbrainz_distinct$track_lower_no_brackets_ws <- gsub("\\s+", "", df_musicbrainz_distinct$track_lower_no_brackets)
df_hh_proc$track_lower_no_brackets_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_brackets)

##artist based alterations

#remove the bracket info from names
df_musicbrainz_distinct$artist_lower_no_bracket <- gsub("\\(.*\\)", "", df_musicbrainz_distinct$artist_lower)
df_hh_proc$artist_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$artist_lower)

#remove the special characteristics
df_musicbrainz_distinct$artist_lower_no_bracket_schar <- gsub("[^[:alnum:]]", "", df_musicbrainz_distinct$artist_lower_no_bracket)
df_hh_proc$artist_lower_no_brackets_schar <- gsub("[^[:alnum:]]", "", df_hh_proc$artist_lower_no_brackets)

#remove any white space
df_musicbrainz_distinct$artist_lower_no_bracket_schar_ws <- gsub("\\s+", "", df_musicbrainz_distinct$artist_lower_no_bracket_schar)
df_hh_proc$artist_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$artist_lower_no_brackets_schar)

# artist column where all feat is replaced with featuring



# Combine the track and artist columns into one column
df_musicbrainz_distinct$tracks_artists <- paste(df_musicbrainz_distinct$track_lower_no_brackets_ws,
                                                df_musicbrainz_distinct$artist_lower_no_bracket_schar_ws,
                                                sep = " ")

df_hh_proc$tracks_artists <- paste(df_hh_proc$track_lower_no_brackets_ws,
                                   df_hh_proc$track_lower_no_brackets_schar_ws,
                                   sep = " ")





#.........................................................
# merging the tables to contain the info from both tables
#.........................................................

# leftjoin of the two tables
df_hh_and_mb_songs_left <- left_join(df_hh_proc,
                                     df_musicbrainz_distinct,
                                     by = "track_lower_no_brackets_schar_ws")


# clean up the dataframes above

# variable definition
var_relevance <- c("Artist",
                   "Track", 
                   "recording.artist.credit.phrase",
                   "recording.title",
                   "track_lower.y",
                   "track_lower.x",
                   "artist_lower.y",
                   "artist_lower.x",
                   "track_lower_no_brackets_schar_ws",
                   "recording.id")

# left join

df_hh_and_mb_leftjoin_clean <- df_hh_and_mb_songs_left %>%
  dplyr::select(all_of(var_relevance))


# for both of the above, there are matches on songs with the same name but with different artists.

# do the first and last word adjustments

df_hh_and_mb_leftjoin_clean <- df_hh_and_mb_leftjoin_clean %>%
  mutate(artist_lower_first.x = str_extract(artist_lower.x,"\\b\\w+\\b"),
         artist_lower_first.y = str_extract(artist_lower.y, "\\b\\w+\\b"),
         artist_last_word.x = str_extract(artist_lower.x, "\\b\\w+\\b$"),
         artist_last_word.y = str_extract(artist_lower.y, "\\b\\w+\\b$"))

# make sure that the match is based on the artist last word is present in the artsit of mb and beyonce matches to beyoncé

if (!require(stringi)) install.packages("stringi"); library(stringi)

df_hh_and_mb_leftjoin_clean_distinct <- df_hh_and_mb_leftjoin_clean %>% 
  group_by(artist_last_word.x) %>% 
  mutate(artist_last_word.y_trans = stri_trans_general(artist_last_word.y, "latin-ascii")) %>%
  filter(grepl(artist_last_word.x, artist_last_word.y_trans, ignore.case = TRUE))

#get the new non-matching thing

hot100_nomatch_titles_artists <- df_hh_proc %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct, by = "track_lower_no_brackets_schar_ws")

mb_nomatch_titles_artists <- df_musicbrainz_distinct %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct, by = "track_lower_no_brackets_schar_ws")

#...................................................................
# create the table that shows the songs with multiple matches in MB
#....................................................................

#create a table that shows the entries with the most duplicate matches

frequency_table_songs_mb <- df_hh_and_mb_leftjoin_clean_distinct %>%
  group_by(recording.title, recording.artist.credit.phrase) %>%
  summarize(number = n()) %>%
  arrange(desc(number))

#.............................
# saving the dataframes
#.............................

#save the dataframe
write.csv(df_hh_and_mb_leftjoin_clean_distinct,
          here("C:/R work/Research/music_data/data/interim_data/df_hh_and_mb_leftjoin_clean.csv"),
          row.names=FALSE)


write.csv(hot100_nomatch_titles_artists,
          here("C:/R work/Research/music_data/data/interim_data/HH_NAs.csv"),
          row.names=FALSE)

write.csv(mb_nomatch_titles_artists,
          here("C:/R work/Research/music_data/data/interim_data/MB_NAs.csv"),
          row.names=FALSE)

write.csv(frequency_table_songs_mb,
          here("C:/R work/Research/music_data/data/interim_data/mb_songs_multiple_matches.csv"),
          row.names=FALSE)

