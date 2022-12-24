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