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

##### -------------- preamble to joining: generating the dataframes that show which songs did and didn't match ----------------- #####


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



##### -------------- 05-23: unmatched hot100 - based on artists to artist mbid in musicbrainz v2 ------------------------- ####

# Function to convert dates to the "yyyy" format
convert_to_yyyy <- function(x) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x) || grepl("^\\d{4}-\\d{2}$", x)) {
    return(substr(x, 1, 4))
  } else if (grepl("^\\d{4}$", x)) {
    return(x)
  } else if (x == "") {
    return(NA)
  } else {
    return("unknown")
  }
}

# Convert all non-blank columns into a "yyyy" date format
df_musicbrainz_v2 <- df_musicbrainz_v2 %>%
  mutate(release_year = sapply(release_date, convert_to_yyyy))

# Create the song_mbid + release_mbid + title
df_musicbrainz_v2$song_release_mbid <- paste0(df_musicbrainz_v2$song_mbid,"_",df_musicbrainz_v2$release_mbid,"_", df_musicbrainz_v2$song_title)

# remove duplicates
# First, get the distinct rows based on 'song_release_mbid' column
df_musicbrainz_v2_unique_pre <- df_musicbrainz_v2 %>%
  distinct(song_release_mbid)

# Next, join the distinct rows back to the original data frame
df_musicbrainz_v2_unique <- df_musicbrainz_v2_unique_pre %>%
  left_join(df_musicbrainz_v2, by = "song_release_mbid")

# make lower case
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)
df_musicbrainz_v2_unique$artist_no_featuring_lower <- tolower(df_musicbrainz_v2_unique$Artist_no_featuring)

# Define the individual patterns
patterns <- c("featuring", "feat\\.", " ft(?=\\s|\\p{P}|$)", "ft\\.", "feat\\b", "/", ",", "&", "\\+", "with", ":",
              #case specific
              "\"misdemeanor\" ")

# create the "artist no featuring" column for hh - lower necessary to match patterns
## Initialize a new column "artist_no_featuring_lower" with the values from the "artist_lower" column
df_hh_proc$artist_no_featuring_lower <- df_hh_proc$artist_lower

## Loop through the patterns and apply the `str_split()` function for each pattern
for (pattern in patterns) {
  df_hh_proc <- df_hh_proc %>%
    mutate(artist_no_featuring_lower = str_split(artist_no_featuring_lower, pattern, simplify = TRUE)[, 1])
}

## Remove trailing spaces from the "artist_no_featuring_lower" column
df_hh_proc <- df_hh_proc %>%
  mutate(artist_no_featuring_lower = str_trim(artist_no_featuring_lower, side = "right"))

# Remove duplicates in the "df_musicbrainz_v2_unique" dataframe based on the "artist_no_featuring_lower" column
df_musicbrainz_v2_distinct <- df_musicbrainz_v2_unique %>%
  distinct(artist_no_featuring_lower, .keep_all = TRUE)

# get the mbids matched to artist names of df_hh_proc

df_hh_proc <- df_hh_proc %>%
  left_join(df_musicbrainz_v2_distinct[,c("artist_mbid", "artist_no_featuring_lower")],
            by="artist_no_featuring_lower", 
            suffix = c("", "_musibrainz"))

#get the hot100 antijoin based on MBID
df_hh_mbid_no_match <- df_hh_proc %>%
  anti_join(df_musicbrainz_v2_distinct[,c("artist_mbid", "artist_no_featuring_lower")],
            by="artist_no_featuring_lower", 
            suffix = c("", "_musibrainz"))

# write it to data
write.csv(df_hh_mbid_no_match, here::here("data", "incidental", "df_hh_mbid_no_match.csv"))

#### ----------------- 23-03:hot100_titles_partial_artists - creating left joined dataset (HH and MB) with fuzzy track join ------------------------####

#..................
# partial matching
#.................

# version 3 - try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package
## results in only less matches than there are observations in MB, but there are matches without any distance - I don't understand at all

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) 

hot100_titles_partial_3 <- stringdist_left_join(df_hh_proc,
                                                df_musicbrainz_distinct,
                                                by = "track_lower_no_brackets_schar_ws", 
                                                method = "lv", distance_col = "distance",max_dist = 2) 

# check the unmatched examples and by filtering by (is.na(distance))

hh_unmatched_fuzzy_join_song <- hot100_titles_partial_3 %>% filter(is.na(distance) == T)

# remove all the superfluous columns - use code below

var_relevance <- c("Artist",
                   "Track", 
                   "artist_last_word",
                   "recording.artist.credit.phrase",
                   "recording.title",
                   "track_lower.y",
                   "track_lower.x",
                   "artist_lower.y",
                   "artist_lower.x",
                   "artist_lower_no_bracket_schar.y",
                   "artist_lower_no_bracket_schar.x",
                   "track_lower_no_brackets_schar_ws.y",
                   "track_lower_no_brackets_schar_ws.x",
                   "recording.id")

hot100_titles_partial <- hot100_titles_partial_3 %>%
  dplyr::select(all_of(var_relevance))


# remove all observations where the "artist_last_name" doesn't match for .x and .y - use code from below

if (!require(stringi)) install.packages("stringi"); library(stringi)

# row by row, via sapply
hot100_titles_partial_artists <- hot100_titles_partial[sapply(seq_len(nrow(hot100_titles_partial)), 
                                                              
                                                              # create the (anonymous) function
                                                              
                                                              function(i) grepl(paste0("\\b",
                                                                                       
                                                                                       # change the "artist_last_word" to lower letters
                                                                                       
                                                                                       tolower(hot100_titles_partial[i,
                                                                                                                     "artist_last_word"]),
                                                                                       "\\b"),
                                                                                
                                                                                #retain only observations which are contained within the column below
                                                                                
                                                                                stri_trans_general(hot100_titles_partial[i,"artist_lower.y"],
                                                                                                   "latin-ascii"))),]


# do an antijoin between df_hh_proc and the df from above to see which observations didn't match properly. Also, spotchecks for:

hot100_nomatch_titles_partial_artists <- df_hh_proc %>% anti_join(hot100_titles_partial_artists, by = "Track") 

#.....................
# the checks
#.....................



# antijoin between the two antijoins - what is new

# antijoin_titles_artists_partial <- hot100_nomatch_titles_partial_artists %>% anti_join(hh_unmatched_fuzzy_join_song, by = "Track")

# do an antijoin between the hot100_nomatch_titles_nobrackets and the new dataframe - check if the songs were erroneously matched

# antijoin_titles_artists_nobrackets <- hot100_nomatch_titles_nobrackets %>% anti_join(hot100_titles_partial_artists, "Track")

#antijoin between the two antijoins
# antijoin_antijoins_nobrackets_partial <- hot100_nomatch_titles_partial_artists %>% anti_join(antijoin_titles_artists_nobrackets, "Track")

#random checks for correct matches
random_sample_100 <- hot100_titles_partial_artists[sample(nrow(hot100_titles_partial_artists), 100), ]

# first trial, 3% mismatches

#create a table that shows the entries with the most duplicate matches

frequency_table_songs_mb <- hot100_titles_partial_artists %>%
  group_by(recording.title, recording.artist.credit.phrase) %>%
  summarize(number = n()) %>%
  arrange(desc(number))

#.............................
# saving the dataframes
#.............................

#save the dataframe
write.csv(hot100_titles_partial_artists,
          here("C:/R work/Research/music_data/data/interim_data/df_hh_and_mb_leftjoin_fuzzy.csv"),
          row.names=FALSE)


write.csv(hot100_nomatch_titles_partial_artists,
          here("C:/R work/Research/music_data/data/interim_data/HH_NAs.csv"),
          row.names=FALSE)


write.csv(frequency_table_songs_mb,
          here("C:/R work/Research/music_data/data/interim_data/mb_songs_multiple_matches.csv"),
          row.names=FALSE)


##### --------------- RETIRED: left join based joining procedure (rather than fuzzy joining) --------------- #####

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

