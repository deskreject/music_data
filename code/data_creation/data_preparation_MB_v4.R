#..............
# Description: Processing of the 4th MB export - aim to get all hh artists' releases (including back catalog)
# Author: Alexander Staub
# Date: 28.07.2023
#.................

#### ------------------------ df_musicbrainz_v4: initial data adjustments --------------------------------- ####

# save an og dataframe
df_musicbrainz_v4_original <- df_musicbrainz_v4

#reduce size of the working dataframe
df_musicbrainz_v4 <- df_musicbrainz_v4_original %>%
  select(`artist mbid`, `song title`, recording_id, artist, artist_credit_phrase, release, country, date, Artist, Artist_no_featuring)

##need to remove the duplicates
#by creating "Artist" + "song title" line and then removing any duplications

df_musicbrainz_v4$artist_songtitle_date <- paste(df_musicbrainz_v4$Artist,"_", df_musicbrainz_v4$`song title`, "_", df_musicbrainz_v4$date)

#remove any duplicates in the newly created combination variable
df_musicbrainz_v4 <- df_musicbrainz_v4 %>%
  distinct(artist_songtitle_date, .keep_all = TRUE)
  

#spot checks on whether I removed something
coldplay <- df_musicbrainz_v4 %>%
  filter(artist == "Coldplay")

#### ------------------------------ df_musicbrainz_v4_relevant: removing dates that are not relevant ------------------------------- ####

# Function to convert dates to the "yyyy" format
convert_to_yyyy <- function(x) {
  #check for NAs first
  if (is.na(x)) {
    return(NA)
    
    # then go through the possible values that may appear in the "date" column
    
  } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x) || grepl("^\\d{4}-\\d{2}$", x)) {
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
df_musicbrainz_v4 <- df_musicbrainz_v4 %>%
  mutate(release_year = sapply(date, convert_to_yyyy))

# remove any observations that fall outside the date range, including NAs
df_musicbrainz_v4_relevant <- df_musicbrainz_v4 %>%
  filter(release_year >= 1998 & release_year < 2005 | is.na(release_year))

#percentage of NAs vs non-nas - 23%
# sum(is.na(df_musicbrainz_v4_relevant$release_year)==T)/291581

#### ---------------------------------- df_musicbrainz_v4_relevant: artist & song column additions and alterations --------------------####

#..............
# artist level
#..............

#MB - make lower and remove any special characters artist
df_musicbrainz_v4_relevant <- df_musicbrainz_v4_relevant %>%
  mutate(artist_no_featuring_lower_no_spec = tolower(Artist_no_featuring) %>%  # Convert to lower case
           stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
           str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
           str_trim())   # Remove leading/trailing whitespace

#hh - make lower artist
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)

##case specfic catches
# Replace ""misdemeanor" " with ""
df_hh_proc$artist_lower <- gsub(pattern = '\"misdemeanor\" ', replacement = '', x = df_hh_proc$artist_lower)


# Define the individual patterns
patterns <- c("featuring", "feat\\.", " ft(?=\\s|\\p{P}|$)", "ft\\.", "feat\\b", "/", ",", "&", "\\+", "with", ":")

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

#remove special chars etc from the artist name
df_hh_proc <- df_hh_proc %>%
  mutate(artist_no_featuring_lower_no_spec = tolower(artist_no_featuring_lower) %>%  # Convert to lower case
           stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
           str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
           str_trim())   # Remove leading/trailing whitespace

#.............
# song level
#.............

# make lower case
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
df_musicbrainz_v4_relevant$track_lower <- tolower(df_musicbrainz_v4_relevant$`song title`)


##track based alterations

#remove the white space - from track
df_musicbrainz_v4_relevant$track_lower_no_ws <- gsub("\\s+", "", df_musicbrainz_v4_relevant$track_lower)

#create the column artists_bid_songtitle
df_musicbrainz_v4_relevant$track_artists_mbid_no_ws <- paste(df_musicbrainz_v4_relevant$track_lower_no_ws,
                                                            df_musicbrainz_v4_relevant$`artist mbid`,
                                                            sep = "_")

# remove all song info in brackets
df_musicbrainz_v4_relevant$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_v4_relevant$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all special characteristics 
df_musicbrainz_v4_relevant$track_lower_no_schar <- gsub("[^a-zA-Z0-9 ]", " ", df_musicbrainz_v4_relevant$track_lower_no_brackets)
df_hh_proc$track_lower_no_schar <- str_replace_all(df_hh_proc$track_lower_no_brackets, "[^[:alnum:]]", " ")


# remove all the whitespace from the no schar column in both df_s to allow for partial matches
df_musicbrainz_v4_relevant$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_musicbrainz_v4_relevant$track_lower_no_schar)
df_hh_proc$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_schar)

#### ---------------------------- hh_mbv4_mbid_match: merging in the songs based on mbid --------------------- ####

# Remove duplicates in the "df_musicbrainz_v2_unique" dataframe based on the "artist_no_featuring_lower" column
df_musicbrainz_v4_distinct <- df_musicbrainz_v4_relevant %>%
  distinct(artist_no_featuring_lower_no_spec, .keep_all = TRUE)

# get the mbids matched to artist names of df_hh_proc

df_hh_proc_v4 <- df_hh_proc %>%
  left_join(df_musicbrainz_v4_distinct[,c("artist mbid", "artist_no_featuring_lower_no_spec")],
            by="artist_no_featuring_lower_no_spec", 
            suffix = c("", "_musibrainz"))

#get the hot100 antijoin based on MBID
df_hh_mbid_no_match_v4 <- df_hh_proc_v4 %>%
  anti_join(df_musicbrainz_v4_relevant[,c("artist mbid", "artist_no_featuring_lower_no_spec")],
            by="artist_no_featuring_lower_no_spec")


#--> reduced non-matches by 18

# spot checks
santana <- df_musicbrainz_v4_original %>%
  filter(str_detect(str_to_lower(Artist), "santana")) %>%
  distinct(`song title`, .keep_all = T)

good_charlotte <- df_musicbrainz_v4_original %>%
  filter(str_detect(str_to_lower(Artist), "charlotte")) %>%
  distinct(`song title`, .keep_all = T)

# save relevant dataframes to share
write.csv(df_hh_mbid_no_match_v4, here::here("data", "incidental", "hot100_nomatch_artists_v4.csv"))

#####---------------------- song-based matching: partial matching based on the process from MB1 data--------------####


# try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) 

hot100_titles_partial_v4 <- stringdist_left_join(df_hh_proc_v4,
                                                 df_musicbrainz_v4_relevant,
                                                 by = "track_lower_no_brackets_schar_ws", 
                                                 method = "lv", distance_col = "distance",
                                                 max_dist = 2) 

# check the unmatched examples and by filtering by (is.na(distance))

hh_unmatched_fuzzy_join_song_v4 <- hot100_titles_partial_v4 %>% filter(is.na(distance) == T)

