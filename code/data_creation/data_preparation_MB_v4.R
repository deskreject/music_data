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
  filter(release_year >= 1998 & release_year <= 2005 | is.na(release_year))

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

# Remove duplicates in the "df_musicbrainz_v2_unique" dataframe based on the "artist_no_featuring_lower" and mbid column
df_musicbrainz_v4_distinct_temp <- df_musicbrainz_v4_relevant %>%
  group_by(artist_no_featuring_lower_no_spec, `artist mbid`) %>%
  summarise(count = n())

# create the new "discinct" dataframe that allows for nested mbids within each observation
df_musicbrainz_v4_distinct <- df_musicbrainz_v4_distinct_temp %>%
  group_by(artist_no_featuring_lower_no_spec) %>%
  summarise(`artist mbid` = unique(`artist mbid`))

#create a nested list, that allows for a "artist_no_featuring" entry to have multiple mbids

# get the mbids matched to artist names of df_hh_proc

df_hh_proc_v4 <- df_hh_proc %>%
  left_join(df_musicbrainz_v4_distinct, by = "artist_no_featuring_lower_no_spec")

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
write.csv(df_hh_mbid_no_match_v4, here::here("data", "incidental","fourth_mb_export", "hot100_nomatch_artists_v4.csv"))

#####---------------------- hot100_titles_partial_artists_mbids_v4: song-based partial matching based on the process from MB1 data--------------####


# try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) 

hot100_titles_partial_v4 <- stringdist_left_join(df_hh_proc_v4,
                                                 df_musicbrainz_v4_relevant,
                                                 by = "track_lower_no_brackets_schar_ws", 
                                                 method = "lv", distance_col = "distance",
                                                 max_dist = 2) 

# check the unmatched examples and by filtering by (is.na(distance))

hh_unmatched_fuzzy_join_song_v4 <- hot100_titles_partial_v4 %>% filter(is.na(distance) == T)

#change the name of the "song title" column so I can retain it
hot100_titles_partial_v4 <- hot100_titles_partial_v4 %>%
  mutate(song_title = `song title`)

#retain only the variables of relevance
var_relevance_v4 <- c("Artist.x",
                      "Artist.y",
                      "Track",
                      "release_year",
                      "Artist_no_featuring",
                      "song_title",
                      "artist_no_featuring_lower_no_spec.x",
                      "artist_no_featuring_lower_no_spec.y",
                      "track_lower_no_brackets.x",
                      "track_lower_no_brackets.y",
                      "track_lower_no_brackets_schar_ws.x",
                      "track_lower_no_brackets_schar_ws.y",
                      "artist mbid.x",
                      "artist mbid.y",
                      "recording_id",
                      "release",
                      "country",
                      "date"
                      #"song_release_mbid"
)

hot100_titles_partial_v4 <- hot100_titles_partial_v4 %>%
  dplyr::select(all_of(var_relevance_v4))

# keep only rows where the artist_mbid.x matches artist_mbid.y

## old - non-list based artist mbid.x column
hot100_titles_partial_artists_mbids_v4 <- subset(hot100_titles_partial_v4, `artist mbid.x` == `artist mbid.y`)

# do an antijoin between df_hh_proc and the df from above to see which observations didn't match properly.

hot100_nomatch_titles_partial_artists_v4 <- df_hh_proc_v4 %>% anti_join(hot100_titles_partial_artists_mbids_v4, by = "Track") 

#total number of unmatched artists (no featuring).
hot100_nomatch_artists_v4 <- hot100_nomatch_titles_partial_artists_v4 %>%
  group_by(artist_no_featuring_lower) %>%
  summarise(count = n())

#distinct hh artists
hh_distinct_artists <- df_hh_proc %>% 
  distinct(Artist)

#quickcheck antijoin between this and previous
## added in 5 previously unmatched tracks, one of which is "the roots" one

###### ------------- song based matching - write the datasets ------------------------------------------------------------------------####

#the matched dataset
write.csv(hot100_titles_partial_artists_mbids_v4, here::here("data", "incidental","fourth_mb_export", "hot100_titles_partial_artists_mbids_v4.csv"))

#the unmatched songs
write.csv(hot100_nomatch_titles_partial_artists_v4, here::here("data", "incidental","fourth_mb_export", "hot100_nomatch_titles_partial_artists_v4.csv"))

#the unmatched artists
write.csv(hot100_nomatch_artists_v4, here::here("data", "incidental","fourth_mb_export", "hot100_nomatch_artists_v4.csv"))

#### ------------- hh_mbv4_mbid_match_window: all songs by hh artists (for remix analysis) ----------------------------------------#####

# match all the unique songs to the hot100 based on the mbid

hh_mbv4_mbid_match <- df_hh_proc_v4 %>% 
  left_join(df_musicbrainz_v4_relevant[,c("Artist_no_featuring",
                                        "song title", 
                                        "recording_id",
                                        "release",
                                        "release_year",
                                        "date",
                                        "artist mbid",
                                        "artist_no_featuring_lower_no_spec")],
            by = "artist_no_featuring_lower_no_spec",
            suffix = c("", "_musibrainz"))

#remove duplicates
hh_mbv4_mbid_match_distinct <- hh_mbv4_mbid_match %>%
  distinct(recording_id, .keep_all = TRUE)

# check which songs from mbv4 didn't find a match
hh_mbv4_mbid_match_antijoin <- df_musicbrainz_v4_relevant %>%
  anti_join(hh_mbv4_mbid_match_distinct, by ="recording_id")

#remove songs before 2008 and after 1994

hh_mbv4_mbid_match_window <- hh_mbv4_mbid_match_distinct %>%
  filter(release_year >= 1994 & release_year <= 2007 | is.na(release_year) == T)

##### ---------------------- hh_mbv4_mbid_match_window: save the file -------------------------------------#####

#save the new "window" based song dataset
write.csv(hh_mbv4_mbid_match_window, here::here("data", "incidental","fourth_mb_export", "hh_mbv4_mbid_match_window.csv"))

######------------------- Spot checks related to multiple mbids related to one artist + multiple artists having 1 mbid ----------------- ######

#spot check - the roots
the_roots_name <- df_musicbrainz_v4_relevant %>% 
  filter(artist_no_featuring_lower_no_spec == "the roots") %>%
  group_by(`artist mbid`, Artist_no_featuring, Artist, artist_credit_phrase, artist, `song title`)%>%
  summarise(count = n())

the_roots_mbid <- df_musicbrainz_v4_relevant %>%
  filter(`artist mbid` == "80b3cf5e-18fe-4c59-98c7-e5bb87210710") %>%
  group_by(Artist, artist, artist_credit_phrase, Artist_no_featuring) %>%
  summarise(count = n())



#spot checks - camron
mbid_camron <- df_musicbrainz_v4_relevant %>%
  filter(`artist mbid` == "bea235a0-db63-44b1-881e-264d25791f5a") %>%
  group_by(Artist, artist, Artist_no_featuring) %>%
  summarise(count = n())

#spot checks - baby
mbid_baby <- df_musicbrainz_v4_relevant %>%
  filter(`artist mbid` == "e6424fa5-08d3-41c6-96f3-8549f733708c") %>%
  group_by(Artist, artist, artist_credit_phrase, Artist_no_featuring) %>%
  summarise(count = n())

name_camron <- df_musicbrainz_v4_relevant %>%
  filter(Artist_no_featuring == "CamRon") %>%
  group_by(`artist mbid`, Artist_no_featuring, Artist, artist_credit_phrase, artist, `song title`) %>%
  summarise(count = n())

name_julez <- df_musicbrainz_v4_relevant %>%
  filter(Artist_no_featuring == "Julez") %>%
  group_by(`artist mbid`, artist) %>%
  summarise(count = n())

#spot checks - n sync
name_nsync <- df_musicbrainz_v4_relevant %>%
  filter(Artist_no_featuring == "N Sync") %>%
  group_by(`artist mbid`) %>%
  summarise(count = n())

#spot check - cher
name_Cher <- df_musicbrainz_v4_relevant %>%
  filter(Artist_no_featuring == "Cher") %>%
  group_by(`artist mbid`) %>%
  summarise(count = n())

#spot checks - multiple mbids, one Artist
multiple_mbid_check <- df_musicbrainz_v4_original %>%
  group_by(`artist mbid`, Artist_no_featuring) %>%
  summarise(count = n()) %>%
  group_by(Artist_no_featuring) %>%
  summarise(count = n())