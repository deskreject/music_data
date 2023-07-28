#..............
# Description: Processing of the first tranch of the 3rd MB export - aim to get all hh artists' releases (including back catalog)
# Author: Alexander Staub
# Date: 19.06.2023
#.................


####----------------- alter the names of the columns so I can reuse the code below without altering --------------####

#change names of the variables for v3 with info to match the code I already produced
df_musicbrainz_v3 <- df_musicbrainz_v3 %>%
  rename("song mbid" = "recording_id",
         "song title" = "title",
         "Artist_no_featuring" = "artist")

#create mbid matching variable - new dataset
df_musicbrainz_v3 <- df_musicbrainz_v3 %>%
  mutate(Artist_no_featuring_lower_no_spec = tolower(Artist_no_featuring) %>%  # Convert to lower case
           stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
           str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
           str_trim())   # Remove leading/trailing whitespace

#create mbid matching variable - old dataset
df_musicbrainz_v3_no_dates <- df_musicbrainz_v3_no_dates %>%
  mutate(Artist_no_featuring_lower_no_spec = tolower(Artist_no_featuring) %>%  # Convert to lower case
           stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
           str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
           str_trim())   # Remove leading/trailing whitespace

#check number of different artist_mbids:

only_mbids <- df_musicbrainz_v3_no_dates %>% 
  group_by(Artist_no_featuring_lower_no_spec, Artist_no_featuring, `artist mbid`) %>%
  summarise(count = n())

#add in the artist mbid from prior dataset without dates

df_musicbrainz_v3 <- left_join(df_musicbrainz_v3, 
                               only_mbids[,c("artist mbid", "Artist_no_featuring_lower_no_spec")],
                               by = "Artist_no_featuring_lower_no_spec")

#check amount of mbids
only_mbids_winfo <- df_musicbrainz_v3%>% 
  group_by(Artist_no_featuring_lower_no_spec, `artist mbid`) %>%
  summarise(count = n())

#check amount of mbids without nas
only_mbids_winfo_no_nas <- df_musicbrainz_v3%>% 
  group_by(Artist_no_featuring_lower_no_spec, `artist mbid`) %>%
  summarise(count = n()) %>%
  filter(is.na(`artist mbid`) == F)


#antijoin
missing_v3_info_artists <- anti_join(only_mbids,
                                     only_mbids_winfo_no_nas,
                                     by = "Artist_no_featuring_lower_no_spec")

#fuzzy matching w 0.2 distance for remainder





#......................
# remove duplication
#.....................


####----------------------------- UNNECESSARY - checking what happens when I remove duplicates ----------------####

# make the songtitle lower

df_musicbrainz_v3$track_lower <- tolower(df_musicbrainz_v3$`song title`)

# remove the white space - from track
df_musicbrainz_v3$track_lower_no_ws <- gsub("\\s+", "", df_musicbrainz_v3$track_lower)

#create the column artists_bid_songtitle
df_musicbrainz_v3$track_artists_mbid_no_ws <- paste(df_musicbrainz_v3$track_lower_no_ws,
                                                    df_musicbrainz_v3$`artist mbid`,
                                                    sep = "_")
# create duplicate less df
df_musicbrainz_v3_no_dupl_me <- df_musicbrainz_v3 %>%
  distinct(track_artists_mbid_no_ws, .keep_all = TRUE)

######-------------------------- Removing further duplication from Alessios DF ------------------#####

#...................................................................
#compare the non-matches between my and Alessio's non-duplicates
#...................................................................

#do the conversions to the track level as before

#make songtitle lower
df_musicbrainz_v3_no_dupl$track_lower <- tolower(df_musicbrainz_v3_no_dupl$`song title`)

#remove the white space - from track
df_musicbrainz_v3_no_dupl$track_lower_no_ws <- gsub("\\s+", "", df_musicbrainz_v3_no_dupl$track_lower)

#create the column artists_bid_songtitle
df_musicbrainz_v3_no_dupl$track_artists_mbid_no_ws <- paste(df_musicbrainz_v3_no_dupl$track_lower_no_ws,
                                                            df_musicbrainz_v3_no_dupl$`artist mbid`,
                                                    sep = "_")

#create the duplicate less df
df_musicbrainz_v3_unique <-df_musicbrainz_v3_no_dupl 
  

#antijoin - my and his - no results
#antijoin - his and his - no results

# show the duplicates - seems to be only duplicates

# df_musicbrainz_v3_no_dupl_duplicates <- df_musicbrainz_v3_no_dupl %>%
  # filter(duplicated(track_artists_mbid_no_ws) | duplicated(track_artists_mbid_no_ws, fromLast = TRUE))

#####------------------------- Pre-processing dfs: Matching between hh_proc and musicbrainz_v3_unique -------------------#####

#...........................
# artist level alterations
#...........................



# make lower case
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)
df_musicbrainz_v3_unique$artist_no_featuring_lower <- tolower(df_musicbrainz_v3_unique$Artist_no_featuring)

##case specfic catches
# Replace ""misdemeanor" " with ""
df_hh_proc$artist_lower <- gsub(pattern = '\"misdemeanor\" ', replacement = '', x = df_hh_proc$artist_lower)


# Define the individual patterns
patterns <- c("featuring", "feat\\.", " ft(?=\\s|\\p{P}|$)", "ft\\.", "feat\\b", "/", ",", "&", "\\+", "with", ":")

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

#........................
# Song level alteration
#.......................

# make lower case
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
# Already done for the mb_v3 data


##track based alterations

# remove all song info in brackets
df_musicbrainz_v3_unique$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_v3_unique$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all special characteristics 
df_musicbrainz_v3_unique$track_lower_no_schar <- gsub("[^a-zA-Z0-9 ]", " ", df_musicbrainz_v3_unique$track_lower_no_brackets)
df_hh_proc$track_lower_no_schar <- str_replace_all(df_hh_proc$track_lower_no_brackets, "[^[:alnum:]]", " ")


# remove all the whitespace from the no schar column in both df_s to allow for partial matches
df_musicbrainz_v3_unique$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_musicbrainz_v3_unique$track_lower_no_schar)
df_hh_proc$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_schar)

#####---------------------- artist-based matching: getting all songs related to an artist ------------------------#####

# Remove duplicates in the "df_musicbrainz_v2_unique" dataframe based on the "artist_no_featuring_lower" column
df_musicbrainz_v3_distinct <- df_musicbrainz_v3_unique %>%
  distinct(artist_no_featuring_lower, .keep_all = TRUE)

# get the mbids matched to artist names of df_hh_proc

df_hh_proc_v3 <- df_hh_proc %>%
  left_join(df_musicbrainz_v3_distinct[,c("artist mbid", "artist_no_featuring_lower")],
            by="artist_no_featuring_lower", 
            suffix = c("", "_musibrainz"))

#get the hot100 antijoin based on MBID
df_hh_mbid_no_match_v3 <- df_hh_proc_v3 %>%
  anti_join(df_musicbrainz_v3_distinct[,c("artist mbid", "artist_no_featuring_lower")],
            by="artist_no_featuring_lower", 
            suffix = c("", "_musibrainz"))

#get the v3 and v2 antijoin (2)
df_hh_mbid_no_match_v2_v3 <- df_hh_mbid_no_match_v2 %>%
  anti_join(df_hh_mbid_no_match_v3,
            by = "artist_no_featuring_lower")

#--> reduced non-matches by 34

# match all the unique songs to the hot100 based on the mbid

hh_mbv3_mbid_match <- df_hh_proc_v3 %>% 
  left_join(df_musicbrainz_v3_unique[,c("Artist_no_featuring",
                                        "song title", 
                                        # "release_mbid",
                                        # "release_title",
                                        # "release_year",
                                        # "song_release_mbid",
                                        "track_lower",
                                        "track_lower_no_ws",
                                        "track_artists_mbid_no_ws",
                                        "artist mbid")],
            by = "artist mbid",
            suffix = c("", "_musibrainz"))

#remove duplicates
hh_mbv3_mbid_match_pre <- hh_mbv3_mbid_match %>%
  distinct(track_artists_mbid_no_ws)

hh_mbv3_mbid_match_distinct <- hh_mbv3_mbid_match_pre %>%
  left_join(df_musicbrainz_v3_unique, by = "track_artists_mbid_no_ws")

# check which songs from mbv2 didn't find a match - same as before
hh_mbv3_mbid_match_antijoin <- df_musicbrainz_v3_unique %>%
  anti_join(hh_mbv3_mbid_match_distinct, by ="track_artists_mbid_no_ws")

#####---------------------- song-based matching: partial matching based on the process from MB1 data--------------####


# try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package
## results in only less matches than there are observations in MB, but there are matches without any distance - I don't understand at all

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) 

hot100_titles_partial_v3 <- stringdist_left_join(df_hh_proc_v3,
                                                 df_musicbrainz_v3_unique,
                                                 by = "track_lower_no_brackets_schar_ws", 
                                                 method = "lv", distance_col = "distance",
                                                 max_dist = 2) 

# check the unmatched examples and by filtering by (is.na(distance))

hh_unmatched_fuzzy_join_song_v3 <- hot100_titles_partial_v3 %>% filter(is.na(distance) == T)

#check for any non-matches between v3 and v2 nonmatches - i.e. non-matches due to new export

hh_unmatched_fuzzy_join_song_v3_v2 <- hh_unmatched_fuzzy_join_song_v3 %>%
  anti_join(hh_unmatched_fuzzy_join_song_v2,
            by = "track_lower_no_brackets_schar_ws.x")


# remove all the superfluous columns - use code below

var_relevance_v3 <- c("Artist",
                      "Track",
                      #"release_year",
                      "Artist_no_featuring",
                      #"release_title",
                      "artist_no_featuring_lower.x",
                      "artist_no_featuring_lower.y",
                      "track_lower_no_brackets.x",
                      "track_lower_no_brackets.y",
                      "track_lower_no_brackets_schar_ws.x",
                      "track_lower_no_brackets_schar_ws.y",
                      "artist mbid.x",
                      "artist mbid.y"
                      #"song_release_mbid"
                      )

hot100_titles_partial_v3 <- hot100_titles_partial_v3 %>%
  dplyr::select(all_of(var_relevance_v3))

# keep only rows where the artist_mbid.x matches artist_mbid.y

hot100_titles_partial_artists_v3 <- subset(hot100_titles_partial_v3, `artist mbid.x` == `artist mbid.y`)

# do an antijoin between df_hh_proc and the df from above to see which observations didn't match properly.

hot100_nomatch_titles_partial_artists_v3 <- df_hh_proc_v3 %>% anti_join(hot100_titles_partial_artists_v3, by = "Track") 

#total number of unmatched artists (no featuring).
hot100_nomatch_artists_v3 <- hot100_nomatch_titles_partial_artists_v3 %>%
  group_by(artist_no_featuring_lower) %>%
  summarise(count = n())

#distinct hh artists
hh_distinct_artists <- df_hh_proc %>%
  distinct(Artist)

##### ------------- write the datasets ------------------------------------------------------------------------####

#the matched dataset
write.csv(hot100_titles_partial_artists_v3, here::here("data", "incidental", "hot100_titles_partial_artists_v3.csv"))

#the unmatched songs
write.csv(hot100_nomatch_titles_partial_artists_v3, here::here("data", "incidental", "hot100_nomatch_titles_partial_artists_v3.csv"))

#the unmatched artists
write.csv(hot100_nomatch_artists_v3, here::here("data", "incidental", "hot100_nomatch_artists_v3.csv"))

