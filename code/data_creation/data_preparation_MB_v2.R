#..............................
# Author: Alexander Staub
# Description: preparing the v2 data export of MB
#....................................................

#...........................................................
# MB specific adjustments - like date, duplication removal
#...........................................................

## create copy of original

df_musicbrainz_original <- df_musicbrainz_v2

# Determine the date format for each observation in the release_date column
date_formats <- sapply(df_musicbrainz_v2$release_date, function(x) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return("yyyy-mm-dd")
  } else if (grepl("^\\d{4}$", x)) {
    return("yyyy")
  } else if (grepl("^\\d{4}-\\d{2}$", x)) {
    return("yyyy-mm")
  } else if (x == "") {
    return("blank")
  } else {
    return("unknown")
  }
})

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

# DONT WORK CORRECTLY: Convert the "yyyy" format to a date format
## df_musicbrainz_v2$release_year <- ifelse(is.na(df_musicbrainz_v2$release_year), NA, ymd(paste0(df_musicbrainz_v2$release_year, "-01-01")))

# Check for duplicates

song_id_duplicates <- table(df_musicbrainz_v2$song_mbid)

print(song_id_duplicates)

#spot checks

spot_check_wiseguys <- df_musicbrainz_v2 %>% filter(song_mbid == "03d78354-d5cf-4732-887a-7f0583e3d2ba")

spot_check_scott <- df_musicbrainz_v2 %>% filter(song_mbid == "01a20123-4628-4712-b86c-f8cf75b87877")


## Removing duplicates:

# Create the song_mbid + release_mbid + title
df_musicbrainz_v2$song_release_mbid <- paste0(df_musicbrainz_v2$song_mbid,"_",df_musicbrainz_v2$release_mbid,"_", df_musicbrainz_v2$song_title)

#spot checks

spot_check_coldplay <- df_musicbrainz_v2 %>% filter(song_release_mbid == "7dfbca16-352e-42f3-9802-8c75128bb189_e7498c8d-906d-40af-9a4b-4c79aa708308_Easy to Please")

# remove duplicates
# First, get the distinct rows based on 'song_release_mbid' column
df_musicbrainz_v2_unique_pre <- df_musicbrainz_v2 %>%
  distinct(song_release_mbid)

# Next, join the distinct rows back to the original data frame
df_musicbrainz_v2_unique <- df_musicbrainz_v2_unique_pre %>%
  left_join(df_musicbrainz_v2, by = "song_release_mbid")

## Removing data ranges that are not relevant

df_musicbrainz_v2_relevant <- df_musicbrainz_v2_unique %>%
  filter(release_year >= 1999 & release_year < 2005)



####----------------- the merging process - preprocessing and adding variables -----------------------------####

#...........................
# artist level alterations
#...........................



# make lower case
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)
df_musicbrainz_v2_unique$artist_no_featuring_lower <- tolower(df_musicbrainz_v2_unique$Artist_no_featuring)

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


#................................................................................
##checking for the amount of the below pattern that appears in the artist column
#................................................................................


# Initialize an empty dataframe to store the counts
num_matches_by_pattern <- data.frame()

# Loop through the patterns and count the number of observations that match each pattern separately
for (pattern in patterns) {
  pattern_count <- df_hh_proc %>%
    mutate(match = str_detect(artist_lower, pattern)) %>%
    filter(match) %>%
    count() %>%
    mutate(Pattern = pattern)
  
  num_matches_by_pattern <- bind_rows(num_matches_by_pattern, pattern_count)
}

# Rename the count column
num_matches_by_pattern <- num_matches_by_pattern %>%
  rename(Count = n)


#........................
# Song level alteration
#.......................

# make lower case
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
df_musicbrainz_v2_unique$track_lower <- tolower(df_musicbrainz_v2_unique$song_title)


##track based alterations

# remove all song info in brackets
df_musicbrainz_v2_unique$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_v2_unique$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all special characteristics 
df_musicbrainz_v2_unique$track_lower_no_schar <- gsub("[^a-zA-Z0-9 ]", " ", df_musicbrainz_v2_unique$track_lower_no_brackets)
df_hh_proc$track_lower_no_schar <- str_replace_all(df_hh_proc$track_lower_no_brackets, "[^[:alnum:]]", " ")


# remove all the whitespace from the no schar column in both df_s to allow for partial matches
df_musicbrainz_v2_unique$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_musicbrainz_v2_unique$track_lower_no_schar)
df_hh_proc$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_schar)


#####---------------------- artist-based matching: getting all songs related to an artist ------------------------#####

# Remove duplicates in the "df_musicbrainz_v2_unique" dataframe based on the "artist_no_featuring_lower" column
df_musicbrainz_v2_distinct <- df_musicbrainz_v2_unique %>%
  distinct(artist_no_featuring_lower, .keep_all = TRUE)

# get the mbids matched to artist names of df_hh_proc

df_hh_proc_v2 <- df_hh_proc %>%
  left_join(df_musicbrainz_v2_distinct[,c("artist_mbid", "artist_no_featuring_lower")],
                        by="artist_no_featuring_lower", 
            suffix = c("", "_musibrainz"))

#get the hot100 antijoin based on MBID
df_hh_mbid_no_match_v2 <- df_hh_proc_v2 %>%
  anti_join(df_musicbrainz_v2_distinct[,c("artist_mbid", "artist_no_featuring_lower")],
            by="artist_no_featuring_lower") 
           # suffix = c("", "_musibrainz"))

# match all the unique songs to the hot100 based on the mbid

hh_mbv2_mbid_match <- df_hh_proc_v2 %>% 
  left_join(df_musicbrainz_v2_unique[,c("Artist_no_featuring",
                                        "song_title", 
                                        "release_mbid",
                                        "release_title",
                                        "release_year",
                                        "song_release_mbid",
                                        "artist_mbid")],
            by = "artist_mbid",
            suffix = c("", "_musibrainz"))

#remove duplicates
hh_mbv2_mbid_match_distinct <- hh_mbv2_mbid_match %>%
  distinct(song_release_mbid, .keep_all = TRUE)

# check which songs from mbv2 didn't find a match
hh_mbv2_mbid_match_antijoin <- df_musicbrainz_v2_unique %>%
  anti_join(hh_mbv2_mbid_match_distinct, by ="song_release_mbid")

#remove songs before 2008 and after 1994

hh_mbv2_mbid_match_window <- hh_mbv2_mbid_match_distinct %>%
  filter(release_year >= 1994 & release_year <= 2007)

#####-------------------------- REMIX analysis: musicbrainz v2 based --------------------------------------######

#...........................
# Moved to: exploration.R
#...........................

#####---------------------- song-based matching: partial matching based on the process from MB1 data--------------####


# try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package
## results in only less matches than there are observations in MB, but there are matches without any distance - I don't understand at all

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) 

hot100_titles_partial_v2 <- stringdist_left_join(df_hh_proc_v2,
                                                df_musicbrainz_v2_unique,
                                                by = "track_lower_no_brackets_schar_ws", 
                                                method = "lv", distance_col = "distance",
                                                max_dist = 2) 

# check the unmatched examples and by filtering by (is.na(distance))

hh_unmatched_fuzzy_join_song_v2 <- hot100_titles_partial_v2 %>% filter(is.na(distance) == T)

# remove all the superfluous columns - use code below

var_relevance_v2 <- c("Artist",
                      "Track",
                      "release_year",
                      "Artist_no_featuring",
                      "release_title",
                      "artist_no_featuring_lower.x",
                      "artist_no_featuring_lower.y",
                      "track_lower_no_brackets.x",
                      "track_lower_no_brackets.y",
                      "track_lower_no_brackets_schar_ws.x",
                      "track_lower_no_brackets_schar_ws.y",
                      "artist_mbid.x",
                      "artist_mbid.y",
                      "song_release_mbid")

hot100_titles_partial_v2 <- hot100_titles_partial_v2 %>%
  dplyr::select(all_of(var_relevance_v2))

# keep only rows where the artist_mbid.x matches artist_mbid.y

hot100_titles_partial_artists_v2 <- subset(hot100_titles_partial_v2, artist_mbid.x == artist_mbid.y)

# do an antijoin between df_hh_proc and the df from above to see which observations didn't match properly.

hot100_nomatch_titles_partial_artists_v2 <- df_hh_proc_v2 %>% anti_join(hot100_titles_partial_artists_v2, by = "Track") 

#total number of unmatched artists (no featuring).
hot100_nomatch_artists_v2 <- hot100_nomatch_titles_partial_artists_v2 %>%
  group_by(artist_no_featuring_lower) %>%
  summarise(count = n())

# write it to data
write.csv(hot100_nomatch_titles_partial_artists_v2, here::here("data", "incidental", "unmatched_hot100_songs_mb_export_v2.csv"))
