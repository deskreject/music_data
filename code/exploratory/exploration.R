#.............................
# Author: Alexander Staub
# Description: Explorative data analysis and visualizations
#.............................


#### ---------- Hot100: looking at the individual artsits ---------------#### 

# aggregate the df to artist & song level

df_artist_songs <- df %>%
  group_by(Artist, Track) %>%
  summarise(count = n(),
            release = min(Week),
            peak_position = max(Peak_Position))

#make a histogram of artists in the charts

df_artist_songs %>%
  group_by(Artist) %>%
  summarise(action = n()) %>%
  ggplot(aes(x=action), stat="count") +
  geom_histogram(binwidth = 1)

# barplot of artists

df_artist_songs %>%
  
  ggplot(aes(x=Artist)) + 
  geom_bar()

# excluding artists only with one song

df_artists_singles <- df_artist_songs %>%
  group_by(Artist) %>%
  summarise(count = n()) %>%
  filter(count > 2)

df_artist_songs %>%
  filter(Artist %in% df_artists_singles$Artist) %>%
  ggplot(aes(x=Artist)) + 
  geom_bar()

# limit to period of relevance

if (!require(lubridate)) install.packages("lubridate"); library(lubridate) # package for dealing with dates

df_artists_triple_relevant <- df_artist_songs %>%
  filter(year(release) > 1999 & year(release) < 2005) %>%
  group_by(Artist) %>%
  summarise(count = n()) %>%
  filter(count > 2)

sum(df_artists_triple_relevant$count)

# only include unique values of songs

df_songs_relevant_time_period <- df %>%
  filter(year(Week) >= 1999 & year(Week) < 2005) %>%
  group_by(Artist, Track) %>%
  summarise(count = n())

#exclude bracket related content

df_songs_relevant_time_period$Track_no_brackets <- gsub("\\(.*\\)", "", df_songs_relevant_time_period$Track) 

#### -------------- checking for the prevalance of remixing ------------ #####

# first, see if there are any words that pop up repeatedly in the title of songs

song_titles <- unique(df$Track)

#create a vector of individual strings

song_words <- unlist(strsplit(song_titles, " "))

#create a sorted frequency table

word_frequency <- sort(table(song_words), decreasing = T)

frequency_func <- function(column_name) {
  
  vector_one <- unique(df[,column_name])
  
  #create a vector of individual strings
  
  vector_two <- unlist(strsplit(vector_one, " "))
  
  #create a sorted frequency table
  
  frequency_table <- sort(table(vector_two), decreasing = T)

  #return the table
  
  return(frquency_table)
  
  }

frequency_func(column_name = "Track")

# --> very little to be learned here

## Do the same with artists

frequency_table_artists <- frequency(Artist)

#check "(from" in song title

from_match <- df[grepl("\\(From", df$Track),]

#checking the amount of song titles with ... in the title

track_match <- function(string){

return(df[grepl(string, df$Track),])

}

lapply()

# checking the amount of artists with the term "feat" in them

feat_match <- df[grepl("Feat", df$Artist),]

unique_feat_match <- unique(feat_match$Track)


#### ----------------- Musicbrainz #1: checking overlap in first scrape musicbrainz and hot100 ------------------------####

# remove all duplicate rows
df_musicbrainz_distinct <- unique(df_musicbrainz[,2:4])

## make all columns all lowercase
# hot100
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)

# musicbrainz
df_musicbrainz_distinct$track_lower <- tolower(df_musicbrainz_distinct$recording.title)
df_musicbrainz_distinct$artist_lower <- tolower(df_musicbrainz_distinct$recording.artist.credit.phrase)

# Creating a new column with replaced spelling of "featuring"
df_musicbrainz_distinct$artist_lower <- str_replace_all(df_musicbrainz_distinct$artist_lower, "(?<!\\w)(feat\\.|ft\\.|featuring)(?!\\w)", "featuring")
df_hh_proc$artist_lower <- str_replace_all(df_hh_proc$artist_lower, "(?<!\\w)(feat\\.|ft\\.|featuring)(?!\\w)", "featuring")

##track based alterations

# remove all song info in brackets
df_musicbrainz_distinct$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_distinct$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all special characteristics from the column without brackets
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
df_musicbrainz_distinct$artist_lower_no_bracket_schar <- gsub("[^[:alnum:]]", " ", df_musicbrainz_distinct$artist_lower_no_bracket)
df_hh_proc$artist_lower_no_bracket_schar <- gsub("[^[:alnum:]]", " ", df_hh_proc$artist_lower_no_brackets)

#remove the white space at the end of the string
df_musicbrainz_distinct$artist_lower_no_bracket_schar  <- trimws(df_musicbrainz_distinct$artist_lower_no_bracket_schar, which = "right")
df_hh_proc$artist_lower_no_bracket_schar <- trimws(df_hh_proc$artist_lower_no_bracket_schar, which = "right")

#remove any white space
df_musicbrainz_distinct$artist_lower_no_bracket_schar_ws <- gsub("\\s+", "", df_musicbrainz_distinct$artist_lower_no_bracket_schar)
df_hh_proc$artist_lower_no_bracket_schar_ws <- gsub("\\s+", "", df_hh_proc$artist_lower_no_bracket_schar)


# Combine the track and artist columns into one column
df_musicbrainz_distinct$tracks_artists <- paste(df_musicbrainz_distinct$track_lower_no_brackets_ws,
                                df_musicbrainz_distinct$artist_lower_no_bracket_schar_ws,
                                sep = " ")

df_hh_proc$tracks_artists <- paste(df_hh_proc$track_lower_no_brackets_ws,
                                  df_hh_proc$track_lower_no_brackets_schar_ws,
                                  sep = " ")

#......................
# The anti-join portion
#......................

# first, clean antijoin on two columns

hot100_nomatch_clean <- df_hh_proc %>% anti_join(df_musicbrainz_distinct, by = c("track_lower", "artist_lower")) 

# antijoin just on the lowercase song column

hot100_nomatch_titles <- df_hh_proc %>% anti_join(df_musicbrainz_distinct, by = "track_lower")

# antijoin just on the lowercase and spec char less song column

hot100_nomatch_titles_noschar <- df_hh_proc %>% anti_join(df_musicbrainz_distinct, by = "track_lower_no_schar")

#antijoin just on the lowercase and no bracket info song column

hot100_nomatch_titles_nobrackets <- df_hh_proc %>% anti_join(df_musicbrainz_distinct, by = "track_lower_no_brackets")

#antijoin no brackets, special characters and white space
## best one, 49 hits

hot100_nomatch_titles_no_brackets_schar_ws <- df_hh_proc %>% anti_join(df_musicbrainz_distinct, by = "track_lower_no_brackets_schar_ws")

#### ----------------- hot100_titles_partial_artists : creating left joined dataset (HH and MB) with fuzzy track join ------------------------####

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

write.csv(hh_unmatched_fuzzy_join_song,
          here::here("data", "incidental", "hh_unmatched_fuzzy_join_song.csv"),
                                          row.names = F)

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


write.csv(hot100_nomatch_titles_partial_artists,
          here::here("data", "interim_data", "hot100_nomatch_titles_partial_artists.csv"),
          row.names = F)

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

#### ----------------- RETIRED - df_hh_and_mb_leftjoin_clean_distinct_2 : creating left joined dataset (HH and MB) with exact track match ------------------------####

#.........................................................
# merging the tables to contain the info from both tables
#.........................................................

# leftjoin of the two tables
df_hh_and_mb_songs_left <- left_join(df_hh_proc,
                                df_musicbrainz_distinct,
                                by = "track_lower_no_brackets_schar_ws")

# full join
df_hh_and_mb_songs_full <- full_join(df_hh_proc,
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
                   "artist_lower_no_bracket_schar.y",
                   "artist_lower_no_bracket_schar.x",
                   "track_lower_no_brackets_schar_ws.y",
                   "track_lower_no_brackets_schar_ws.x",
                   "recording.id")

# left join

df_hh_and_mb_leftjoin_clean <- df_hh_and_mb_songs_left %>%
  dplyr::select(all_of(var_relevance))

# full join

df_hh_and_mb_fulljoin_clean <- df_hh_and_mb_songs_full %>%
  dplyr::select(all_of(var_relevance))

# for both of the above, there are matches on songs with the same name but with different artists.

# do the first and last word adjustments

df_hh_and_mb_leftjoin_clean <- df_hh_and_mb_leftjoin_clean %>%
  mutate(artist_lower_first.x = str_extract(artist_lower.x,"\\b\\w+\\b"),
         artist_lower_first.y = str_extract(artist_lower.y, "\\b\\w+\\b"),
         artist_last_word.x = str_extract(artist_lower.x, "\\b\\w+\\b$"),
         artist_last_word.y = str_extract(artist_lower.y, "\\b\\w+\\b$"),
         artist_last_word_lower_no_bracket_schar_ws.x = str_extract(artist_lower_no_bracket_schar.x,"\\b\\w+\\b$"),
         artist_last_word_lower_no_bracket_schar_ws.y = str_extract(artist_lower_no_bracket_schar.y,"\\b\\w+\\b$"))

# make sure that the match is based on the artist last word is present in the artsit of mb and beyonce matches to beyoncé - with brackets and special chars

if (!require(stringi)) install.packages("stringi"); library(stringi)

df_hh_and_mb_leftjoin_clean_distinct <- df_hh_and_mb_leftjoin_clean %>% 
  group_by(artist_last_word.x) %>% 
  mutate(artist_last_word.y_trans = stri_trans_general(artist_last_word.y, "latin-ascii")) %>%
  filter(grepl(artist_last_word.x, artist_last_word.y_trans, ignore.case = TRUE))

#get the new non-matching thing

hot100_nomatch_titles_artists <- df_hh_proc %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct, by = "track_lower_no_brackets_schar_ws")

mb_nomatch_titles_artists <- df_musicbrainz_distinct %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct, by = "track_lower_no_brackets_schar_ws")

#compare the two preferred matching specs

intersection_nomatch <- hot100_nomatch_titles_artists %>% anti_join(hot100_nomatch_titles_no_brackets_schar_ws, by = "track_lower_no_brackets_schar_ws")

# make sure that the match is based on the artist last word is present in the artsit of mb and beyonce matches to beyoncé - no brackets and special chars

df_hh_and_mb_leftjoin_clean_distinct_2 <- df_hh_and_mb_leftjoin_clean %>% 
  group_by(artist_last_word.x) %>% 
  mutate(artist_last_word_lower_no_bracket_schar_ws.y_trans = stri_trans_general(artist_last_word_lower_no_bracket_schar_ws.y, "latin-ascii")) %>%
  filter(grepl(artist_last_word_lower_no_bracket_schar_ws.x, artist_last_word_lower_no_bracket_schar_ws.y_trans, ignore.case = TRUE))

#get the new non-matching thing

hot100_nomatch_titles_artists_2 <- df_hh_proc %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct_2, by = "track_lower_no_brackets_schar_ws")

mb_nomatch_titles_artists_2 <- df_musicbrainz_distinct %>% anti_join(df_hh_and_mb_leftjoin_clean_distinct_2, by = "track_lower_no_brackets_schar_ws")

#compare the two preferred matching specs

intersection_nomatch_2 <- hot100_nomatch_titles_artists_2 %>% anti_join(hot100_nomatch_titles_no_brackets_schar_ws, by = "track_lower_no_brackets_schar_ws")


##### ----------------- H100 + MusicBrainz - Checking the distribution of songs and special versions of them --------------- ####

#create a table that shows the entries with the most duplicate matches

frequency_table_songs_mb <- hot100_titles_partial_artists %>%
  group_by(recording.title, recording.artist.credit.phrase) %>%
  summarize(number = n()) %>%
  arrange(desc(number))

#### -----------------hot100_titles_partial_artists : Analyses remixes -  look at the amount of "remix" in the left_joined df ------------------------####

# create dummy variable in the df for instances in the "recording.title" and / or "recording.artist.credit.phrase" of the words

# remix

hot100_titles_partial_artists$is_remix <- ifelse(grepl("remix",
                                                       hot100_titles_partial_artists$track_lower.y) | grepl("remix",
                                                                                                            hot100_titles_partial_artists$artist_lower.y),
                                                 1,
                                                 0)

# filter by only remix to test
hot100_partial_remix <- hot100_titles_partial_artists %>%
  filter(is_remix == 1)

## random checks

# mix

hot100_titles_partial_artists$is_mix <- ifelse(grepl("mix",
                                                     hot100_titles_partial_artists$track_lower.y) | grepl("mix",
                                                                                                          hot100_titles_partial_artists$artist_lower.y),
                                               1,
                                               0)
## random checks

# edit

hot100_titles_partial_artists$is_edit <- ifelse(grepl("edit",
                                                      hot100_titles_partial_artists$track_lower.y) | grepl("edit",
                                                                                                           hot100_titles_partial_artists$artist_lower.y),
                                                1,
                                                0)
## random checks

# remove duplicates in the "track.lower.y"

hot100_partial_mix_analysis <- hot100_titles_partial_artists[!duplicated(hot100_titles_partial_artists$track_lower.y), ]


##  What if multiple remixes of the same song?



# aggregate by month, group by the dummy, n counts as well as sum of remixes 
##  What if multiple remixes of the same song?

hot100_mix_analysis_tracks <- hot100_partial_mix_analysis %>%
  group_by(Artist, Track) %>%
  summarise(sum_remixes = sum(is_remix),
            sum_mixes = sum(is_mix),
            sum_edits = sum(is_edit))

# check which Tracks aren't in the remix analysis dataset
antijoin_remix_analysis <- df_hh_proc %>% anti_join(hot100_mix_analysis_tracks, by = "Track")

# check which songs that aren't in the remix dataset are not present in the antijoin based on the fuzzy join

antijoin_antijoins_remix_analysis <- antijoin_remix_analysis %>% anti_join(hot100_nomatch_titles_partial_artists, by = "Track")

# histogram remixes, mixes and edits

hot100_mix_analysis_tracks %>% ggplot(aes(x=sum_remixes)) + 
  geom_histogram(bins = 20)

# add the date of first charting to each "Track" and "Artist" match

