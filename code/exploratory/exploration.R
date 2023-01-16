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


#### ----------------- Musicbrainz: checking overlap in first scrape musicbrainz and hot100 ------------------------####

# remove all duplicate rows
df_musicbrainz_distinct <- unique(df_musicbrainz[,2:3])

## make all columns all lowercase
# hot100
df_hh_proc$track_lower <- tolower(df_hh_proc$Track)
df_hh_proc$artist_lower <- tolower(df_hh_proc$Artist)

# musicbrainz
df_musicbrainz_distinct$track_lower <- tolower(df_musicbrainz_distinct$recording.title)
df_musicbrainz_distinct$artist_lower <- tolower(df_musicbrainz_distinct$recording.artist.credit.phrase)

##track based alterations
# remove all special characteristics 
df_musicbrainz_distinct$track_lower_no_schar <- gsub("[^a-zA-Z0-9 ]", " ", df_musicbrainz_distinct$track_lower)
df_hh_proc$track_lower_no_schar <- str_replace_all(df_hh_proc$track_lower, "[^[:alnum:]]", " ")

# remove all song info in brackets
df_musicbrainz_distinct$track_lower_no_brackets <-  gsub("\\(.*\\)", "", df_musicbrainz_distinct$track_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$track_lower)

# remove all the whitespace from the no schar column in both df_s to allow for partial matches
df_musicbrainz_distinct$track_lower_no_schar_ws <- gsub("\\s+", "", df_musicbrainz_distinct$track_lower_no_schar)
df_hh_proc$track_lower_no_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_schar)

# the same, no white space but excluding anything in brackets
df_musicbrainz_distinct$track_lower_no_brackets_ws <- gsub("\\s+", "", df_musicbrainz_distinct$track_lower_no_brackets)
df_hh_proc$track_lower_no_brackets_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_brackets)

##artist based alterations

#remove the bracket info from names
df_musicbrainz_distinct$artist_lower_no_bracket <- gsub("\\(.*\\)", "", df_musicbrainz_distinct$artist_lower)
df_hh_proc$track_lower_no_brackets <- gsub("\\(.*\\)", "", df_hh_proc$artist_lower)

#remove the special characteristics
df_musicbrainz_distinct$artist_lower_no_bracket_schar <- gsub("[^[:alnum:]]", "", df_musicbrainz_distinct$artist_lower_no_bracket)
df_hh_proc$track_lower_no_brackets_schar <- gsub("[^[:alnum:]]", "", df_hh_proc$track_lower_no_brackets)

#remove any white space
df_musicbrainz_distinct$artist_lower_no_bracket_schar_ws <- gsub("\\s+", "", df_musicbrainz_distinct$artist_lower_no_bracket_schar)
df_hh_proc$track_lower_no_brackets_schar_ws <- gsub("\\s+", "", df_hh_proc$track_lower_no_brackets_schar)

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

# try with substring matching the song title
## version 1
hot100_nomatch_titles_partial <- anti_join(df_hh_proc,
                                               df_musicbrainz_distinct,
                                               by = c("track_lower_no_schar" = "track_lower_no_schar")) %>%
  filter(!str_detect(track_lower_no_schar,
                     paste(df_musicbrainz_distinct$track_lower_no_schar, collapse = "|")))




## version 2 - try to deal with "na na na" not matching to "na na na na"
hot100_nomatch_titles_partial_2 <-  anti_join(df_hh_proc, df_musicbrainz_distinct,
                                              by = "track_lower_no_schar_ws",
                                              match = "partial")

## version 3 - try to deal with "nanana" not matching to "nananana" using the fuzzyjoin package

if (!require(fuzzyjoin)) install.packages("fuzzyjoin"); library(fuzzyjoin) # relative file paths

hot100_nomatch_titles_partial_3 <- stringdist_left_join(df_hh_proc,
                                                        df_musicbrainz_distinct,
                                                        by = c("track_lower_no_schar_ws" = "track_lower_no_schar_ws"), 
                                  method = "lv", distance_col = "distance",max_dist = 2) %>%
  filter(is.na(distance)) %>% select(-distance)

## version 4 - the above, but without the bracket text
hot100_nomatch_titles_partial_4 <- stringdist_left_join(df_hh_proc,
                                                        df_musicbrainz_distinct,
                                                        by = c("track_lower_no_brackets_ws" = "track_lower_no_brackets_ws"), 
                                                        method = "lv", distance_col = "distance",max_dist = 2) %>%
  filter(is.na(distance)) %>% select(-distance)


## version 5 - the above, but with the artist + track combination

hot100_nomatch_titles_partial_5 <- stringdist_left_join(df_hh_proc,
                                                        df_musicbrainz_distinct,
                                                        by = c("tracks_artists" = "tracks_artists"), 
                                                        method = "lv", distance_col = "distance",max_dist = 5) %>%
  filter(is.na(distance)) %>% select(-distance)


#save the dataframe
write.csv(hot100_nomatch_titles_partial_4,
          here("C:/R work/Research/music_data/data/interim_data/hot100_nomatch_titles_partial.csv"),
          row.names=FALSE)

write.csv(hot100_nomatch_titles_partial_5,
          here("C:/R work/Research/music_data/data/interim_data/hot100_nomatch_titles_artists_partial.csv"),
          row.names=FALSE)
          



#.........................................................
# merging the tables to contain the info from both tables
#.........................................................

# merging the tables using fuzzy_join

df_hh_and_mb_songs <- stringdist_left_join(df_hh_proc,
                                     df_musicbrainz_distinct,
                                     by = c("track_lower_no_brackets_ws" = "track_lower_no_brackets_ws"), 
                                     method = "lv",
                                     distance_col = "distance",
                                     max_dist = 2)

# need to combin the two columns of names and track to make the join

df_hh_and_mb_songs_artists <- stringdist_left_join(df_hh_proc,
                                     df_musicbrainz_distinct,
                                     by = c("tracks_artists" = "tracks_artists"), 
                                     method = "lv",
                                     distance_col = "distance",
                                     max_dist = 5)

#save the dataframe
write.csv(df_hh_and_mb_songs,
          here("C:/R work/Research/music_data/data/interim_data/hot100_joined_titles_partial.csv"),
          row.names=FALSE)

write.csv(df_hh_and_mb_songs_artists,
          here("C:/R work/Research/music_data/data/interim_data/hot100_joined_titles_artists_partial.csv"),
          row.names=FALSE)
