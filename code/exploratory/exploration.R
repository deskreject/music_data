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
  filter(year(Week) > 1999 & year(Week) < 2005) %>%
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
