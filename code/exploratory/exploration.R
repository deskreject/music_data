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

if (!require(lubridate)) install.packages("lubridate"); library(lubridate) # environment creator package
df_artists_triple_relevant <- df_artist_songs %>%
  filter(year(release) > 1999 & year(release) < 2004) %>%
  group_by(Artist) %>%
  summarise(count = n()) %>%
  filter(count > 2)

sum(df_artists_triple_relevant$count)
