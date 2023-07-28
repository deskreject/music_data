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

# Now to be found in the "data preparation.R" file, heading "23-03: ..." 

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

# filter by only "mix" to test
hot100_partial_mix <- hot100_titles_partial_artists %>%
  filter(is_mix == 1)


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

table(hot100_mix_analysis_tracks$sum_remixes)

# add the date of first charting to each "Track" and "Artist" match

#####-------------------------- hh_mbv2_mbid_match_distinct: REMIX analysis - musicbrainz v2 based --------------------------------------######

#...........................
# prechecks
#...........................

# table by year to assess the amount 

df_year_count <- hh_mbv2_mbid_match_window %>%
  group_by(release_year) %>%
  summarise(recordings = n())

# plot
df_year_count %>% ggplot(aes(x=release_year, y=recordings)) + 
  geom_bar(stat = "identity")

# random sample to check whether they actually match + to identify terms that need to be used to identify remixes

random_sample_check <- hh_mbv2_mbid_match_window[sample(nrow(hh_mbv2_mbid_match_window), size =200),]

View(random_sample_check[,c("artist_no_featuring_lower", "Artist_no_featuring")])

## resulting term collection
terms <- c("remix", "mix", "dub", "edit", "radio", "rmx", "dirty", "club mix", "club version", "extended version", "extended mix", "instrumental")

# remove duplicates of song titles

hh_mbv2_mbid_match_unique_song <- hh_mbv2_mbid_match_window %>%
  distinct(song_title, .keep_all = T)

## redo the plot by year

df_year_unique_song_count <- hh_mbv2_mbid_match_unique_song %>%
  group_by(release_year) %>%
  summarise(recordings = n())

# proceed to the remix analysis

hh_mbv2_mbid_match_remix <- hh_mbv2_mbid_match_unique_song %>%
  select(Artist, Artist_no_featuring, track_lower, song_title, release_year) %>%
  mutate(song_title_lower = tolower(song_title))

# Create new columns for each term
for (term in terms) {
  hh_mbv2_mbid_match_remix <- hh_mbv2_mbid_match_remix %>%
    mutate(!!sym(paste0(term, "_found")) := as.integer(str_detect(song_title_lower, term)))
}

# Create an "any_term" column
hh_mbv2_mbid_match_remix <- hh_mbv2_mbid_match_remix %>%
  mutate(any_term = as.integer(rowSums(select(., ends_with("_found"))) > 0))


#............................
# show the plot of the terms by year
#............................

# Reshape the data
data_long <- hh_mbv2_mbid_match_remix %>%
  select(release_year, ends_with("_found")) %>%
  pivot_longer(cols = -release_year, names_to = "term", values_to = "found") %>%
  mutate(term = str_remove(term, "_found")) %>%
  group_by(release_year, term) %>%
  summarize(count = sum(found)) %>%
  ungroup()

#create the barplot of the terms
total_song_plot <- ggplot(hh_mbv2_mbid_match_remix, aes(x = release_year)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

by_term_plot <- ggplot(data_long, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot, by_term_plot, ncol = 1)

#create a barplot only for remix/clubmix

data_long_restricted_remix <- data_long %>%
  filter(term %in% c("club mix", "club version", "remix"))

by_remix_term_plot <- ggplot(data_long_restricted_remix, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot, by_remix_term_plot, ncol = 1)

#...........................................................
# generate the proportion of terms per year plot
#...........................................................

##  Data preparation

# Generate "_found" versions of terms for column names
term_cols <- colnames(hh_mbv2_mbid_match_remix[7:18])

# Create a function to summarize for a given term
summarize_term <- function(df, term) {
  df %>%
    group_by(release_year) %>%
    summarise(across(all_of(term), list(count = sum, prop = mean), .names = "{.col}_{.fn}"))
}

# Apply the function to each term
df_list <- lapply(term_cols, function(term) summarize_term(hh_mbv2_mbid_match_remix, term))

# Combine the dataframes
df_combined <- Reduce(function(df1, df2) merge(df1, df2, by = "release_year", all = TRUE), df_list)

# Calculate the total number of songs and total count of terms for each year
df_combined <- df_combined %>%
  mutate(any_terms_count = rowSums(select(df_combined, contains("_count"))),
         any_terms_prop = rowSums(select(df_combined, contains("_prop"))))

# add in total number of songs
 df_combined$total_songs = df_combined$any_terms_count/df_combined$any_terms_prop
 
 ## Plotting
 
 # Reshape the data for plotting
 df_plot <- df_combined %>%
   pivot_longer(cols = ends_with("_prop"), names_to = "term", values_to = "proportion")
 
 # Remove the "_prop" from the term names for cleaner plot labels
 df_plot$term <- gsub("_prop$", "", df_plot$term)
 
 # remove the "any_term" from the df_plot
 df_plot_no_anyterm <- df_plot %>%
   filter(term != "any_terms")
 
 # keep only the terms that were of relevance for remixing
 df_plot_remixing <- df_plot %>%
   filter(term %in% c("club mix_found", "remix_found"))
 
 
 # Plot - all
 ggplot(df_plot, aes(x = release_year, y = proportion, color = term, group = term)) +
   geom_line() +
   labs(x = "Release Year", y = "Proportion", color = "Term") +
   theme_minimal()
 
 # Plot - no any terms
 
 prop_plot <- ggplot(df_plot_no_anyterm, aes(x = release_year, y = proportion, color = term, group = term)) +
   geom_line() +
   labs(x = "Release Year", y = "Proportion", color = "Term") +
   theme_minimal() +
   theme(legend.position = "none")
 
 # Plot - only remixing
 
 prop_plot_remix <- ggplot(df_plot_remixing, aes(x = release_year, y = proportion, color = term, group = term)) +
   geom_line() +
   labs(x = "Release Year", y = "Proportion", color = "Term") +
   theme_minimal() +
   theme(legend.position = "none")
 
 
 
 ## combining the three plots - all terms
 grid.arrange(total_song_plot,
              by_term_plot,
              prop_plot,
              ncol = 1)

## combining the three plots - remixing only
 
 grid.arrange(total_song_plot,
              by_remix_term_plot,
              prop_plot_remix,
              ncol = 1)
  
######------------------------- hh_mbv2_mbid_match_distinct: REMIX analysis - OUI 2023 conference visuals -------#####
 
#.......................................................................................
 # Requirements:
 # data_perparation_MB_v2.R: all the merging undertaken there to lead to the _window dataset
 # exploration_mb_v2.R: section "hh_mbv2_mbid_match_distinct: REMIX analysis - musicbrainz v2 based" - prior to plots
#.......................................................................................
 
# Data alterations - reduce years
 #all songs
 data_long_oui <- data_long %>%
   filter(as.integer(release_year) >= 1996, as.integer(release_year) <= 2005)
 
 #remix songs
 hh_mbv2_mbid_match_remix_oui <- hh_mbv2_mbid_match_remix %>%
   filter(as.integer(release_year) >= 1996, as.integer(release_year) <= 2005)
 
 #remix proportions
 df_plot_remixing_oui <- df_plot_remixing %>%
   filter(as.integer(release_year) >= 1996, as.integer(release_year) <= 2005)
 
 #create a barplot only for remix/clubmix
 
 data_long_restricted_oui_remix <- data_long_oui %>%
   filter(term %in% c("club mix", "remix"))
 
 #create the barplot of the total songs
 total_song_plot_oui <- ggplot(hh_mbv2_mbid_match_remix_oui, aes(x = release_year)) +
   geom_bar() +
   theme_minimal() +
   labs(title = "Total Songs per Year", x = "Year", y = "Count")
 
 #create the barplot of the terms
 by_remix_term_plot_oui <- ggplot(data_long_restricted_oui_remix, aes(x = release_year, y = count, fill = term)) +
   geom_bar(stat = "identity") +
   theme_minimal() +
   labs(title = "Term Count per Year", x = "Year", y = "Count") +
   theme(legend.position = "bottom") +
   guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))
 
 #create the proportions plot by terms
 prop_plot_remix_oui <- ggplot(df_plot_remixing_oui, aes(x = release_year, y = proportion, color = term, group = term)) +
   geom_line(size = 1) +
   geom_point() + 
   labs(title = "Proportion of mix/remix to total songs", x = "Year", y = "Proportion", color = "Term") +
   theme_minimal() +
   theme(legend.position = "none")

 grid.arrange(total_song_plot_oui,
              by_remix_term_plot_oui,
              prop_plot_remix_oui,
              ncol = 1)
  