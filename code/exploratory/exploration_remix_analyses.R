#..............
# Author: Alexander Staub
# Date: 29.08.2023
# description: File for analysis that is done to understand the change in song remixes pre and post shock
#........................................................................................................

# Package loading beforehand



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

#####-------------------------- hh_mbv2_mbid_match_distinct: REMIX analysis - generalized version, adapted to mb_v4 --------------------------------------######

#..............................
# loading in the required data
#..............................

# mb v4 match basis
hh_mb_mbid_match <- read.csv(here::here("data", "incidental","fourth_mb_export", "hot100_titles_partial_artists_mbids_v4.csv")) 


#...........................
# prechecks
#...........................

# table by year to assess the amount 

df_year_count <- hh_mb_mbid_match %>%
  group_by(release_year) %>%
  summarise(recordings = n())

# plot
df_year_count %>% ggplot(aes(x=release_year, y=recordings)) + 
  geom_bar(stat = "identity")

# random sample to check whether they actually match + to identify terms that need to be used to identify remixes

random_sample_check <- hh_mb_mbid_match[sample(nrow(hh_mb_mbid_match), size =200),]

View(random_sample_check[,c("artist_no_featuring_lower", "Artist_no_featuring")])

## resulting term collection
terms <- c("remix", "mix", "dub", "edit", "radio", "rmx", "dirty", "club mix", "club version", "extended version", "extended mix", "instrumental")

# remove duplicates of song titles

hh_mb_mbid_match_unique_song <- hh_mb_mbid_match %>%
  
  #make sure that "distinct" function only keeps observations that have a release year
  
  arrange(song_title, is.na(release_year)) %>%
  
  distinct(song_title, .keep_all = T)

## redo the plot by year

df_year_unique_song_count <- hh_mb_mbid_match_unique_song %>%
  group_by(release_year) %>%
  summarise(recordings = n())

# proceed to the remix analysis

hh_mb_mbid_match_remix <- hh_mb_mbid_match_unique_song %>%
  select(Artist.x, Artist_no_featuring, song_title, release_year, date) %>%
  mutate(song_title_lower = tolower(song_title))

# Create new columns for each term
for (term in terms) {
  hh_mb_mbid_match_remix <- hh_mb_mbid_match_remix %>%
    mutate(!!sym(paste0(term, "_found")) := as.integer(str_detect(song_title_lower, term)))
}

# Create an "any_term" column
hh_mb_mbid_match_remix <- hh_mb_mbid_match_remix %>%
  mutate(any_term = as.integer(rowSums(select(., ends_with("_found"))) > 0))


#............................
# show the plot of the terms by year
#............................

# Reshape the data
data_long <- hh_mb_mbid_match_remix %>%
  select(release_year, ends_with("_found")) %>%
  pivot_longer(cols = -release_year, names_to = "term", values_to = "found") %>%
  mutate(term = str_remove(term, "_found")) %>%
  group_by(release_year, term) %>%
  summarize(count = sum(found)) %>%
  ungroup()

#create the barplot of the terms
total_song_plot <- ggplot(hh_mb_mbid_match_remix, aes(x = release_year)) +
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
term_cols <- colnames(hh_mb_mbid_match_remix[7:18])

# Create a function to summarize for a given term
summarize_term <- function(df, term) {
  df %>%
    group_by(release_year) %>%
    summarise(across(all_of(term), list(count = sum, prop = mean), .names = "{.col}_{.fn}"))
}

# Apply the function to each term
df_list <- lapply(term_cols, function(term) summarize_term(hh_mb_mbid_match_remix, term))

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
hh_mbv2_mbid_match_remix_oui <- hh_mb_mbid_match_remix %>%
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