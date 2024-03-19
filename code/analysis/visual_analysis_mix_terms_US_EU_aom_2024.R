#.............................
# Author: Alexander Staub
# Description: Code for remix analysis visually and with models for aom 2024
# Date created: 11.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
release_labels_merged_unique_tracks  <- readRDS(here::here("data", "interim_data", "unique_tracks_mix_terms_countries_aom_2023.rda"))

#### --------------------------- preparing data for plotting and analysis country --------------------------- #####

#make a country indicator of is_us

release_labels_merged_unique_tracks$is_US <- ifelse(release_labels_merged_unique_tracks$release_country == "US",
                                 1,
                                 0)


# make country indiator for is EU
release_labels_merged_unique_tracks$is_europe <- ifelse(release_labels_merged_unique_tracks$release_country != "US",
                                 1,
                                 0)

#make a "is_EU" indicator - europe minus GB

release_labels_merged_unique_tracks$is_EU <- ifelse(release_labels_merged_unique_tracks$release_country == "US",
                                 0,
                                 ifelse(release_labels_merged_unique_tracks$release_country == "GB", 0,1))

#.......
# create proprotions of terms by year
#.......

# Generate "_found" versions of terms for column names
term_cols <- colnames(release_labels_merged_unique_tracks[15:30])

# Create a function to summarize for a given term
summarize_term <- function(df, term) {
  df %>%
    group_by(release_year, release_country) %>%
    summarise(across(all_of(term), list(count = sum, prop = mean), .names = "{.col}_{.fn}")) 
}

# Apply the function to each term
df_list <- lapply(term_cols, function(term) summarize_term(release_labels_merged_unique_tracks, term))

# Combine the dataframes
df_combined <- Reduce(function(df1, df2) merge(df1, df2, by = c("release_year", "release_country"), all = TRUE), df_list)

# add in total number of songs
df_combined$total_songs = df_combined$any_term_count/df_combined$any_term_prop

# Reshape the data for plotting
df_plot_us <- df_combined %>%
  pivot_longer(cols = ends_with("_prop"), names_to = "term", values_to = "proportion")

# Remove the "_prop" suffix from the term names
df_plot_us$term <- gsub("_prop$", "", df_plot_us$term)

#round to three decimal places
df_plot_us$proportion <- round(df_plot_us$proportion, 3)

# merge in the "is_US" etc indicators

df_country_indicators <- release_labels_merged_unique_tracks %>%
  group_by(release_country, is_US)  %>%
  dplyr::summarise(count = n())  %>%
  dplyr::select(!count)

df_plot_us <- left_join(df_plot_us, 
                         df_country_indicators,
                         by = "release_country") 

# Remove the "any_term" from df_plot
df_plot_no_anyterm_us <- df_plot_us %>%
  filter(term != "any_terms")

# keep only the terms that were of relevance for remixing
df_plot_remixing_us <- df_plot_us %>%
  filter(term %in% c("edit_found", "mix_found", "radio_found", "remix_found"))

#### ------------------- PLOTS: simple year level term analysis ------------------------------ ####


#............................
# show the plot of the terms by year
#............................

#keep only years of relevance
release_labels_merged_unique_tracks <- release_labels_merged_unique_tracks %>%
  filter(release_year > 1997, release_year < 2006)

# Reshape the data
data_long_country <- release_labels_merged_unique_tracks %>%
  filter(release_year > 1997, release_year < 2006) %>%
  select(release_year, release_country, is_EU, is_US, is_europe, ends_with("_found")) %>%
  pivot_longer(cols = -c(release_year, release_country, is_EU, is_US, is_europe), names_to = "term", values_to = "found") %>%
  mutate(term = str_remove(term, "_found")) %>%
  group_by(release_year, release_country, is_EU, is_US, is_europe, term) %>%
  summarize(count = sum(found)) %>%
  ungroup()

#create the barplot of the terms
total_song_plot <- release_labels_merged_unique_tracks %>%
  ggplot(aes(x = release_year)) +
  geom_bar() +
  facet_wrap (~ release_country) +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

by_term_plot <- ggplot(data_long_country, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  facet_wrap (~ release_country) +
  theme_minimal() +
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot, by_term_plot, ncol = 1)

#create a barplot only for remix/clubmix

total_song_plot_country <- release_labels_merged_unique_tracks %>%
  ggplot(aes(x = release_year)) +
  geom_bar() +
  facet_wrap(~release_country) +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

data_long_restricted_remix <- data_long_country %>%
  filter(term %in% c("mix", "remix", "radio", "edit"))

by_remix_term_plot <- ggplot(data_long_restricted_remix, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  facet_wrap(~release_country)+
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot_country, by_remix_term_plot, ncol = 1)

#create the plots for US vs non-US

total_song_plot_nonUS <- release_labels_merged_unique_tracks %>%
  ggplot(aes(x = release_year)) +
  geom_bar() +
  facet_wrap(~is_US) +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

data_long_restricted_remix <- data_long_country %>%
  filter(term %in% c("mix", "remix", "radio", "edit"))

by_remix_term_plot_nonUS  <- ggplot(data_long_restricted_remix, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  facet_wrap(~is_US)+
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot_nonUS, by_remix_term_plot_nonUS, ncol = 1)


#create the plots for US vs EU

total_song_plot_EU <- release_labels_merged_unique_tracks %>%
  filter(release_country != "GB") %>%
  ggplot(aes(x = release_year)) +
  geom_bar() +
  facet_wrap(~is_US) +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

data_long_restricted_remix <- data_long_country %>%
  filter(term %in% c("mix", "remix", "radio", "edit"))

by_remix_term_plot_EU  <- data_long_restricted_remix %>%
  filter(release_country != "GB") %>%
  ggplot( aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  facet_wrap(~is_US)+
  labs(title = "Term Count per Year", x = "Year", y = "Count") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = 1, keyheight = 1))

# Combine the plots into one figure
grid.arrange(total_song_plot_EU, by_remix_term_plot_EU, ncol = 1)


####------------------------- proportions/intensity plots -------------------------- #### 

# all terms - by release country
ggplot(df_plot_us, aes(x = release_year, y = proportion, color = term, group = term)) +
  geom_line() +
  labs(x = "Release Year", y = "Proportion", color = "Term") +
  facet_wrap(~release_country) +
  geom_vline(xintercept = 2001, color = "red", linetype = "dashed") +
  theme_minimal()

# all terms - by is_US
prop_plot_is_us <- df_plot_us %>%
  group_by(release_year, is_US, term) %>%
  dplyr::summarise(proportion = mean(proportion)) %>%
ggplot( aes(x = release_year, y = proportion, color = term, group = term)) +
  geom_line() +
  labs(x = "Release Year", y = "Proportion", color = "Term") +
  facet_wrap(~is_US) +
  geom_vline(xintercept = 2001, color = "red", linetype = "dashed") +
  theme_minimal()

# Plot - only remixing by is_us value
prop_plot_remix_is_us <- df_plot_remixing_us %>%
  group_by(release_year, is_US, term) %>%
  dplyr::summarise(proportion = mean(proportion)) %>%
  ggplot( aes(x = release_year, y = proportion, color = term, group = term)) +
  geom_line() +
  labs(x = "Release Year", y = "Proportion", color = "Term") +
  facet_wrap(~is_US) +
  geom_vline(xintercept = 2001, color = "red", linetype = "dashed") +
  theme_minimal()

