#.............................
# Author: Alexander Staub
# Description: Code for remix analysis visually and with models for aom 2024
# Date created: 11.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
readRDS(here::here("data", "interim_data", "unique_tracks_mix_terms_countries_aom_2023.rda"))

#### --------------------------- preparing data for plotting and analysis country --------------------------- #####

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
  select(release_year, release_country, ends_with("_found")) %>%
  pivot_longer(cols = -c(release_year, release_country), names_to = "term", values_to = "found") %>%
  mutate(term = str_remove(term, "_found")) %>%
  group_by(release_year, release_country, term) %>%
  summarize(count = sum(found)) %>%
  ungroup()

#create the barplot of the terms
total_song_plot <- release_labels_merged_unique_tracks %>%
  ggplot(aes(x = release_year)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Total Songs per Year", x = "Year", y = "Count")

by_term_plot <- ggplot(data_long_country, aes(x = release_year, y = count, fill = term)) +
  geom_bar(stat = "identity") +
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
