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
# create proprotions of terms by year by company
#.......

# Generate "_found" versions of terms for column names
term_cols <- colnames(release_labels_merged_unique_tracks[15:30])

# Step 1: Calculate count of each term
counts <- release_labels_merged_unique_tracks %>%
  group_by(release_year, release_country, label_name) %>%
  summarise(across(all_of(term_cols), sum, .names = "{.col}_count"))

# Step 2: Create a total count column
counts <- counts %>%
  rowwise() %>%
  mutate(count_total = sum(c_across(ends_with("_count")), na.rm = TRUE))

# Step 3: Calculate proportions for each term, handling division by zero and rounding to 3 decimal places
proportions <- counts %>%
  rowwise() %>%
  mutate(across(ends_with("_count"),
                ~round(ifelse(count_total == 0, 0, .x / count_total), 3),
                .names = "{.col}_prop"))


# Combine counts and proportions
final_df <- counts %>%
  bind_cols(proportions %>% select(ends_with("_prop")))

# Clean up for final output
df_combined <- final_df %>%
  ungroup()

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

