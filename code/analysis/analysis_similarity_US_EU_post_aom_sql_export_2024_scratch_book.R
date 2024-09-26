#.............................
# Author: Alexander Staub
# Description: Code to prepare the sql export data for further analysis
# Date created: 22.05.2024
# dependencies:
## script "analysis_similarity_US_EU_postaom_sql_export_2024"
#.............................

#####----------------------------- exploratory analysis - n of songs ----------------------- #####

#check the number of observations by region and year of remix panel data
n_obs_panel_remix <- labels_year_similarity_df_remix_new %>%
  group_by(is_US, release_year) %>%
  summarise(n = n()) %>%
  arrange(is_US, release_year)

#plot the data, grouped by is_US grouping variable
n_obs_panel_remix %>%
  ggplot(aes(x = release_year, y = n, color = as.factor(is_US))) +
 # geom_line() +
  geom_point() +
  labs(title = "Number of observations by region and year of remix panel data",
       x = "Year",
       y = "Number of observations") +
  theme_minimal()


#unique recording names
unique_song_names <- df_mb_songs_filtered_unique %>% 
  select(name_recording) %>% 
  distinct() %>% 
  nrow()
