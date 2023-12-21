#.............................
# Author: Alexander Staub
# Description: Code for similarity change analysis based on years for aom 2024
# Date created: 21.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
labels_year_similarity_df  <- readRDS(here::here("data", "interim_data", "similarity_scores_label_years_aom_2023.rda"))
