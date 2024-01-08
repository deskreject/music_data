#.............................
# Author: Alexander Staub
# Description: Code for similarity change visual analysis based on years for aom 2024
# Date created: 28.12.2023
# dependencies:
## script "data_preparation_MB_spotify_aom_2024" under "data_creation"
#.............................

#load the necessary dataframe
labels_year_similarity_df  <- readRDS(here::here("data", "interim_data", "similarity_scores_label_years_aom_2023.rda")) %>% 
  ungroup()

#transform DVs to numeric
cols_to_convert <- c("mean", "median", "sd", "cv")
labels_year_similarity_df[, cols_to_convert] <- lapply(labels_year_similarity_df[, cols_to_convert], function(x) as.numeric(as.character(x)))


#remove irrelevant years and irrelevant labels (less than three songs)
labels_year_similarity_df <- labels_year_similarity_df %>% 
  filter(release_year > 1997, release_year < 2006 ) %>% 
  filter(n_songs > 2)

#####----------------------------------- descripive visualization of similarity variables --------------------- #####

#extract the dependent variables

dependent_variables <- names(labels_year_similarity_df[,9:12])
names_dv <- c("mean similarity", "median similarity", "Standard Deviation", "Coefficient of Variation")


# distribution of variables

plot_list_distribution <- lapply(dependent_variables, function(dv){
  
  labels_year_similarity_df %>% ggplot(aes_string(x=dv)) +
    geom_histogram(bins = 50)
  
  
})

#rename
names(plot_list_distribution) <- paste0(dependent_variables)

#create grid - balanced plots
do.call(grid.arrange, c(plot_list_distribution, ncol = 2, nrow = 2))


# distribution of variables - by whether is US or is not US

plot_list_distribution_by_US <- lapply(seq_along(dependent_variables), function(i){
  
  labels_year_similarity_df %>% 
    ggplot(aes(x = .data[[dependent_variables[[i]]]], fill = as.factor(is_US))) +  # Add fill = "is_US" for grouping
    geom_histogram(bins = 70, position = "identity", alpha = 0.5) +  # Set alpha for transparency
    labs(title = paste0("Distribution of ", names_dv[[i]]), fill = "US (1) vs Europe (0)") +
    xlim(0,1) +
    theme_minimal()
    
  
  
})

#rename
names(plot_list_distribution_by_US) <- paste0(dependent_variables)

#create grid - balanced plots
do.call(grid.arrange, c(plot_list_distribution_by_US, ncol = 2, nrow = 2))