#.............................
# Author: Alexander Staub
# Description: Code used to prepare data for the aom 2024 submission analyses
# Date created: 11.12.2023
#.............................

####----------------- preparation of the sql label export ---------------------------------####


#save the observations for which release_id is unique
unique_release_IDs_labels <- release_labels_AS_v1 %>%
  group_by(release_mbid) %>%
  filter(n() == 1) %>%
  ungroup()

# Data frame with rows where the variable has duplicates
duplicate_release_IDs_labels <- release_labels_AS_v1 %>%
  group_by(release_mbid) %>%
  filter(n() > 1) %>%
  ungroup()


# sort duplicates by year in ascending order and retain unique release ids
unique_duplicates_release_IDs_labels <- duplicate_release_IDs_labels %>%
  group_by(release_mbid) %>%
  arrange(release_mbid, is.na(release_year), release_year) %>%
  distinct(release_mbid, .keep_all = TRUE) %>%
  ungroup()

#row bind both the unique_dupicates and the unique release ids table
release_IDs_labels_AS_merge <- bind_rows(unique_release_IDs_labels,
                                         unique_duplicates_release_IDs_labels)




#................
# all the checks
#................

#check number of 
## country codes: 227
## release year: 32
apply(release_labels_AS_v1, 2, function(x){length(unique(x))})

#distribution of the country codes
country_code_table <- release_labels_AS_v1 %>%
  group_by(release_country) %>%
  dplyr::summarise(count = n())

#year table
year_table <- release_labels_AS_v1 %>%
  group_by(release_year) %>%
  dplyr::summarise(count = n())

glimpse(unique_release_IDs_labels)

#check variable distribution for unique rows
apply(unique_release_IDs_labels, 2, function(x){length(unique(x))})

#check distribution of label types for uniques
unique_label_type_table <- unique_release_IDs_labels %>%
  group_by(type_name) %>%
  dplyr::summarise(count = n())

#check distribution of label names
unique_label_names_table <- unique_release_IDs_labels %>%
  group_by(label_name) %>%
  dplyr::summarise(count = n())

#number of nas for non_unique
apply(duplicate_release_IDs_labels, 2, function(x){sum(is.na(x) == T)})

#check distribution of unique values - merge dataframe 

apply(release_IDs_labels_AS_merge, 2, function(x){length(unique(x))})

#check NAs - merge dataframe 
apply(release_IDs_labels_AS_merge, 2, function(x){sum(is.na(x) == T)})

#distribution of the country codes
country_code_table_merge <- release_IDs_labels_AS_merge %>%
  group_by(release_country) %>%
  dplyr::summarise(count = n())

#year table
year_table_merge <- release_IDs_labels_AS_merge %>%
  group_by(release_year) %>%
  dplyr::summarise(count = n())

#### -------------------------- analysis of ADs data on release IDs -------------------- ####

# Replace 'Unknown' with NA in all columns
releases_labels_v1 <- releases_labels_v1 %>%
  mutate_all(~na_if(., "Unknown"))

#recreate appropriate release dates 
releases_labels_v1 <- releases_labels_v1 %>%
  mutate(
    release_year = as.integer(str_extract(release_date, "^\\d{4}")),
    release_month = as.integer(ifelse(str_length(release_date) >= 7, str_sub(release_date, 6, 7), NA)),
    release_day = as.integer(ifelse(str_length(release_date) == 10, str_sub(release_date, 9, 10), NA))
  )



#left_join my sql variables to alessio on release_MBID with suffix AS 
release_labels_merged <- left_join(releases_labels_v1,
                                   release_IDs_labels_AS_merge[,c("release_mbid",
                                                                  "label_name",
                                                                  "type_name",
                                                                  "release_country",
                                                                  "release_year",
                                                                  "release_month",
                                                                  "release_day")],
                                   by = "release_mbid",
                                   suffix = c("_AD","_AS"))


#add in the release year for NA values of my observations for label, year, month and day

release_labels_merged$label_name_AS  <- ifelse(is.na(release_labels_merged$label_name_AS)==T,
                                                    release_labels_merged$label_name_AD,
                                                    release_labels_merged$label_name_AS)

release_labels_merged$release_year_AS <- ifelse(is.na(release_labels_merged$release_year_AS)==T,
                                                release_labels_merged$release_year_AD,
                                                release_labels_merged$release_year_AS)

release_labels_merged$release_month_AS <- ifelse(is.na(release_labels_merged$release_month_AS)==T,
                                                 release_labels_merged$release_month_AD,
                                                 release_labels_merged$release_month_AS)

release_labels_merged$release_day_AS <- ifelse(is.na(release_labels_merged$release_day_AS)==T,
                                               release_labels_merged$release_day_AD,
                                               release_labels_merged$release_day_AS)

#keep only my columns
release_labels_merged_final <- release_labels_merged %>%
  dplyr::select(isrc,
                track_id,
                track_title,
                artist_name,
                artist_mbid,
                release_mbid,
                release_group_mbid,
                release_country_AD,
                release_year_AS,
                release_month_AS,
                release_day_AS,
                label_name_AS,
                type_name) %>%
  rename(release_year = release_year_AS,
         release_month = release_month_AS,
         release_day = release_day_AS,
         release_country = release_country_AD,
         label_name = label_name_AS) %>% 
  #remove labels without a label name as these won't be assignable anyway
  filter(is.na(label_name) == F)

#remove the main release label column to save memory
rm(release_labels_merged)

#make a country indicator of is_us

release_labels_merged_final$is_US <- ifelse(release_labels_merged_final$release_country == "US",
                                                    1,
                                                    0)


# make country indiator for is EU
release_labels_merged_final$is_europe <- ifelse(release_labels_merged_final$release_country != "US",
                                                        1,
                                                        0)

#make a "is_EU" indicator - europe minus GB

release_labels_merged_final$is_EU <- ifelse(release_labels_merged_final$release_country == "US",
                                                    0,
                                                    ifelse(release_labels_merged_final$release_country == "GB", 0,1))

#.................................................................
##create a table with names for labels that release in two states
#..................................................................

#STEP 1: get names of the labels that operate in both regions
international_labels_table <- release_labels_merged_final %>% 
  group_by(label_name, is_US) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(label_name) %>% 
  dplyr::summarise(count = n()) %>% 
  filter(count > 1)

#STEP 2: filter the df_combined table by those label names and then create the new names
int_labels_table_new_names <- release_labels_merged_final %>% 
  filter(label_name %in% international_labels_table$label_name) %>% 
  rowwise() %>% 
  mutate(label_name_new = ifelse(is_US == 1,
                                 paste0(label_name,"_america"),
                                 paste0(label_name,"_international"))
  ) %>% 
  dplyr::select(label_name, is_US, label_name_new) %>% 
  distinct(label_name_new, .keep_all = T)

#STEP3: left join to original table

release_labels_merged_final <- left_join(release_labels_merged_final,
                         int_labels_table_new_names,
                         by= c("label_name", "is_US"))

#STEP4: ifelse NAs to label name
release_labels_merged_final$label_name_new <- ifelse(is.na(release_labels_merged_final$label_name_new) == T,
                                     release_labels_merged_final$label_name,
                                     release_labels_merged_final$label_name_new)

#STEP 5: create an indicator variable that is 0 if a label only operates domestically and 1 if abroad
release_labels_merged_final$is_international <- ifelse(release_labels_merged_final$label_name %in% international_labels_table$label_name,
                                                       1,
                                                       0)

#.................................................................
## create the major vs indie distinction - pro forma for now without sub labels
## starts with: UMG, Universal, Sony, WMG, Warner, BMG, PolyGram; includes EMI
#..................................................................

#STEP 1: Create a table of label names
label_table <- release_labels_merged_final %>% 
  group_by(label_name) %>% 
  dplyr::summarise(count = n()) %>% 
  
  #STEP 2: assign 1 or 0 binary indicator of whether a label is a major or indie
  
  mutate(is_major_label = ifelse(
    grepl("^UMG|^Universal|^Sony|^WMG|^Warner|^BMG|^PolyGram|EMI", label_name, ignore.case = FALSE),
    1, 0
  )) %>% 
  select(-count)

#STEP 3: merge in the dummy variable to the main dataset of relevance

release_labels_merged_final <-  left_join(release_labels_merged_final,
                                              label_table,
                                              by= "label_name")


#remove duplicate song names - arranging by release year
release_labels_merged_unique_tracks <- release_labels_merged_final %>%
  group_by(track_title, release_country) %>%
  arrange(track_title, is.na(release_year), release_year) %>%
  distinct(track_title, release_country, .keep_all = TRUE) %>%
  ungroup()


#.............
# Checks
#..................


glimpse(releases_labels_v1)

#recreate the unique release IDs table again
uniqe_releaes_IDs_labels_AD <- releases_labels_v1 %>%
  distinct(release_mbid, .keep_all = T)

#check na values
apply(release_labels_merged, 2, function(x){sum(is.na(x) == T)})

#sample random rows
random_sample_rows <- sample(length(release_labels_merged[,1]), 100)

random_sample_df <- release_labels_merged[random_sample_rows,]


#CHECK: count number of release countries for the unique release merged dataset
country_table_release_merged <- release_labels_merged_unique_tracks %>%
  group_by(release_country) %>%
  dplyr::summarise(count = n())

#check counts of songs of major vs indie labels
indie_v_major_songs <- release_labels_merged_final  %>% 
  group_by(is_major_label) %>% 
  dplyr::summarise(count = n())

#check songs count of international vs domestic only
international_v_domestic <- release_labels_merged_final %>% 
  group_by(is_international) %>% 
  dplyr::summarise(count = n())

#### ---------------------------- Create the mix/remix terms for all songs across all countries ----------------------- ####

## resulting term collection
terms <- c("remix", "mix", "dub", "edit", "radio", "rmx", "dirty", "club mix", "club version", "extended version", "extended mix", "instrumental", "acoustic", "version", "live")

# rename track title column to make analysis streamlined

release_labels_merged_unique_tracks <- release_labels_merged_unique_tracks %>%
  
  rename(song_title = track_title) %>%
  
  filter(is.na(song_title) == F)


# proceed to the remix analysis

release_labels_merged_unique_tracks <- release_labels_merged_unique_tracks %>%
  mutate(song_title_lower = tolower(song_title))

# Create new columns for each term
for (term in terms) {
  release_labels_merged_unique_tracks <- release_labels_merged_unique_tracks %>%
    mutate(!!sym(paste0(term, "_found")) := as.integer(str_detect(song_title_lower, paste0("\\b", term, "\\b"))))
}

# Create an "any_term" column
release_labels_merged_unique_tracks <- release_labels_merged_unique_tracks %>%
  mutate(any_term = as.integer(rowSums(select(., ends_with("_found"))) > 0))

#save the data frame as an .rda object
saveRDS(release_labels_merged_unique_tracks, here::here("data","interim_data","unique_tracks_mix_terms_countries_aom_2023.rda"))

#............
#checks
#............

apply(release_labels_merged_unique_tracks,2,function(x){sum(is.na(x) == T)})

#####------------------------- data preparation for the similarity analysis ---------------------------- #####

#...........................
# merge the datasets
#...........................

# remove duplicates from spotify based on spotify ids

spotify_audio_char_unique_id <- spotify_audio_characteristics_EU_US %>% 
  distinct(Spotify.ID, .keep_all = T) %>% 
  dplyr::select(!c(id, uri, track_href, analysis_url))

# remove duplicates from the labels data basedon ISRCs

release_labels_merged_unique_isrc <- release_labels_merged_final %>%
  group_by(isrc, release_country) %>%
  arrange(isrc, is.na(release_year), release_year) %>%
  distinct(isrc, release_country, .keep_all = TRUE) %>%
  ungroup() %>% 
  rename(ISRC = isrc)

# attach the song characteristics by ISRC

labels_country_audio_char <- left_join(release_labels_merged_unique_isrc,
                                       spotify_audio_char_unique_id,
                                       by = "ISRC")

#remove the rows that didn't find a match
labels_country_audio_char_naless <- labels_country_audio_char %>% 
  filter(is.na(mode) ==F)

#check what happens when I remove all spotify_ID + country combinations -- NOTHING
 #labels_country_audio_char_naless_unique <-  labels_country_audio_char_naless %>% 
   # distinct(Spotify.ID, release_country, .keep_all = T)

#check what happens when I sort by country, and a new column pastet together from song name, release_id and a number of spotify chars
## country to make sure I am accounting for rereleases in other countries
## release ID to account for some form of rerelease

labels_country_audio_char_naless$filter_column <- paste0(labels_country_audio_char_naless$track_title,
                                                        labels_country_audio_char_naless$release_mbid,
                                                        labels_country_audio_char_naless$danceability,
                                                        labels_country_audio_char_naless$energy,
                                                        labels_country_audio_char_naless$valence,
                                                        labels_country_audio_char_naless$acousticness,
                                                        labels_country_audio_char_naless$tempo,
                                                        labels_country_audio_char_naless$duration_ms,
                                                        labels_country_audio_char_naless$speechiness) 

labels_country_audio_char_naless_unique <- labels_country_audio_char_naless  %>% 
  group_by(filter_column, release_country) %>% 
  arrange(filter_column, is.na(release_year), release_year) %>%
  distinct(filter_column, release_country, .keep_all = T) %>% 
  ungroup() %>% 
  select(-type, -filter_column)

#........................
# create the similarity coefficients
#.......................

## turn key into dummies

# install.packages("fastDummies")
library(fastDummies)

# Create dummy variables for the 'key' column
labels_country_audio_char_naless_unique <- dummy_cols(labels_country_audio_char_naless_unique,
                                                      select_columns = "key",
                                                      remove_selected_columns = TRUE)

# Create dummy variables for the 'time_signature' column
labels_country_audio_char_naless_unique <- dummy_cols(labels_country_audio_char_naless_unique,
                                                      select_columns = "time_signature",
                                                      remove_selected_columns = TRUE)


## normalize the coefficients - help by chatgpt
# Columns to normalize
columns_to_normalize <- c("danceability", "energy", "loudness", "mode", "speechiness", 
                          "acousticness", "instrumentalness", "liveness", "valence", 
                          "tempo", "duration_ms")

# Applying min-max normalization
labels_country_audio_char_naless_unique[columns_to_normalize] <- lapply(labels_country_audio_char_naless_unique_test[columns_to_normalize], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})


# do the data processing in order to get cosine similarity by label and year - help by chatgpt

df_filtered <- labels_country_audio_char_naless_unique %>% 
  group_by(label_name_new, release_year) %>% 
  dplyr::summarise(count = n())

## cosine similarity

#install.packages("future")
#install.packages("future.apply")

library(tidyr)
library(proxy)
library(future)
library(future.apply)

# Step 1: Data Preprocessing
# Create a label-year indicator
labels_country_audio_char_naless_unique <- labels_country_audio_char_naless_unique %>%
  mutate(label_year = as.factor(paste(label_name_new, release_year, sep = "_")))

# Filter out label-year combinations with fewer than two songs
df_filtered <- labels_country_audio_char_naless_unique %>%
  group_by(label_year) %>%
  filter(n() > 2)

#create the label_year_id column
df_filtered <- df_filtered %>% 
  mutate(label_year = as.factor(paste(label_name_new, release_year, sep = "_"))) %>% 
  mutate(label_year_id = as.numeric(label_year))



# Step 2: Cosine Similarity Calculation
# Define a function to calculate aggregated similarity statistics within each group
calculate_similarity_stats <- function(df) {
  # Convert to matrix for faster computation
  relevant_columns <- as.matrix(df[, c("danceability", "energy", "loudness", "mode", "speechiness",
                                       "acousticness", "instrumentalness", "liveness", "valence",
                                       "tempo", "duration_ms", paste("key_", 0:11, sep=""),
                                       paste("time_signature_", c(0, 1, 3, 4, 5), sep=""))])
  
  # Calculate pairwise cosine similarity
  similarity_matrix <- proxy::simil(relevant_columns, method = "cosine")
  similarities <- similarity_matrix[upper.tri(similarity_matrix)]
  
  # Calculate statistics
  mean_similarity <- mean(similarities, na.rm = TRUE)
  median_similarity <- median(similarities, na.rm = TRUE)
  sd_similarity <- sd(similarities, na.rm = TRUE)
  cv_similarity <- sd_similarity / mean_similarity  # Coefficient of variation
  
  return(c(mean = mean_similarity, median = median_similarity, 
           sd = sd_similarity, cv = cv_similarity))
}

# Step 3: Parallel Processing
# Get the number of cores
plan(multisession)  # Use multisession plan for parallel processing

# Apply the function in parallel
split_output_check <- split(df_filtered, df_filtered$label_year)
similarity_results <- future_lapply(split_output_check, calculate_similarity_stats)

# Combine results into a data frame
similarity_results_df <- do.call(rbind, similarity_results)
similarity_results_df <- cbind(label_year = names(similarity_results), similarity_results_df)
similarity_results_df_transf <- as.data.frame(similarity_results_df, stringsAsFactors = FALSE)

# Remove row names for the dataframe
rownames(similarity_results_df_transf) <- NULL


# Step 4: do the merging process - create a dataframe grouped by label year with relevant variables for later analysis

label_year_similarity_df <- labels_country_audio_char_naless_unique %>% 
  group_by(release_year,
           label_name_new,
           label_name,
           is_US,
           is_international,
           is_major_label,
           label_year) %>% 
  summarise(n_songs = n())

#change to string
label_year_similarity_df$label_year <- as.character(label_year_similarity_df$label_year)

# merge in the similarity scores based on labels
label_year_similarity_df_merged <- left_join(label_year_similarity_df,
                                             similarity_results_df_transf,
                                             by = "label_year")

#save as a rds object
saveRDS(label_year_similarity_df_merged, here::here("data", "interim_data", "similarity_scores_label_years_aom_2023.rda"))

#.....
# trouble shooting
#........
split_output_check <- split(df_filtered, df_filtered$label_year_id)


#........
#Checks
#........

# check how many ISRCs are related to single Spotify ID
spotify_ids_to_isrcs <- spotify_audio_characteristics_EU_US %>% 
  group_by(Spotify.ID, ISRC) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(Spotify.ID) %>% 
  dplyr::summarise(count=n()) %>% 
  filter(count > 1)

#check the post merge NA count

apply(labels_country_audio_char, 2, function(x){sum(is.na(x))})

#CHECK: country count pre and post NA removal
country_count_pre <- labels_country_audio_char %>% 
  group_by(release_country) %>% 
  dplyr::summarise(count= n())



country_count <- labels_country_audio_char_naless %>% 
  group_by(release_country) %>% 
  dplyr::summarise(count = n())

country_count$pre_na_count <- country_count_pre$count

country_count$perc_change <- (country_count$count - country_count$pre_na_count)/country_count$count



#CHECK: count number of release countries for the unique release merged dataset
country_table_release_merged <- labels_country_audio_char_naless_unique %>%
  group_by(release_country) %>%
  dplyr::summarise(count = n())

#check counts of songs of major vs indie labels
indie_v_major_songs <- labels_country_audio_char_naless_unique  %>% 
  group_by(is_major_label) %>% 
  dplyr::summarise(count = n())

#check songs count of international vs domestic only
international_v_domestic <- labels_country_audio_char_naless_unique %>% 
  group_by(is_international) %>% 
  dplyr::summarise(count = n())


#check why some label_year_ids have an error
label_year_id_73 <- df_filtered %>% 
  filter(label_year_id == 73)
