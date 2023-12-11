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
         label_name = label_name_AS)

#remove the main release label column to save memory
rm(release_labels_merged)

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
