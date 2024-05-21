#.............................
# Author: Alexander Staub
# Description: Code to prepare the sql export data for further analysis
# Date created: 09.04.2024
# dependencies:
## script "packages_and_data" to load the df_mb_countries_labels dataset
#.............................


# loading packages




####----------------------------- Preparing Country labels df -----------------------------####



#add in a column that is a concatentation of id_release, name_country and date_year called duplicate_check
df_mb_countries_labels_filtered <- df_mb_countries_labels %>% 
  mutate(duplicate_check = paste(id_release, name_country, date_year))

#........
# removing duplicates with priority for label types
#........

# Step 1: Assign priority to each name_label_type - lower numbers = higher priority
priority_mapping <- c("Reissue Production" = 10,
                      "Distributor" = 9,
                      "Publisher" = 8,
                      "Original Production" = 1,
                      "NULL" = 5,
                      "Imprint" = 2,
                      "Production" = 3,
                      "Holding" = 4,
                      "Bootleg Production" = 6,
                      "Manufacturer" = 7)

# Add a priority column to the dataframe
df_mb_countries_labels_filtered <- df_mb_countries_labels_filtered %>%
  mutate(priority = priority_mapping[name_label_type])

# Step 2: Sort the dataframe by duplicate_check and priority
df_mb_countries_labels_filtered <- df_mb_countries_labels_filtered %>%
  arrange(duplicate_check, priority)

# Step 3: Remove duplicates, keeping the first row of each duplicate set
df_mb_countries_labels_unique <- df_mb_countries_labels_filtered %>%
  group_by(duplicate_check) %>%
  slice_min(order_by = priority, with_ties = FALSE) %>%
  ungroup()

# Optional: You might want to remove the priority column if it's no longer needed
df_mb_countries_labels_unique <- df_mb_countries_labels_unique %>%
  select(-priority)

#...........
# additional adjustments
#...........

# remove rows with name_release_g_type == "broadcast" from the unique dataframe
df_mb_countries_labels_unique <- df_mb_countries_labels_unique %>% 
  filter(name_release_g_type != "broadcast")

# add in indicator variable is_US that = 1 if release_country = United states
df_mb_countries_labels_unique <- df_mb_countries_labels_unique %>% 
  mutate(is_US = ifelse(release_country == "United States", 1, 0)) %>%
  mutate(is_NorthA = ifelse(release_country %in% c("Canada", "United States"), 1, 0)) %>% 
  mutate(is_Europe = ifelse(release_country %in% c("Germany", "France", "Italy", "Spain", "United Kingdom"), 1, 0))

# rename columns to match conventions in prior scripts
df_mb_countries_labels_unique <- df_mb_countries_labels_unique %>% 
  rename(label_name = name_label,
         label_type = name_label_type,
         release_country = name_country,
         release_year = date_year,
         release_month = date_month,
         release_day = date_day,
         release_type = name_release_g_type)

#.................................................................
##create a table with names for labels that release in two states
#..................................................................

#STEP 1: get names of the labels that operate in both regions
international_labels_table <- df_mb_countries_labels_unique %>% 
  group_by(label_name, is_US) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(label_name) %>% 
  dplyr::summarise(count = n()) %>% 
  filter(count > 1)

#STEP 2: filter the df_combined table by those label names and then create the new names
int_labels_table_new_names <- df_mb_countries_labels_unique %>% 
  filter(label_name %in% international_labels_table$label_name) %>% 
  rowwise() %>% 
  mutate(label_name_new = ifelse(is_US == 1,
                                 paste0(label_name,"_america"),
                                 paste0(label_name,"_international"))
  ) %>% 
  dplyr::select(label_name, is_US, label_name_new) %>% 
  distinct(label_name_new, .keep_all = T)

#STEP3: left join to original table

df_mb_countries_labels_unique <- left_join(df_mb_countries_labels_unique,
                                         int_labels_table_new_names,
                                         by= c("label_name", "is_US"))

#STEP4: ifelse NAs to label name
df_mb_countries_labels_unique$label_name_new <- ifelse(is.na(df_mb_countries_labels_unique$label_name_new) == T,
                                                  df_mb_countries_labels_unique$label_name,
                                                  df_mb_countries_labels_unique$label_name_new)

#STEP 5: create an indicator variable that is 0 if a label only operates domestically and 1 if abroad
df_mb_countries_labels_unique$is_international <- ifelse(df_mb_countries_labels_unique$label_name %in% international_labels_table$label_name,
                                                       1,
                                                       0)


#....................
# label releases by label's primary market of operation
#.....................

# check the percentage of releases in US vs Europe for labels in international_labels_table
int_label_release_distribution <- df_mb_countries_labels_unique %>% 
  filter(label_name %in% international_labels_table$label_name) %>% 
  group_by(label_name, is_US) %>% 
  dplyr::summarise(count = n()) %>%
  group_by(label_name) %>% 
  dplyr::mutate(perc = count / sum(count) * 100,
                #add a total count variable by label
                total_count = sum(count)) %>%
  #filter to only include the perc. of US releases
  dplyr::filter(is_US == 1) %>% 
  dplyr::select(label_name, perc, total_count) %>% 
  dplyr::rename(perc_US = perc)

#need to create new label names column which coerces label names which have between 40 and 60% releases into _america when release _US ==1 and international otherwise

#STEP 1: create the table of label names
close_call_label_names <- int_label_release_distribution %>% 
  filter(perc_US >= 40 & perc_US <= 60) %>% 
  dplyr::select(label_name)

#STEP 2: filter the df_mb_ct_lb_98_05_unique dataframe to only include the label names in close_call_label_names

close_call_label_names_new <- df_mb_countries_labels_unique %>% 
  filter(label_name %in% close_call_label_names$label_name) %>% 
  rowwise() %>% 
  mutate(label_name_close = ifelse(is_US == 1,
                                 paste0(label_name,"_america"),
                                 paste0(label_name,"_international"))
  ) %>%
  dplyr::select(label_name, is_US, label_name_close) %>%
  distinct(label_name_close, .keep_all = T)

#STEP 3: left join to the original df
df_mb_countries_labels_unique <- left_join(df_mb_countries_labels_unique,
                                         close_call_label_names_new,
                                         by= c("label_name", "is_US"))

#STEP 4: ifelse NAs to label name
df_mb_countries_labels_unique$label_name_close <- ifelse(is.na(df_mb_countries_labels_unique$label_name_close) == T,
                                                  df_mb_countries_labels_unique$label_name,
                                                  df_mb_countries_labels_unique$label_name_close)

#STEP 5: create a lookup table of the labels with label_name_close that have 50% or more US releases
mainly_US_labels <- df_mb_countries_labels_unique %>% 
  group_by(label_name_close, is_US) %>%
  dplyr::summarise(count = n()) %>%
  group_by(label_name_close) %>%
  dplyr::mutate(perc = count / sum(count) * 100) %>%
  filter(is_US == 1 & perc >= 50) %>% 
  dplyr::select(label_name_close)
  


#STEP 6: then create a new column called _is_US_labels which is 1 if name is in mainly_US_labels
df_mb_countries_labels_unique <- df_mb_countries_labels_unique %>% 
  mutate(is_US_labels = ifelse(label_name_close %in% mainly_US_labels$label_name_close,
                               1,
                               0))


##### ------------------------------ Filtering down the label country df to countries and years -----------------------------####

#filter the dataframe to only inclue the years 1998 - 2005 
#filter the dataframe to only include the name_countries: "United States", "United Kingdom", "Canada", "Germany", "France", "Italy", "Spain"
df_mb_ct_lb_98_05_unique <- df_mb_countries_labels_unique %>% 
  filter(release_year >= 1998 & release_year <= 2005) %>% 
  filter(release_country %in% c("United States", "United Kingdom", "Canada", "Germany", "France", "Italy", "Spain")) %>% 
  filter(label_type != "broadcast")

#filter the dataframe to only include years 1987 - 1994
# filter the dataframe to only include the name_countries: "United States", "United Kingdom", "Canada", "Germany", "France", "Italy", "Spain"
df_mb_ct_lb_87_94_unique <- df_mb_countries_labels_unique %>% 
  filter(release_year >= 1987 & release_year <= 1994) %>% 
  filter(release_country %in% c("United States", 
                                #"United Kingdom", --> seems to have introduced the EPoS data at same time as US
                                "Canada", 
                                "Germany", 
                                "France", 
                                "Italy", 
                                "Spain")) %>% 
  filter(label_type != "broadcast")
  

#### ----------------------------- Preparing song level data -----------------------------####

# load the first dataframe of the song level information to check
df_mb_songs_v1 <- read.csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/release_recordings_unique_80_10_part_1.csv")
df_mb_songs_v2 <- read.csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/release_recordings_unique_80_10_part_2.csv")
df_mb_songs_v3 <- read.csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/release_recordings_unique_80_10_part_3.csv")
df_mb_songs_v4 <- read.csv("Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/release_recordings_unique_80_10_part_4.csv")


#row bind the dataframes together
df_mb_songs_all <- rbind(df_mb_songs_v1,
                         df_mb_songs_v2,
                         df_mb_songs_v3,
                         df_mb_songs_v4)

#remove the dataframes df_mb_songs_v1, df_mb_songs_v2, df_mb_songs_v3 
rm(df_mb_songs_v1, df_mb_songs_v2, df_mb_songs_v3, df_mb_songs_v4)
gc()

#remove the columns "name_medium_format", "id_track", "mbid_track", "isrc"
df_mb_songs_all_filtered <- df_mb_songs_all %>% 
  select(-c(name_medium_format, id_track, mbid_track))

#remove the df_mb_songs_all dataframe
rm(df_mb_songs_all)
gc()

#create a duplicate check column consisting of id_release, id_recording, id_artist_credit
df_mb_songs_all_filtered <- df_mb_songs_all_filtered %>% 
  mutate(duplicate_check = paste(id_release, id_recording))

#remove duplicates based on the duplicate check column
df_mb_songs_filtered_unique <- df_mb_songs_all_filtered %>% 
  distinct(duplicate_check, .keep_all = TRUE)



#### -------------------- merging songs by release id to the country label dataframe --------------------- ####

#create the list of the 2 dataframes from countries and labels
list_df_ct_lb <- list(df_mb_ct_lb_98_05_unique, 
                      df_mb_ct_lb_87_94_unique)

# name the list elements "remix" and "soundscan"
names(list_df_ct_lb) <- c("remix", "soundscan")

#remove redundant columns from the df_mb_ct_lb_98_05_unique dataframe - label_type, mbid_release, id_artist_credit, duplicate_check, is_international
list_df_ct_lb <- lapply(list_df_ct_lb, function(x) {
  x %>% 
  select(-c(label_type, mbid_release, id_artist_credit, duplicate_check
            #is_international
            ))
})

#left join the df_songs_filtered_unique to the df_mb_ct_lb_98_05_unique_merge dataframe based on id_release
list_df_ct_lb <- lapply(list_df_ct_lb, function(x) {
  left_join(x,
            df_mb_songs_filtered_unique,
            by = "id_release")
})

#create a new duplicate check column that consists of id_recording, id_release, release_country
#df_mb_ct_lb_songs_98_05_merged <- df_mb_ct_lb_songs_98_05_merged %>% 
 # mutate(duplicate_check = paste(id_recording, id_release, release_country))
#--> NO DUPLICATES FOUND


#create a dataframe at the label_name_new and release_year level, counting number of songs, number of artists, and retaining the _is columns
list_panel_ct_lb_songs_new <- lapply(list_df_ct_lb, function(x){

x %>%
  filter(id_recording != "NA") %>% 
  group_by(label_name_new, release_year) %>% 
  summarise(n_songs = n_distinct(id_recording),
            n_artists = n_distinct(id_artist_credit),
            n_releases = n_distinct(id_release),
            # count the number of albums (release_type == "album")
            n_albums = sum(release_type == "album"),
            is_US = max(is_US),
            is_NorthA = max(is_NorthA),
            is_Europe = max(is_Europe),
            is_international = max(is_international)) %>% 
  arrange(label_name_new, release_year)

})

#rename the list items
names(list_panel_ct_lb_songs_new) <- c("region_song_artists_releases_lab_new_98_05", 
                                       "region_song_artists_releases_lab_new_87_94")

#create the dataframe as above, using label_name_close as the grouping variable instead of label_name_new
list_panel_ct_lb_songs_close <- lapply(list_df_ct_lb, function(x){

x %>%
  filter(id_recording != "NA") %>% 
  group_by(label_name_close, release_year) %>% 
  summarise(n_songs = n_distinct(id_recording),
            n_artists = n_distinct(id_artist_credit),
            n_releases = n_distinct(id_release),
            n_albums = sum(release_type == "album"),
            is_US_labels = max(is_US_labels),
            is_NorthA = max(is_NorthA),
            is_Europe = max(is_Europe),
            is_internation = max(is_international)) %>% 
  arrange(label_name_close, release_year)

})

#rename the list items
names(list_panel_ct_lb_songs_close) <- c("region_song_artists_releases_lab_close_98_05", 
                                         "region_song_artists_releases_lab_close_87_94")

# the same without Canada
#region_song_artists_releases_98_05_no_canada <- df_mb_ct_lb_songs_98_05_merged %>%
 # filter(id_recording != "NA", release_country != "Canada") %>% 
  #group_by(label_name_new, release_year) %>% 
  #summarise(n_songs = n_distinct(id_recording),
   #         n_artists = n_distinct(id_artist_credit),
    #        n_releases = n_distinct(id_release),
     #       is_US = max(is_US),
      #      is_Europe = max(is_Europe)) %>% 
  # arrange(label_name_new, release_year)



#save the dataframe to an rds file under "data", "interim_data", "region_song_artist_releases_98_05.rda"
saveRDS(list_panel_ct_lb_songs_new[[1]],
        here::here("data", "interim_data", "region_song_artists_releases_lab_new_98_05.rda"))
# alternative region specification panel
saveRDS(list_panel_ct_lb_songs_close[[1]], 
        here::here("data", "interim_data", "region_song_artists_releases_lab_close_98_05.rda"))

#save the 87-94 data as .rda datasets
saveRDS(list_panel_ct_lb_songs_new[[2]],
        here::here("data", "interim_data", "region_song_artists_releases_lab_new_87_94.rda"))
# alternative region specification panel
saveRDS(list_panel_ct_lb_songs_close[[2]], 
        here::here("data", "interim_data", "region_song_artists_releases_lab_close_87_94.rda"))

#### -------------- Add in the artist names by artist_credit_id to the df_mb_countries_labels dataset ----------- #####

#filter down the df_mb_songs_filtered_unique dataframe to unique artist_credit_id and save dataframe with artist_credit_name

df_mb_songs_filtered_unique_artist <- df_mb_songs_all_filtered %>% 
  group_by(id_artist_credit) %>% 
  dplyr::summarise(artist_credit_name = first(name_artist_credit))

#left join the artist_credit_name to the df_mb_countries_labels dataset 
df_mb_countries_labels_artist <- left_join(df_mb_countries_labels,
                                           df_mb_songs_filtered_unique_artist,
                                           by = "id_artist_credit")

# save the dataframe as release_country_labels_80_10_w_artists as a csv to the folder Z:\Data_alexander\data\raw_data\musicbrainz\sql_exports
write.csv(df_mb_countries_labels_artist,
          "Z:/Data_alexander/data/raw_data/musicbrainz/sql_exports/release_country_labels_80_10_w_artists.csv")
