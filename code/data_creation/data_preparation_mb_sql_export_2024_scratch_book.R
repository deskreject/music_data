#.............................
# Author: Alexander Staub
# Description: Code to prepare the sql export data for further analysis
# Date created: 09.04.2024
# dependencies:
## script "packages_and_data" to load the df_mb_countries_labels dataset
#.............................

#####----------------------------- Exploratory checks - Country labels df -----------------------------#####

#create a table that contains the number of releases (id_release) by name_country
count_release_country <- df_mb_countries_labels %>% 
  group_by(name_country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#same table, just with the years 1998 - 2005
count_release_country_98_05 <- df_mb_countries_labels_98_05 %>% 
  group_by(name_country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#create a table of id_release where the count of the releases is greater than 7
count_release_country_98_05_gt7 <- df_mb_ct_lb_98_05_filtered %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  filter(n > 7) %>% 
  arrange(desc(n))

#create a count of name_label_type for the df_mb_ct_lb_98_05_filtered
count_label_type <- df_mb_ct_lb_98_05_filtered %>% 
  group_by(name_label_type) %>% 
  summarise(n = n()) %>% 
  #add a percentage of total column
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

count_label_type_unique <- df_mb_ct_lb_98_05_unique %>% 
  group_by(name_label_type) %>% 
  summarise(n = n()) %>% 
  #add a percentage of total column
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

#filter df_mb_ct_lb_98_05_filtered for the id_releases in the count_release_country_98_05_gt7 table, then group by label_type and country name, then count
count_label_type_duplicates <- df_mb_ct_lb_98_05_filtered %>% 
  filter(id_release %in% count_release_country_98_05_gt7$id_release) %>% 
  group_by(name_label_type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#create table with name_release_g_type of df_mb_ct_lb_98_05_unique
count_release_type_unique <- df_mb_ct_lb_98_05_unique %>% 
  group_by(name_release_g_type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#check release count of international vs domestic only
international_v_domestic <-df_mb_ct_lb_98_05_unique %>%
  group_by(is_international) %>% 
  dplyr::summarise(count = n())

# create table where id_release count is larger than 2 in the table df_mb_ct_lb_98_05_unique
count_release_gt2 <- df_mb_ct_lb_98_05_unique %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  filter(n > 2) %>% 
  arrange(desc(n))

##### ------- checking label level information ------------- #####

# get the distribution by country of releases for all labels except the label "[no label]"
count_label_country <- df_mb_ct_lb_98_05_unique %>% 
  filter(label_name != "[no label]") %>% 
  group_by(release_country) %>% 
  summarise(n = n()) %>%
  #calculate the percentages
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

#get the distribution by country of relesases by [no label]
count_label_country_no_label <- df_mb_ct_lb_98_05_unique %>% 
  filter(label_name == "[no label]") %>% 
  group_by(release_country) %>% 
  summarise(n = n()) %>%
  #calculate the percentages
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

#grab 100 random releases of the label [no label] and add to a sample_no_label table
sample_no_label <- df_mb_ct_lb_98_05_unique %>% 
  filter(label_name == "[no label]") %>% 
  sample_n(100)

#.........
# percentage of releaeses by region
#.........

# check the percentage of releases in US vs Europe for labels in international_labels_table
int_label_release_distribution <- df_mb_ct_lb_98_05_unique %>% 
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

#plot the distribution of percentages of US releases by label name
ggplot(int_label_release_distribution, aes(x = perc_US)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of US releases by label name",
       x = "Percentage of US releases",
       y = "Count of labels") +
  theme_minimal()

#the same plot, just removing observations with 50% US releases and 2 total releases (joint)
ggplot(int_label_release_distribution %>% filter(perc_US != 50 & total_count != 2), aes(x = perc_US)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of US releases by label name",
       x = "Percentage of US releases",
       y = "Count of labels") +
  theme_minimal()

#check labels that have between 40% and 60% US releases
close_calls <- int_label_release_distribution %>% 
  filter(perc_US != 50 & total_count != 2) %>%
  filter(perc_US >= 40 & perc_US <= 60)

#####----------------------------- Exploratory checks - song level data ----------------------------------------------#####

#count the number of duplicate entries based on the duplicate check column
count_duplicates <- df_mb_songs_all_filtered %>% 
  group_by(duplicate_check) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

# count unique release ids df_mb_songs_all_filtered
count_release_id_unique_all_songs <- df_mb_songs_all_filtered %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# count unique release ids for df_mb_songs_filtered_unique
count_release_id_unique <- df_mb_songs_filtered_unique %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#####----------------------------- Exploratory checks - merged dataset ----------------------------------------------#####

#create a table that counts the number of entries in duplicate check and filters for those greater than 1
count_duplicates_merged <- df_mb_ct_lb_songs_98_05_merged %>% 
  group_by(duplicate_check) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

#count number of nas by column in the df_mb_ct_lb_songs_98_05_merged dataframe
count_nas <- df_mb_ct_lb_songs_98_05_merged %>% 
  summarise_all(~sum(is.na(.)))

# count the number of unique id_release where id_recording = NA
count_id_release_na <- df_mb_ct_lb_songs_98_05_merged %>% 
  filter(is.na(id_recording)) %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#count the number of unique recording ids in df_mb_ct_lb_songs_98_05_merged
count_recording_id_unique <- df_mb_ct_lb_songs_98_05_merged %>% 
  group_by(id_recording) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#count the number of unique release ids
count_release_id_unique_merged <- df_mb_ct_lb_songs_98_05_merged %>% 
  group_by(id_release) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#check the release types of the merged dataset
count_release_type_merged <- df_mb_ct_lb_songs_98_05_merged %>% 
  group_by(release_type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#...........
# investigatoin of missing data
#...........

# subset the data where id_recording is NA
df_missing_recording <- df_mb_ct_lb_songs_98_05_merge...d %>% 
  filter(is.na(id_recording))

#create a table of countries for df_mb_ct_lb_98_05_unique_merge
count_country_98_05 <- df_mb_ct_lb_98_05_unique_merge %>% 
  group_by(release_country) %>% 
  summarise(n = n()) %>% 
  # add in the percentage of total column
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

#create a table of countries for the missing recordings
count_missing_recording_country <- df_missing_recording %>% 
  group_by(release_country) %>% 
  summarise(n = n()) %>% 
  # add in the percentage of total column
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

#create count of release_year for the missing recordings
count_missing_recording_year <- df_missing_recording %>% 
  group_by(release_year) %>% 
  summarise(n = n()) %>% 
  # add in the percentage of total column
  mutate(perc_total = round((n / sum(n) * 100),1)) %>%
  arrange(desc(n))

##### ---------------- Exploratory analysis of the panel data ----------------- #####

#count nas in region_song_artists_releases_lab_close_98_05 panel data by column
region_song_artists_releases_lab_close_98_05 %>% 
  summarise_all(~sum(is.na(.)))

##### ------------ Exploratory analysis of the merging in of artists to labels ------------ #####

#count the number of nas in the artist_credit_name column (7%)
count_na_artist_credit_name <- df_mb_countries_labels_artist %>% 
  filter(is.na(artist_credit_name)) %>% 
  dplyr::summarise(n = n())

#take a random sample of releases where artist_credit_name is NA
df_na_artist_credit_name <- df_mb_countries_labels_artist %>% 
  filter(is.na(artist_credit_name)) %>% 
  sample_n(100)

