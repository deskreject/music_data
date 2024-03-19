#.............................
# Author: Alexander Staub
# Description: File where base packages are loaded and where data files are loaded
# Date created: 06.12.2023
#.............................

#get unique release_mbids

unique_release_IDs_AD <- releases_labels_v1 %>%
  distinct(release_mbid) %>%
  rename(gid = release_mbid)

#save as a csv under incidental

write.csv(unique_release_IDs_AD, here::here("data", "incidental", "mb_sql_related", "release_IDs_AD.csv"))
