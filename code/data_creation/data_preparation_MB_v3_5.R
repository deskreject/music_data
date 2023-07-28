#..............
# Description: Processing of after the 3rd MB export - aim to get all hh artists' releases (including back catalog)
# Author: Alexander Staub
# Date: 28.06.2023
#.................

###### ------------------------- the artist to mbid pre check ----------------------######

#remove duplicate MBIDs
artist_mbids_v4_no_dupl <- artist_mbids_v4 %>%
  dplyr::distinct(artist_mbid, .keep_all = T)

#make the column lower
artist_mbids_v4_no_dupl$artist_no_featuring_lower <- tolower(artist_mbids_v4_no_dupl$Artist_no_featuring)

#remove special non-alpha chars from both
artist_mbids_v4_no_dupl <- artist_mbids_v4_no_dupl %>%
  mutate(artist_no_featuring_lower_no_spec = tolower(Artist_no_featuring) %>%  # Convert to lower case
                                    stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
                                    str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
                                    str_trim())   # Remove leading/trailing whitespace

hot100_nomatch_artists <- hot100_nomatch_artists %>%
  mutate(artist_no_featuring_lower_no_spec = tolower(artist_no_featuring_lower ) %>%  # Convert to lower case
           stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents from characters
           str_replace_all("[^[:alnum:] ]", "") %>% # Remove special characters
           str_trim())   # Remove leading/trailing whitespace

#match MBIDs to the artists that did not match the last time
hot100_nomatch_artists_w_mbids <- left_join(hot100_nomatch_artists,
                                            artist_mbids_v4_no_dupl[,c("artist_no_featuring_lower_no_spec",
                                                                       "artist_name_mb", 
                                                                       "Artist",
                                                                       "artist_mbid",
                                                                       "Track")],
                                            by ="artist_no_featuring_lower_no_spec")
# check the NAs
hot100_mbid_nas <- hot100_nomatch_artists_w_mbids %>%
  filter(is.na(artist_mbid) ==T)
