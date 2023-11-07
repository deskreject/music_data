#.................................
# Author: Alexander Staub
# Decription: Check out how many songs I find that match the ISRC between the MB data and the spotify data based on the ISRCs 
# date creation: 13.10.2023
#..................................


####------------------------------- merge spotify sample with the MB data ---------------------------------####

#create inner join between MB and Spotify data based on ISRC
nsync_inner_join <- inner_join(nsync_sample_df, df_musicbrainz_v6_isrcs, by=c("ISRC" = "isrc"))

#check unique artist and artist identifier matched to the spotify dataset from mb
janitor::tabyl(nsync_inner_join, artist_name, artist_mbid)

#check the unique song titles/how many obs are left after removing duplicates from innerjoin
nsync_inner_join_dist <- nsync_inner_join %>%
  distinct(name, .keep_all = T)


#do an antijoin between names of the nsync sample and innerjoin to identify the missing songs
nsync_antijoin <- anti_join(nsync_sample_df, nsync_inner_join, by="name")
