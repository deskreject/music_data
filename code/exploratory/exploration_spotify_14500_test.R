#......................
# Description: Aimed at checking whether the results of the spotify api access worked better than the last try
# author: Alexander Staub
# date creation: 10.11.2023
#...............................

# investigate the data
glimpse(spotify_acoustic_char_v2)

#check for NAs - none
colSums(is.na(spotify_acoustic_char_v2))

# unique values of the columns
apply(spotify_acoustic_char_v2, 2, function(x){length(unique(x))})

#investigate the unique ISRCs more closely
unique_isrcs <- spotify_acoustic_char_v2 %>%
  group_by(ISRC) %>%
  summarise(occurrences = n())

#get the left join of artist names and song titles
lapply(seq_along(spotify_acoustic_char_14500), function(x){
  
  print(x)
})

####------------------ Plotting data -----------------------------####


# pivotting data
long_df <- pivot_longer(spotify_acoustic_char_v2,
                        cols = 3:13,
                        names_to = "variable",
                        values_to = "value")


#plot histograms
P
