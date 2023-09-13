#.................................
# Author: Alexander Staub
# Decription: Glimpse into the data of the 5th export, which should include isrc codes
# date creation: 05.09.2023
#..................................

#### ---------------------- Prechecks of the data -------------------------------------#####

## ISRC investigation

# Generate a table of frequencies for the lengths of list elements in var1
table(sapply(df_musicbrainz_v5$ISRC, function(x) if (is.null(x)) 0 else length(x)))


# Count empty or NULL lists for ISRC column
sum(sapply(df_musicbrainz_v5$ISRC, function(x) is.null(x) || length(x) == 0))

# spot check 8+ isrc
indicator_isrc_8 <- sapply(df_musicbrainz_v5$ISRC, function(x) length(x) > 7)

df_musicbrainz_v5_w_isrc_8 <- df_musicbrainz_v5[indicator_isrc_8,]

df_musicbrainz_v5_w_isrc_8$ISRC

# create an indicator variable of the elements where length of isrc == 0
indicator_isrc <- sapply(df_musicbrainz_v5$ISRC, function(x) is.null(x) || length(x) == 0)

#filter the dataframe down to elements that do include an ISRC
df_musicbrainz_v5_w_isrc <- df_musicbrainz_v5[!indicator_isrc,]

#filter dataframe down to elements that don't include an ISRC
df_musicbrainz_v5_no_isrc <- df_musicbrainnz_v5[indicator_isrc,]

#investigate the number of MBIDs with ISRCs

df_musicbrainz_v5_w_isrc_artists <- df_musicbrainz_v5_w_isrc %>%
  group_by(`artist mbid`, Artist_no_featuring) %>%
  summarise(occurrences = n())

#investigate number of MBIDs witout ISRCs

df_musicbrainz_v5_no_isrc_artists <- df_musicbrainz_v5_no_isrc %>%
  group_by(`artist mbid`, Artist_no_featuring) %>%
  summarise(occurrences = n())

antijoin_artists_no_w_isrcs <- anti_join(df_musicbrainz_v5_no_isrc_artists,
                                         df_musicbrainz_v5_w_isrc_artists,
                                         by="artist mbid")

#......................
## Genre investigation
#......................

# generate a table of frequencies for the number of genres for all
table_genres <- table(sapply(df_musicbrianz_v5$genre, function(x) if (is.null(x)) 0 else length(x)))

# generate a table of frequencies for number of genres for songs with ISRCs
table_genres_w_isrcs <- table(sapply(df_musicbrainz_v5_w_isrc$genre, function(x) if (is.null(x)) 0 else length(x)))
