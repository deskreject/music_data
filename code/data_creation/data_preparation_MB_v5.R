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
df_musicbrainz_v5_no_isrc <- df_musicbrainz_v5[indicator_isrc,]

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

####------------------------------ Getting sample of the data with ISRCs ------------------------------####

#draw the random sample
n <- nrow(df_musicbrainz_v5_w_isrc) # Number of rows in the dataframe
sample_size <- 1000 # The number of rows you want to sample
random_rows <- sample(1:n, sample_size, replace = FALSE) # Random row numbers
random_sample_w_iscrcs_mb_v5 <- df_musicbrainz_v5_w_isrc[random_rows, ] # Subset the dataframe

#unpack the list of ISRCs
random_sample_w_iscrcs_mb_v5_export <- random_sample_w_iscrcs_mb_v5 %>%
  unnest(ISRC) %>%
  dplyr::select(Artist,
                artist,
                artist_credit_phrase,
                Artist_no_featuring,
                `song title`,
                `artist mbid`,
                ISRC,
                release,
                recording_id,
                country,
                date)

#save the random sample
write.csv(random_sample_w_iscrcs_mb_v5_export, here::here("data", "interim_data", "random_sample_w_iscrcs_mb_v5_export.csv"))

