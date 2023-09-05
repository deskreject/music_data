#.................................
# Author: Alexander Staub
# Decription: Glimpse into the data of the 5th export, which should include isrc codes
# date creation: 05.09.2023
#..................................

#### ---------------------- Prechecks of the data -------------------------------------#####

## ISRC investigation

# Generate a table of frequencies for the lengths of list elements in var1
table(sapply(df_musicbrianz_v5$ISRC, function(x) if (is.null(x)) 0 else length(x)))


# Count empty or NULL lists for ISRC column
sum(sapply(df_musicbrianz_v5$ISRC, function(x) is.null(x) || length(x) == 0))

# create an indicator variable of the elements where length of isrc == 0
indicator_isrc <- sapply(df_musicbrianz_v5$ISRC, function(x) is.null(x) || length(x) == 0)

#filter the dataframe down to elements that don't include an ISRC
df_musicbrainz_v5_w_isrc <- df_musicbrianz_v5[!indicator_isrc,]

# spot check 8+ isrc
indicator_isrc_8 <- sapply(df_musicbrianz_v5$ISRC, function(x) length(x) > 7)

df_musicbrainz_v5_w_isrc_8 <- df_musicbrianz_v5[indicator_isrc_8,]

df_musicbrainz_v5_w_isrc_8$ISRC

## Genre investigation

