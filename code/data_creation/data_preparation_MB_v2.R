#..............................
# Author: Alexander Staub
# Description: preparing the v2 data export of MB
#....................................................

#...........................................................
# MB specific adjustments - like date, duplication removal
#...........................................................

## create copy of original

df_musicbrainz_original <- df_musicbrainz_v2

# Determine the date format for each observation in the release_date column
date_formats <- sapply(df_musicbrainz_v2$release_date, function(x) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return("yyyy-mm-dd")
  } else if (grepl("^\\d{4}$", x)) {
    return("yyyy")
  } else if (grepl("^\\d{4}-\\d{2}$", x)) {
    return("yyyy-mm")
  } else if (x == "") {
    return("blank")
  } else {
    return("unknown")
  }
})

# Function to convert dates to the "yyyy" format
convert_to_yyyy <- function(x) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x) || grepl("^\\d{4}-\\d{2}$", x)) {
    return(substr(x, 1, 4))
  } else if (grepl("^\\d{4}$", x)) {
    return(x)
  } else if (x == "") {
    return(NA)
  } else {
    return("unknown")
  }
}

# Convert all non-blank columns into a "yyyy" date format
df_musicbrainz_v2 <- df_musicbrainz_v2 %>%
  mutate(release_year = sapply(release_date, convert_to_yyyy))

# DONT WORK CORRECTLY: Convert the "yyyy" format to a date format
## df_musicbrainz_v2$release_year <- ifelse(is.na(df_musicbrainz_v2$release_year), NA, ymd(paste0(df_musicbrainz_v2$release_year, "-01-01")))

# Check for duplicates

song_id_duplicates <- table(df_musicbrainz_v2$song_mbid)

print(song_id_duplicates)

#spot checks

spot_check_wiseguys <- df_musicbrainz_v2 %>% filter(song_mbid == "03d78354-d5cf-4732-887a-7f0583e3d2ba")

spot_check_scott <- df_musicbrainz_v2 %>% filter(song_mbid == "01a20123-4628-4712-b86c-f8cf75b87877")
