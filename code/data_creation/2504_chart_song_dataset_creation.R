#.............................
# Author: Alexander Staub
# Description: Assembling songs from different chart datasets (and albums)
# Date created: 22.04.2025
#.............................

'''
Naming conventions:
  artist = name_artist_credit
  title = name_recording
  country = country (two letter abbriviations)
'''

#...........................
# loading required packages ####
#...........................

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(here)
library(haven) #for reading dta files


# France charts (1984) -----

## loading Christians chart data .dta ----------

#load a .dta file as a dataframe
df_christian_data <- read_dta(here("data", "raw_data", "christian_chart_data", "combined.dta"))

#check out the data

glimpse(df_christian_data)

#filter a dataset to only include the years from 1980 to 2000
df_christian_relevant_time <- df_christian_data %>%
  filter(year >= 1980, year <= 2000)


#create a table that is grouped by scountry and cat, summarising the number of observations by each group
summary_country_category <- df_christian_data %>%
  group_by(scountry, cat) %>%
  summarise(n = n(),
  year_min = min(year),
  year_max = max(year))

## get the the subset of the data for france -------------- 

# filter the data that is from France, category "s" and year between 1980 and 2000
df_christian_france <- df_christian_data %>%
  filter(scountry == "fr", cat == "s", year >= 1980, year <= 2000)

#create a new column that is the concatenation of the "artist" and "title" columns

df_christian_france <- df_christian_france %>%
  mutate(artist_song = paste(artist, title, sep = "_"))

#deduplicate the dataframe based on the "song_title" column
df_christian_france_deduplicated <- df_christian_france %>%
  distinct(artist_song, .keep_all = TRUE)

#retain only the columns of relevance
france_songs_1984_2000 <- df_christian_france_deduplicated %>%
  dplyr::select(title, artist, scountry, artist_song, label)

#rename the columns to make analysis streamlined
france_songs_1984_2000 <- france_songs_1984_2000 %>%
  rename(name_recording = title,
         name_artist_credit = artist,
        country = scountry)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(france_songs_1984_2000, here("data", "raw_data", "country_chart_data", "france_songs_1984_2000.csv"))

# US charts (Whitburn project) -----------

## hot100
library(readxl)

# Read sheet 2, only columns A through Y (which is column 25)
# cell_cols("A:Y") tells read_excel to only process these columns
whitburn_hot100 <- read_excel(
  path = here("data", "raw_data", "whitburn", "Billboard Pop ME (1890-2015) 20150328.xls"),
  sheet = 2,
  range = cell_cols("A:Y")
)

#limit the years between 1980 and 2000
#limit to the chart category "h" (hot100)
#create a field called song_artist that concatenates the song + artist
whitburn_hot100_relevant <- whitburn_hot100 %>%
  filter(Year >= 1980, Year <= 2000) %>%
  #filter(Source == h) %>%
  mutate(song_artist = paste(Track, Artist, sep = "_"))

#deduplicate based on the song + artist field

#keep columns of relevance (year, artist, title, label)
#add column "country" with "us"