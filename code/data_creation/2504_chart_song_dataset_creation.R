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

# DE songs ---------

#load in the data
df_de_song_charts <- read_csv(here("data", "raw_data", "country_chart_data", "de_charts_1980_2000.csv"))

#remove the "album" entries
df_de_song_charts <- df_de_song_charts %>%
  filter(chart_type == "singles")

#create a column which is a concatenation of song_title and artist_name
df_de_song_charts <- df_de_song_charts %>%
  mutate(song_artist = paste(song_title, artist_name, sep = "_"))

#deduplicate the dataframe based on the song_artist column
df_de_song_charts <- df_de_song_charts %>%
  distinct(song_artist, .keep_all = TRUE)

#retain only the columns of relevance
df_de_songs_1980_2000 <- df_de_song_charts %>%
  dplyr::select(song_title, artist_name, country, song_artist, record_label)

#rename the columns to make analysis streamlined
df_de_songs_1980_2000 <- df_de_songs_1980_2000 %>%
  rename(name_recording = song_title,
         name_artist_credit = artist_name,
         label = record_label)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(df_de_songs_1980_2000, here("data", "raw_data", "country_chart_data", "de_songs_1980_2000.csv"))

# UK charts --------------

#load in the data
df_uk_charts <- read_csv(here("data", "raw_data", "country_chart_data", "uk_charts_1980_2000.csv"))

#remove the "album" entries
df_uk_charts <- df_uk_charts %>%
  filter(chart_type == "singles")

#create a column which is a concatenation of song_title and artist_name
df_uk_charts <- df_uk_charts %>%
  mutate(song_artist = paste(song_title, artist_name, sep = "_"))

#deduplicate the dataframe based on the song_artist column
df_uk_charts <- df_uk_charts %>%
  distinct(song_artist, .keep_all = TRUE)

#retain only the columns of relevance
df_uk_songs_1980_2000 <- df_uk_charts %>%
  dplyr::select(song_title, artist_name, song_artist) %>%
  #add a column with NAs called label and a column called "country" with "uk"
  mutate(label = NA,
         country = "uk")

#rename the columns to make analysis streamlined
df_uk_songs_1980_2000 <- df_uk_songs_1980_2000 %>%
  rename(name_recording = song_title,
         name_artist_credit = artist_name)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(df_uk_songs_1980_2000, here("data", "raw_data", "country_chart_data", "uk_songs_1980_2000.csv"))

# IT charts --------------

#load in the data
df_it_charts <- read_csv(here("data", "raw_data", "country_chart_data", "it_charts_1980_2000.csv"))

# create a column which is a concatenation of song_title and artist_name
df_it_charts <- df_it_charts %>%
  mutate(song_artist = paste(song_title, artist_name, sep = "_"))

#deduplicate the dataframe based on the song_artist column
df_it_charts <- df_it_charts %>%
  distinct(song_artist, .keep_all = TRUE)

#retain only columns of relevance
df_it_songs_1980_2000 <- df_it_charts %>%
  dplyr::select(song_title, artist_name, song_artist) %>%
  #add a column with NAs called label and a column called "country" with "it"
  mutate(label = NA,
         country = "it")

#rename the columns to make analysis streamlined
df_it_songs_1980_2000 <- df_it_songs_1980_2000 %>%
  rename(name_recording = song_title,
         name_artist_credit = artist_name)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(df_it_songs_1980_2000, here("data", "raw_data", "country_chart_data", "it_songs_1980_2000.csv"))

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
#limit to the chart category "a" -> essentially hot100 for my time period (h only for a short time period)
#create a field called song_artist that concatenates the song + artist
whitburn_hot100_relevant <- whitburn_hot100 %>%
  filter(Year >= 1980, Year <= 2000) %>%
  filter(Source == "a") %>%
  mutate(song_artist = paste(Track, Artist, sep = "_"))

#deduplicate based on the song + artist field
whitburn_hot100_relevant <- whitburn_hot100_relevant %>%
  distinct(song_artist, .keep_all = TRUE)


#keep columns of relevance (year, artist, title, label)
#add column "country" with "us"
whitburn_hot100_relevant <- whitburn_hot100_relevant %>%
  dplyr::select( Artist, Track, song_artist, "Label/Number") %>%
  mutate(country = "us")

#rename the columns to make analysis streamlined
whitburn_hot100_relevant <- whitburn_hot100_relevant %>%
  rename(label = "Label/Number",
         name_artist_credit = Artist,
         name_recording = Track
)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(whitburn_hot100_relevant, here("data", "raw_data", "country_chart_data", "us_BB_songs_1980_2000.csv"))

## Checks of the data -------

#Create a table of entries by year of the "source" variable

source_counts <- whitburn_hot100 %>%
  group_by(Source, Year) %>%
  summarize(count = n())


# US charts (Cashbox charts) -----------

#packages
library(readxl)

#load the data
cashbox_pop <- read_excel(
  path = here("data", "raw_data", "whitburn", "Cash Box Pop Charts (1944-1996).xlsx"),
  sheet = 1,
  range = cell_cols("B:S")
)

#transform the "Date Entered" variable from a string to a date format, currently as "YYYY-MM-DD"
cashbox_pop <- cashbox_pop %>%
  mutate(Date_Entered = as.Date(`Date Entered`, format = "%Y-%m-%d"))

#limit the years between 1980 and 2000 and create the song_artist deduplication column
cashbox_pop_relevant <- cashbox_pop %>%
  filter(Date_Entered >= "1980-01-01", Date_Entered <= "2000-01-01") %>%
  mutate(song_artist = paste(`Song Title`, `Artist (as appears on Label)`, sep = "_"))

#deduplicate based on the song + artist field
cashbox_pop_relevant <- cashbox_pop_relevant %>%
  distinct(song_artist, .keep_all = TRUE)

#keep the variables: Song Title, Artist, Label, Year (extracted from Date Entered)
cashbox_pop_unique <- cashbox_pop_relevant %>%
  mutate(Year = year(Date_Entered)) %>%
  dplyr::select(`Song Title`, `Artist (as appears on Label)`, `Label/Number`, song_artist) %>%
  mutate(country = "us")

#rename the columns so they match those of the previous dataframes
cashbox_pop_unique <- cashbox_pop_unique %>%
  rename(name_recording = `Song Title`,
         name_artist_credit = `Artist (as appears on Label)`,
         label = `Label/Number`)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(cashbox_pop_unique, here("data", "raw_data", "country_chart_data", "us_CB_songs_1980_2000.csv"))

# Compiling all country specific data to single dataframe -----------

#load all the dataframes of DE, FR, UK, IT, US
df_de_songs_1980_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "de_songs_1980_2000.csv"))
df_fr_songs_1984_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "france_songs_1984_2000.csv"))
df_it_songs_1980_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "it_songs_1980_2000.csv"))
df_uk_songs_1980_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "uk_songs_1980_2000.csv"))
df_us_songs_1980_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "us_songs_1980_2000.csv"))
df_us_CB_songs_1980_2000 <- read_csv(here("data", "raw_data", "country_chart_data", "us_CB_songs_1980_2000.csv"))

#combine all dataframes into one
df_all_songs_1980_2000 <- bind_rows(df_de_songs_1980_2000,
   df_fr_songs_1984_2000, 
   df_it_songs_1980_2000, 
   df_uk_songs_1980_2000, 
   df_us_songs_1980_2000,
   df_us_CB_songs_1980_2000)

#create trasnformed columns
#tf_name_recording, tf_name_artist_credit, tf_artist_song
#transformation makes all lower case and removes white space
df_all_songs_1980_2000 <- df_all_songs_1980_2000 %>%
  mutate(tf_name_recording = str_to_lower(name_recording),
         tf_name_artist_credit = str_to_lower(name_artist_credit),
         tf_artist_song = str_to_lower(paste(name_artist_credit, name_recording, sep = "_")))

#remove any duplicates based on the tf_artist_song column
df_all_songs_1980_2000_deduplicated <- df_all_songs_1980_2000 %>%
  distinct(tf_artist_song, .keep_all = TRUE)

#save the dataframe as a .csv file under data/raw_data/country_chart_data
write_csv(df_all_songs_1980_2000_deduplicated, here("data", "raw_data", "country_chart_data", "all_charts_songs_1980_2000.csv"))
