#.............................
# Author: Alexander Staub
# Description: File where base packages are loaded and where data files are loaded
#.............................

#...............
# INstalling Base packages
#..............


# if (!require(readr)) install.packages("readr"); library(readr) # reading csv files
# if (!require(renv)) install.packages("renv"); library(renv) # environment creator package
# if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) # tidyverse
# if (!require(here)) install.packages("here"); library(here) # relative file paths
# if (!require(stringr)) install.packages("stringr"); library(stringr) # string package
# if (!require(lubridate)) install.packages("lubridate"); library(lubridate) # package for dealing with date-type format
# if (!require(psych)) install.packages("psych"); library(psych) # get the describe function
# if (!require(gridExtra)) install.packages("gridExtra"); library(gridExtra) # get the describe function
# if (!require(jsonlite)) install.packages("jsonlite"); library(jsonlite) # read json data
# if (!require(janitor)) install.packages("janitor"); library(janitor) # mainly for the tabyl package


# initiate r environment - only once
# renv::init()

# if code runs, create a new snapshot
# renv::snapshot()

#..........................................
# loading base packages
#..........................................

library(readr) # 
library(renv) # 
library(tidyverse) 
library(here) # 
library(stringr) # string
library(lubridate) # 
library(psych) # 
library(gridExtra) # 
library(jsonlite) # 
library(janitor) # 

#.............
# Loading  raw data
#.............

# The chart data filepath - takes forever, do with caution

  # filenames <- list.files(path = here("data", "raw_data", "hot_100"), pattern = "*.csv")

   #filenames2 <- paste0(here("data", "raw_data", "hot_100"), "/", filenames)

# reading in the file names

  # df <-  filenames2 %>% map_df(~read_delim(., delim = "r"))


#................
# main mb data files
#................

# read in csv from musibrainz - export #1

df_musicbrainz <- read.csv(here("data", "raw_data", "musicbrainz", "all_songs_artists_with_id.csv"))


# read in csv from musicbrainz - export #2

df_musicbrainz_v2 <- read.csv(here("data", "raw_data", "musicbrainz", "MB_export_artists_songs_dates_v2.csv"))

# read in csv from musicbrainz - export #3 with duplicates

df_musicbrainz_v3_no_dates <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_V2_correct_with_names.json"))

# read in the csv from musicbrainz - export #3 without duplicates from Alessio

df_musicbrainz_v3_no_dates_dupl <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_V2_no_dup_with_names.json"))

# read in the csv from musicbrainz - export #3 with dates from Alessio

df_musicbrainz_v3 <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_v3_with_info100.json"))

# read in the musicbrainz data - export #4, according to the song level similarity with discography process to capture sufficient artists

df_musicbrainz_v4_original <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_v4_with_dates.json"))

# read in the musicbrainz data - export #5, based on export #4 but including additional irsc variable

df_musicbrainz_v5 <- fromJSON(here::here("data", "raw_data", "musicbrainz", "df_all_songs_v5_ircs.json"))

# read in the musicbrainz data which includes US as country, the date range of interest and the ISRCs for songs
df_musicbrainz_v6_isrcs <- fromJSON(here::here("data", "raw_data", "musicbrainz","isrcs_from_release_with_label_v6.json"))

#.................................
# mb label to releases information
#.................................

#labels associated to ISRCs for DE, GB, Fr, US, IT, v1 
releases_labels_v1 <- read.csv(here::here("data", "raw_data", "musicbrainz", "label_information", "releases_labels_v1_AD.csv"))

#my export from sql (12.2023) 
release_labels_AS_v1 <- read.csv(here::here("data", "interim_data", "mb_sql_tables", "labelnames_type_country_date_AD.csv"))

#......................
# Spotify data
#......................

#Axel's sample data
nsync_sample_df <- read.csv(here::here("data", "raw_data", "Spotify", "nsync_axel_sample.csv"))

#Alessios test data after failed first attempt - 10k
spotify_acoustic_char_14500 <- fromJSON(here::here("data", "raw_data", "Spotify", "AD_spotify_acoustic_char_14500.json"))

#Alessio follow up to test data - 160k
spotify_acoustic_char_v2 <- fromJSON(here::here("data", "raw_data", "Spotify", "AD_spotify_acoustic_char_v2.json"))

#27.11.2023 Alessios test data
#import the list that was transformed from json to csv that includes isrcs
isrc_spotify_id_check_data <- read.csv(here::here("data", "incidental", "spotify_related", "last_step_song_characteristics_df_sample_post_test_1.csv"))

#30.11.2023 Alessio's full US data as csv
spotify_audio_characteristics <- read.csv(here::here("data", "raw_data", "Spotify", "AD_spotify_accoustic_char_250k.csv"))

#08.12.2023 Alessio's full US + EU data as csv
spotify_audio_characteristics_EU_US <- read.csv((here::here("data", "raw_data", "Spotify", "AD_spotify_acoustic_char_US_EU.csv")))

#.......................
# loading processed data
#.......................

#load the hot100 df created to do the initial scrape of musicbrainz: year >= 1999, <= 2005

df_hh_proc <- read.csv(here("data", "interim_data", "df_songs_relevant_time.csv")) 
                           

#...................
# supplementary data
#...................

# load the collection of artists with mbids for a spot check
artist_mbids_v4 <-  read.csv(here("data", "raw_data", "musicbrainz", "artist_mbids_v4.csv"))

#load the artists that did not match based on v3 export
hot100_nomatch_artists <- read.csv(here("data", "incidental", "hot100_nomatch_artists_v3.csv"))
