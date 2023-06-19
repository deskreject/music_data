#.............................
# Author: Alexander Staub
# Description: File where base packages are loaded and where data files are loaded
#.............................

#...............
# Base packages
#..............


if (!require(renv)) install.packages("renv"); library(renv) # environment creator package
if (!require(readr)) install.packages("readr"); library(readr) # reading csv files
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) # tidyverse
if (!require(here)) install.packages("here"); library(here) # relative file paths
if (!require(stringr)) install.packages("stringr"); library(stringr) # string package
if (!require(lubridate)) install.packages("lubridate"); library(lubridate) # package for dealing with date-type format
if (!require(psych)) install.packages("psych"); library(psych) # get the describe function
if (!require(gridExtra)) install.packages("gridExtra"); library(gridExtra) # get the describe function
if (!require(jsonlite)) install.packages("jsonlite"); library(jsonlite) # read json data


# initiate r environment - only once
# renv::init()

# if code runs, create a new snapshot
# renv::snapshot

#.............
# Loading  raw data
#.............

# The chart data filepath - takes forever, do with caution

  # filenames <- list.files(path = here("data", "raw_data", "hot_100"), pattern = "*.csv")

   #filenames2 <- paste0(here("data", "raw_data", "hot_100"), "/", filenames)

# reading in the file names

  # df <-  filenames2 %>% map_df(~read_delim(., delim = "r"))



# read in csv from musibrainz - export #1

df_musicbrainz <- read.csv(here("data", "raw_data", "musicbrainz", "all_songs_artists_with_id.csv"))


# read in csv from musicbrainz - export #2

df_musicbrainz_v2 <- read.csv(here("data", "raw_data", "musicbrainz", "MB_export_artists_songs_dates_v2.csv"))

# read in csv from musicbrainz - export #3 with duplicates

df_musicbrainz_v3 <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_V2_correct_with_names.json"))

# read in the csv from musicbrainz - export #3 without duplicates from Alessio

df_musicbrainz_v3_no_dupl <- fromJSON(here::here("data", "raw_data", "musicbrainz", "all_songs_V2_no_dup_with_names.json"))


#.......................
# loading processed data
#.......................

#load the hot100 df created to do the initial scrape of musicbrainz: year >= 1999, <= 2005

df_hh_proc <- read.csv(here("data", "interim_data", "df_songs_relevant_time.csv")) 
                           