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

# initiate r environment - only once
# renv::init()

# if code runs, create a new snapshot
# renv::snapshot

#.............
# Loading  raw data
#.............

# The chart data filepath

filenames <- list.files(path = here("data", "raw_data", "hot_100"), pattern = "*.csv")

filenames2 <- paste0(here("data", "raw_data", "hot_100"), "/", filenames)

# reading in the file names

df <-  filenames2 %>% map_df(~read_delim(., delim = "r"))

# read in the csv from musicbrainz

df_musicbrainz <- read.csv(here("data", "raw_data", "musicbrainz", "all_songs_artists.csv"))

#.......................
# loading processed data
#.......................

#load the hot100 df created to do the initial scrape of musicbrainz: year >= 1999, <= 2005

df_hh_proc <- read.csv(here("data", "interim_data", "df_songs_relevant_time.csv")) 
                           