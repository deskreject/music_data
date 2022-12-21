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
# Loading data
#.............

# The chart data filepath

filenames <- list.files(path = here("data", "raw_data", "hot_100"), pattern = "*.csv")

filenames2 <- paste0(here("data", "raw_data", "hot_100"), "/", filenames)

# reading in the file names

df <-  filenames2 %>% map_df(~read_delim(., delim = "r"))

