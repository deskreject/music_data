#.............................
# Author: Alexander Staub
# Description: Assembling songs from different chart datasets (and albums)
# Date created: 22.04.2025
#.............................

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


# loading Christians chart data .dta ----------

#load a .dta file as a dataframe
df_christian_data <- read_dta(here("data", "raw_data", "christian_chart_data", "combined.dta"))

#check out the data

glimpse(df_christian_data)

#filter a dataset to only include the years from 1980 to 2000
df_christian_relevant_time <- df_christian_data %>%
  filter(year >= 1980, year <= 2000)


#create a table that is grouped by scountry and cat, summarising the number of observations by each group
summary_country_category <- df_christian_data %>%
  group_by(scountry, cat, year) %>%
  summarise(n = n())


## a sub section -----------
