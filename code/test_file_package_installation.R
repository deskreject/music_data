#.............................
# Author: Alexander Staub
# Description: Code to prepare the sql export data for further analysis
# Date created: 09.04.2024
# dependencies:
## script "packages_and_data" to load the df_mb_countries_labels dataset
#.............................

#install specific packages for this script
install.packages("haven")

#do this if a new (useful) package was installed
renv::snapshot()

# loading packages
library("haven") # package for reading wide range of dta file formats
library("here")
library("tidyerse")
library("dplyr")


#load the dataframe
here::here()
