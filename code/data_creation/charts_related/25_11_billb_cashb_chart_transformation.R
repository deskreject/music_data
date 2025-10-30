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
  record label = label
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
library(readxl)

#...............................
# Loading the datasets of relevance
#................................

#load the data
cashbox_pop <- read_excel(
  path = here("data", "raw_data", "whitburn", "Cash Box Pop Charts (1944-1996).xlsx"),
  sheet = 1
  #range = cell_cols("B:S")
)

whitburn_hot100 <- read_excel(
  path = here("data", "raw_data", "whitburn", "Billboard Pop ME (1890-2015) 20150328.xls"),
  sheet = 2
  #range = cell_cols("A:Y")
)

# Data wrangling: Cashbox charts ------------------------------------------------------------------------------- 

#transform the "Date Entered" variable from a string to a date format, currently as "YYYY-MM-DD"
cashbox_pop <- cashbox_pop %>%
  mutate(Date_Entered = as.Date(`Date Entered`, format = "%Y-%m-%d"))

#limit the years between 1980 and 2000 and create the song_artist deduplication column
cashbox_pop_relevant <- cashbox_pop %>%
  filter(Date_Entered >= "1980-01-01", Date_Entered <= "2000-01-01")

#save a df with the variables for the chart positioning
cashbox_pop_chart_pos <- cashbox_pop_relevant[39:93] 

#transform all values of all columne of the _pos dataframe to strings
cashbox_pop_chart_pos <- cashbox_pop_chart_pos %>% mutate(across(everything(), as.integer))



#save the relevant variables:
cashbox_pop_vars <- cashbox_pop_relevant %>%
  dplyr::select(Date_Entered,
  `Song Title`,
`Artist (as appears on Label)`,
`Label/Number`) %>%
  # change the names of the columns to match the key up top
  rename(name_recording = `Song Title`,
         name_artist_credit = `Artist (as appears on Label)`,
         label = `Label/Number`) %>%
  #add the country column
  mutate(country = "us_cb")

#columnbind the vars and pos dataset
cashbox_pop_charts_relevant <- bind_cols(cashbox_pop_vars, cashbox_pop_chart_pos)

# Cashbox chart pivot longer -------------------------------------------------------####

# Convert Date Entered to proper date format
chart_data <- cashbox_pop_charts_relevant %>%
  rename(Date.Entered = Date_Entered)

# PIVOT: Transform from wide to long format
# values_drop_na = TRUE will automatically handle the sparse data
# (removes all the NA values from weeks when songs weren't charting)
long_data_cashbox <- chart_data %>%
  pivot_longer(
    cols = matches("^\\d+(st|nd|rd|th)$"),  # Selects: 1st, 2nd, 3rd, ..., 55th
    names_to = "week_number",
    values_to = "chart_position",
    values_drop_na = TRUE  # KEY: This removes all NA values efficiently
  ) %>%
  # Extract week number and calculate chart week date
  mutate(
    week_num = parse_number(week_number),
    # Chart week = Date Entered + (week number - 1) weeks
    chart_week = Date.Entered + weeks(week_num - 1)
  ) %>%
  # Clean up and reorder columns
  select(
    chart_week,
    chart_position,
    name_recording,
    name_artist_credit,
    Date.Entered,
    label,
    country,
    week_num,
    everything(),
    -week_number
  ) %>%
  arrange(chart_week, chart_position)

cat("=== POST-PIVOT DATA INFO ===\n")
cat("Total song-week entries:", nrow(long_data), "\n")
cat("Date range:", format(min(long_data$chart_week, na.rm = TRUE), "%Y-%m-%d"), 
    "to", format(max(long_data$chart_week, na.rm = TRUE), "%Y-%m-%d"), "\n\n")

#=====================================
# Data wrangling: Billboard charts -----------------------------------------------
#=====================================

#transform the "Date Entered" variable from a string to a date format, currently as "YYYY-MM-DD"
billboard_charts <- whitburn_hot100 %>%
  mutate(Date_Entered = as.Date(`Date Entered`, format = "%Y-%m-%d"))

#limit the years between 1980 and 2000 and create the song_artist deduplication column
billboard_charts_relevant <- billboard_charts %>%
  filter(Date_Entered >= "1980-01-01", Date_Entered <= "2000-01-01",
         Source == "a")

#save a df with the variables for the chart positioning
billboard_charts_pos <- billboard_charts_relevant[35:125] 

#transform all values of all columne of the _pos dataframe to strings
billboard_charts_pos <- billboard_charts_pos %>% mutate(across(everything(), as.integer))



#save the relevant variables:
billboard_charts_vars <- billboard_charts_relevant %>%
  dplyr::select(Date_Entered,
  Track,
  Artist,
`Label/Number`) %>%
  # change the names of the columns to match the key up top
  rename(name_recording = Track,
         name_artist_credit = Artist,
         label = `Label/Number`) %>%
  #add the country column
  mutate(country = "us_bb")

#columnbind the vars and pos dataset
billboard_relevant <- bind_cols(billboard_charts_vars, billboard_charts_pos)

#==============================================
# Billboard Charts pivot longer
#==============================================


# PIVOT: Transform from wide to long format
# NOTE: Billboard columns are "1st Week", "2nd Week", etc. (with "Week" suffix)
long_data_billboard <- billboard_relevant %>%
  pivot_longer(
    cols = matches("^\\d+(st|nd|rd|th) Week$"),  # Matches: 1st Week, 2nd Week, etc.
    names_to = "week_number",
    values_to = "chart_position",
    values_drop_na = TRUE  # KEY: Removes all NA values efficiently
  ) %>%
  # Extract week number and calculate chart week date
  mutate(
    # Extract just the number from "1st Week", "2nd Week", etc.
    week_num = parse_number(week_number),
    # Chart week = Date_Entered + (week number - 1) weeks
    chart_week = Date_Entered + weeks(week_num - 1)
  ) %>%
  # Clean up and reorder columns
  select(
    chart_week,
    chart_position,
    name_recording,
    name_artist_credit,
    label,
    country,
    Date_Entered,
    week_num,
    everything(),
    -week_number
  ) %>%
  arrange(chart_week, chart_position)

cat("=== POST-PIVOT DATA INFO ===\n")
cat("Total song-week entries:", nrow(long_data), "\n")
cat("Date range:", format(min(long_data$chart_week, na.rm = TRUE), "%Y-%m-%d"), 
    "to", format(max(long_data$chart_week, na.rm = TRUE), "%Y-%m-%d"), "\n\n")

#=============================================
# Saving the dataframes ------------------------------------------------------------
#=============================================

#cashbox to the shared drive
write.csv(long_data_cashbox, "//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/country_chart_data/us_CB_songs_1980_2000_weekly.csv")
#cashbox locally
write.csv(long_data_cashbox, here::here("data", "raw_data", "country_chart_data", "us_CB_songs_1980_2000_weekly.csv"))

# billboard to shared drive
write.csv(long_data_billboard, "//bigdata.wu.ac.at/delpero/Data_alexander/data/raw_data/country_chart_data/us_BB_songs_1980_2000_weekly.csv")
#billboard locally
write.csv(long_data_billboard, here::here("data", "raw_data", "country_chart_data", "us_BB_songs_1980_2000_weekly.csv"))



# ============================================
# Cashbox: DATA VALIDATION CHECKS --------------------------------------------------
# ============================================

cat("=== DATA VALIDATION ===\n\n")

# Check 1: Identify weeks with duplicate chart positions
duplicates <- long_data_cashbox %>%
  group_by(chart_week, chart_position) %>%
  filter(n() > 1) %>%
  arrange(chart_week, chart_position) %>%
  select(chart_week, chart_position, name_recording, name_artist_credit)

if (nrow(duplicates) > 0) {
  cat("⚠️  WARNING: Found", nrow(duplicates) / 2, "cases of duplicate positions:\n")
  print(duplicates, n = 20)
  cat("\n")
} else {
  cat("✓ No duplicate chart positions found\n")
}

# Check 2: Identify weeks with gaps in chart positions
# For each week, check if positions are consecutive starting from 1
gaps <- long_data_cashbox %>%
  group_by(chart_week) %>%
  summarise(
    positions = list(sort(unique(chart_position))),
    min_pos = min(chart_position),
    max_pos = max(chart_position),
    n_positions = n_distinct(chart_position),
    expected_positions = max_pos - min_pos + 1,
    has_gap = n_positions < expected_positions,
    .groups = "drop"
  ) %>%
  filter(has_gap)

if (nrow(gaps) > 0) {
  cat("⚠️  WARNING: Found", nrow(gaps), "weeks with missing chart positions (gaps):\n")
  print(gaps %>% select(chart_week, min_pos, max_pos, n_positions, expected_positions), n = 20)
  cat("\n")
} else {
  cat("✓ No gaps in chart positions found\n")
}

# Check 3: Chart completeness by week
weekly_summary <- long_data_cashbox %>%
  group_by(chart_week) %>%
  summarise(
    n_songs = n(),
    min_position = min(chart_position),
    max_position = max(chart_position),
    .groups = "drop"
  )

cat("\n=== WEEKLY CHART SUMMARY ===\n")
cat("Total unique weeks:", nrow(weekly_summary), "\n")
cat("Songs per week - Min:", min(weekly_summary$n_songs), 
    "Max:", max(weekly_summary$n_songs),
    "Mean:", round(mean(weekly_summary$n_songs), 1), "\n")
cat("Highest chart position range: 1 to", max(weekly_summary$max_position), "\n")

# Check 4: Songs with unusually long chart runs
long_runners <- long_data_cashbox %>%
  group_by(name_recording, name_artist_credit, Date.Entered) %>%
  summarise(
    weeks_on_chart = n(),
    peak_position = min(chart_position),
    .groups = "drop"
  ) %>%
  filter(weeks_on_chart > 30) %>%
  arrange(desc(weeks_on_chart))

if (nrow(long_runners) > 0) {
  cat("\n=== SONGS WITH 30+ WEEKS ON CHART ===\n")
  print(long_runners, n = 10)
}

# Check 5: Missing or invalid dates
missing_dates <- sum(is.na(long_data_cashbox$chart_week))
if (missing_dates > 0) {
  cat("\n⚠️  WARNING:", missing_dates, "rows have missing chart_week dates\n")
} else {
  cat("\n✓ All rows have valid chart_week dates\n")
}



# ============================================
# Billboard: DATA VALIDATION CHECKS --------------------------------------------------
# ============================================

# Check 1: Identify weeks with duplicate chart positions
duplicates <- long_data_billboard %>%
  group_by(chart_week, chart_position) %>%
  filter(n() > 1) %>%
  arrange(chart_week, chart_position) %>%
  select(chart_week, chart_position, name_recording, name_artist_credit)

if (nrow(duplicates) > 0) {
  cat("⚠️  WARNING: Found", nrow(duplicates) / 2, "cases of duplicate positions:\n")
  print(duplicates, n = 20)
  cat("\n")
}

# Check 2: Identify weeks with gaps in chart positions
# For each week, check if positions are consecutive
gaps <- long_data_billboard %>%
  group_by(chart_week) %>%
  summarise(
    positions = list(sort(unique(chart_position))),
    min_pos = min(chart_position),
    max_pos = max(chart_position),
    n_positions = n_distinct(chart_position),
    expected_positions = max_pos - min_pos + 1,
    has_gap = n_positions < expected_positions,
    .groups = "drop"
  ) %>%
  filter(has_gap)

if (nrow(gaps) > 0) {
  cat("⚠️  WARNING: Found", nrow(gaps), "weeks with missing chart positions (gaps):\n")
  print(gaps %>% select(chart_week, min_pos, max_pos, n_positions, expected_positions), n = 20)
  cat("\n")
}

# Check 3: Chart completeness by week
weekly_summary <- long_data_billboard %>%
  group_by(chart_week) %>%
  summarise(
    n_songs = n(),
    min_position = min(chart_position),
    max_position = max(chart_position),
    .groups = "drop"
  )

cat("\n=== WEEKLY CHART SUMMARY ===\n")
cat("Total unique weeks:", nrow(weekly_summary), "\n")
cat("Songs per week - Min:", min(weekly_summary$n_songs), 
    "Max:", max(weekly_summary$n_songs),
    "Mean:", round(mean(weekly_summary$n_songs), 1), "\n")
cat("Highest chart position range: 1 to", max(weekly_summary$max_position), "\n")

# Show some examples of weekly totals
cat("\nSample weekly song counts:\n")
print(head(weekly_summary %>% arrange(chart_week) %>% 
           select(chart_week, n_songs, min_position, max_position), 10))

# Check 4: Songs with unusually long chart runs
long_runners <- long_data_billboard %>%
  group_by(name_recording, name_artist_credit, Date_Entered) %>%
  summarise(
    weeks_on_chart = n(),
    peak_position = min(chart_position),
    .groups = "drop"
  ) %>%
  filter(weeks_on_chart > 40) %>%
  arrange(desc(weeks_on_chart))

if (nrow(long_runners) > 0) {
  cat("\n=== SONGS WITH 40+ WEEKS ON CHART ===\n")
  print(long_runners, n = 15)
}


  
