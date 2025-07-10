# -*- coding: utf-8 -*-
"""
Created on Thu Jul  3 18:46:34 2025

@author: User
"""

import pandas as pd
from pathlib import Path

# Define paths
base_dir = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists")
part1 = base_dir / "parts_charted_songs" / "spotify_track_info" / "charted_songs_1980_2000_full.csv"
part2 = base_dir / "parts_charted_songs" / "more_normalized" / "spotify_track_info" / "charted_songs_1980_2000_second_fetch_normalized.csv"
output_combined = base_dir / "all_charts_songs_1980_2000_after_second_fetch.csv"
original_full = base_dir / "all_charts_songs_1980_2000_complete_clean.csv"
output_missing = base_dir / "missing_after_second_fetch.csv"

# 1. Load and append the two files
print("Loading files...")
df1 = pd.read_csv(part1)
df2 = pd.read_csv(part2)

print(f"Rows in first file: {len(df1)}")
print(f"Rows in second file: {len(df2)}")

# 2. Combine and drop duplicate songs (if any), keeping all columns
df_combined = pd.concat([df1, df2], ignore_index=True)
# If you want to drop exact duplicates, uncomment the following line:
# df_combined = df_combined.drop_duplicates()

print(f"Combined rows: {len(df_combined)}")

# 3. Save the combined file
df_combined.to_csv(output_combined, index=False)
print(f"Combined file saved as: {output_combined}")

# 4. Load the original complete clean database
df_original = pd.read_csv(original_full)
print(f"Original complete has {len(df_original)} rows.")

# 5. Find missing entries
# Define the columns that uniquely identify a song (customize as needed!)
id_cols = ['name_recording', 'name_artist_credit']
df_combined_id = df_combined[id_cols].drop_duplicates()
df_original_id = df_original[id_cols].drop_duplicates()

# Merge to find which originals are missing from combined
missing = df_original_id.merge(
    df_combined_id, 
    how='left', 
    on=id_cols, 
    indicator=True
)
missing_only = missing[missing['_merge'] == 'left_only'][id_cols]

print(f"Missing songs after second fetch: {len(missing_only)}")

# Get full rows from original corresponding to missing
df_missing_full = df_original.merge(missing_only, on=id_cols, how='inner')

# 6. Save missing as CSV
df_missing_full.to_csv(output_missing, index=False)
print(f"Missing songs file saved as: {output_missing}")
