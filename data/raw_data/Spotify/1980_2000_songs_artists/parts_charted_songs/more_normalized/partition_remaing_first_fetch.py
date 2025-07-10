# -*- coding: utf-8 -*-
"""
Created on Fri Jun 27 10:02:52 2025

@author: User
"""

import pandas as pd
from pathlib import Path
import os

# -----------------------------
# Set up paths
# -----------------------------
BASE_DIR = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists")
INPUT_FILE = BASE_DIR / "all_charts_songs_1980_2000_fetch_compared.csv"
OUTPUT_DIR = BASE_DIR / "parts_charted_songs" / "more_normalized"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# -----------------------------
# Step 1: Load input file
# -----------------------------
print(f"🔍 Loading: {INPUT_FILE}")
df = pd.read_csv(INPUT_FILE)

# -----------------------------
# Step 2: Filter entries not found on Spotify
# -----------------------------
df_not_found = df[df["fetched_from_spotify"] == 0].copy()
print(f"🧹 Found {len(df_not_found)} entries not matched on Spotify")

# -----------------------------
# Step 3: Save full not-found set
# -----------------------------
full_output_file = OUTPUT_DIR / "all_charts_songs_1980_2000_not_found.csv"
df_not_found.to_csv(full_output_file, index=False)
print(f"💾 Saved full not-found dataset to: {full_output_file}")

# -----------------------------
# Step 4: Partition into 6 parts
# -----------------------------
part_size = len(df_not_found) // 6
for i in range(6):
    start = i * part_size
    end = None if i == 5 else (i + 1) * part_size
    df_part = df_not_found.iloc[start:end].copy()
    part_file = OUTPUT_DIR / f"not_found_part{i+1}.csv"
    df_part.to_csv(part_file, index=False)
    print(f"📁 Saved part {i+1} with {len(df_part)} rows to {part_file}")
