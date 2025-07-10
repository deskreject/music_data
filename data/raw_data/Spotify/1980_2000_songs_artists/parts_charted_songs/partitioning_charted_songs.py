# -*- coding: utf-8 -*-
"""
Created on Thu Jun 26 19:52:07 2025

@author: User
"""

import pandas as pd
from pathlib import Path
import os

# -----------------------------
# Paths
# -----------------------------
BASE_DIR = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists")
INPUT_CSV = BASE_DIR / "all_charts_songs_1980_2000.csv"
OUTPUT_DIR = BASE_DIR / "parts_charted_songs"
OUTPUT_DIR.mkdir(exist_ok=True)

# -----------------------------
# Settings
# -----------------------------
NUM_PARTS = 10

# -----------------------------
# Load CSV
# -----------------------------
print("📥 Reading full dataset into memory (if feasible)...")
df = pd.read_csv(INPUT_CSV)
total_rows = len(df)
rows_per_part = total_rows // NUM_PARTS
print(f"✅ Total rows: {total_rows}, rows per part: ~{rows_per_part}")

# -----------------------------
# Partition and Save
# -----------------------------
for i in range(NUM_PARTS):
    start = i * rows_per_part
    end = (i + 1) * rows_per_part if i < NUM_PARTS - 1 else total_rows
    part_df = df.iloc[start:end]
    part_path = OUTPUT_DIR / f"charted_songs_part{i+1}.csv"
    part_df.to_csv(part_path, index=False)
    print(f"📄 Saved part {i+1} with {len(part_df)} rows → {part_path.name}")

print("✅ All parts saved successfully.")
