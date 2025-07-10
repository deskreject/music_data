# -*- coding: utf-8 -*-
"""
Created on Fri Jun 27 08:21:36 2025

@author: User
"""

import os
import pandas as pd
from pathlib import Path

# -----------------------------
# CONFIGURATION
# -----------------------------
PARTS_DIR = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists\parts_charted_songs\more_normalized\third_fetch\spotify_track_info")
CSV_OUTPUT = PARTS_DIR / "charted_songs_1980_2000_third_fetch.csv"
PARQUET_OUTPUT = PARTS_DIR / "charted_songs_1980_2000_third_fetch.parquet"

# -----------------------------
# Step 1: Recursively gather all .parquet files from *_chunked_output folders
# -----------------------------
all_dfs = []
parquet_files = sorted(PARTS_DIR.rglob("*_chunked_output/*.parquet"))

print(f"🔍 Found {len(parquet_files)} parquet files to merge.")

for idx, file in enumerate(parquet_files, 1):
    try:
        print(f"📦 Reading file {idx}/{len(parquet_files)}: {file.name}")
        df = pd.read_parquet(file)
        all_dfs.append(df)
    except Exception as e:
        print(f"⚠️ Skipping {file.name} due to error: {e}")

# -----------------------------
# Step 2: Concatenate and save the full dataset
# -----------------------------
if all_dfs:
    print("🧩 Concatenating all DataFrames...")
    df_full = pd.concat(all_dfs, ignore_index=True)

    print(f"💾 Saving full dataset to CSV: {CSV_OUTPUT}")
    df_full.to_csv(CSV_OUTPUT, index=False)

    print(f"💾 Saving full dataset to Parquet: {PARQUET_OUTPUT}")
    df_full.to_parquet(PARQUET_OUTPUT, index=False, engine="pyarrow", compression="snappy")

    print("✅ Merge complete and files saved.")
else:
    print("❌ No parquet files found in *_chunked_output folders.")

