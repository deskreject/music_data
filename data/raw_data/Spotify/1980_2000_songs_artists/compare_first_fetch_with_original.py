# -*- coding: utf-8 -*-
"""
Created on Fri Jun 27 08:31:09 2025

@author: User
"""

import pandas as pd
from pathlib import Path

# -----------------------------
# Paths
# -----------------------------
BASE_DIR = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists")
CHARTS_PATH = BASE_DIR / "all_charts_songs_1980_2000_complete_clean.csv"
SPOTIFY_FETCH_PATH = BASE_DIR / "parts_charted_songs" / "spotify_track_info" / "charted_songs_1980_2000_full.csv"
OUTPUT_PATH = BASE_DIR / "all_charts_songs_1980_2000_fetch_compared.csv"

# -----------------------------
# Load both datasets
# -----------------------------
print("🔄 Loading chart dataset...")
df_chart = pd.read_csv(CHARTS_PATH)

print("🔄 Loading fetched Spotify dataset...")
df_spotify = pd.read_csv(SPOTIFY_FETCH_PATH, usecols=["name_recording", "name_artist_credit"])

# -----------------------------
# Normalize for comparison
# -----------------------------
df_chart["rec_norm"] = df_chart["name_recording"].astype(str).str.lower().str.strip()
df_chart["art_norm"] = df_chart["name_artist_credit"].astype(str).str.lower().str.strip()

df_spotify["rec_norm"] = df_spotify["name_recording"].astype(str).str.lower().str.strip()
df_spotify["art_norm"] = df_spotify["name_artist_credit"].astype(str).str.lower().str.strip()

# -----------------------------
# Create set of normalized tuples from Spotify fetch
# -----------------------------
spotify_keys = set(zip(df_spotify["rec_norm"], df_spotify["art_norm"]))

# -----------------------------
# Mark matches in the chart dataset
# -----------------------------
df_chart["fetched_from_spotify"] = df_chart.apply(
    lambda row: 1 if (row["rec_norm"], row["art_norm"]) in spotify_keys else 0,
    axis=1
)

# -----------------------------
# Save the output
# -----------------------------
df_chart.drop(columns=["rec_norm", "art_norm"], inplace=True)
df_chart.to_csv(OUTPUT_PATH, index=False)

print(f"✅ Comparison complete. Output saved to: {OUTPUT_PATH}")
