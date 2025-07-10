# -*- coding: utf-8 -*-
"""
Created on Tue Jun 24 17:09:32 2025

@author: User
"""
import pandas as pd
import pyarrow.parquet as pq
from rapidfuzz import fuzz
import re
from pathlib import Path

# -----------------------------
# Paths
# -----------------------------
BASE_DIR = Path(r"Z:\Data_alexander\data\raw_data\Spotify\1980_2000_songs_artists")
charts_csv = BASE_DIR / "all_charts_songs_1980_2000.csv"
musicbrainz_parquet = BASE_DIR / "musicbrainz_spotify_combined_track_artist_final.parquet"
output_csv = BASE_DIR / "all_charts_songs_1980_2000_compared.csv"
summary_txt = BASE_DIR / "all_charts_songs_1980_2000_comparison_summary.txt"

# -----------------------------
# Normalization Function
# -----------------------------
def normalize(text):
    if not isinstance(text, str):
        return ""
    return re.sub(r"[^a-z0-9]", "", text.lower())

# -----------------------------
# Step 1: Load and normalize chart file
# -----------------------------
df_chart = pd.read_csv(charts_csv)
df_chart["normalized_name_recording"] = df_chart["name_recording"].apply(normalize)
df_chart["normalized_name_artist_credit"] = df_chart["name_artist_credit"].apply(normalize)

# -----------------------------
# Step 2: Scan large parquet in chunks and extract normalized keys
# -----------------------------
pf = pq.ParquetFile(musicbrainz_parquet)
mb_keys_name = set()
mb_keys_spotify = set()

for i in range(pf.num_row_groups):
    table = pf.read_row_group(i, columns=[
        "name_recording", "name_artist_credit",
        "spotify_track_title", "spotify_artist_name"
    ])
    df = table.to_pandas()
    for _, row in df.iterrows():
        norm_mb_rec = normalize(row.get("name_recording"))
        norm_mb_art = normalize(row.get("name_artist_credit"))
        norm_spot_rec = normalize(row.get("spotify_track_title"))
        norm_spot_art = normalize(row.get("spotify_artist_name"))
        mb_keys_name.add((norm_mb_rec, norm_mb_art))
        mb_keys_spotify.add((norm_spot_rec, norm_spot_art))

# -----------------------------
# Step 3: Compare with conditional fuzzy match
# -----------------------------
match_strict_mb = []
match_fuzzy_mb = []
match_strict_spotify = []
match_fuzzy_spotify = []

for _, row in df_chart.iterrows():
    chart_key = (row["normalized_name_recording"], row["normalized_name_artist_credit"])

    strict_mb = chart_key in mb_keys_name
    strict_spotify = chart_key in mb_keys_spotify

    match_strict_mb.append(1 if strict_mb else 0)
    match_strict_spotify.append(1 if strict_spotify else 0)

    # Fuzzy match only if strict fails
    fuzzy_mb = 0
    fuzzy_spotify = 0
    if not strict_mb:
        fuzzy_mb = max(
            (fuzz.token_sort_ratio(" ".join(chart_key), " ".join(k)) for k in mb_keys_name),
            default=0
        )
    if not strict_spotify:
        fuzzy_spotify = max(
            (fuzz.token_sort_ratio(" ".join(chart_key), " ".join(k)) for k in mb_keys_spotify),
            default=0
        )

    match_fuzzy_mb.append(1 if fuzzy_mb >= 90 else 0)
    match_fuzzy_spotify.append(1 if fuzzy_spotify >= 90 else 0)

# -----------------------------
# Step 4: Add matches to DataFrame
# -----------------------------
df_chart["match_strict_mb"] = match_strict_mb
df_chart["match_fuzzy_mb"] = match_fuzzy_mb
df_chart["match_strict_spotify"] = match_strict_spotify
df_chart["match_fuzzy_spotify"] = match_fuzzy_spotify

# For visual reference, duplicate normalized keys (as placeholders)
df_chart["norm_mb_recording"] = df_chart["normalized_name_recording"]
df_chart["norm_mb_artist"] = df_chart["normalized_name_artist_credit"]
df_chart["normalized_spotify_track_title"] = df_chart["normalized_name_recording"]
df_chart["normalized_spotify_artist_name"] = df_chart["normalized_name_artist_credit"]

# -----------------------------
# Step 5: Save results
# -----------------------------
df_chart.to_csv(output_csv, index=False)

with open(summary_txt, "w") as f:
    f.write(f"Total chart entries: {len(df_chart)}\n")
    f.write(f"Strict matches on name_recording + name_artist_credit: {sum(match_strict_mb)}\n")
    f.write(f"Fuzzy matches on name_recording + name_artist_credit: {sum(match_fuzzy_mb)}\n")
    f.write(f"Strict matches on spotify_track_title + spotify_artist_name: {sum(match_strict_spotify)}\n")
    f.write(f"Fuzzy matches on spotify_track_title + spotify_artist_name: {sum(match_fuzzy_spotify)}\n")

print("✅ Done: Matches compared and saved.")
