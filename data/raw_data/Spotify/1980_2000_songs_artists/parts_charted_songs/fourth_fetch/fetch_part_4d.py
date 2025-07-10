# -*- coding: utf-8 -*-
"""
Created on Thu Jun 26 19:59:59 2025

@author: User
"""

import re
import os
import time
import pickle
import requests
import pandas as pd
from spotipy import Spotify
from spotipy.oauth2 import SpotifyClientCredentials
from pathlib import Path
import unicodedata

# ---------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------
SPOTIFY_CLIENT_ID = "7c527b6782d14fcd9774d61b5b246cb3"
SPOTIFY_CLIENT_SECRET = "5dfb579c75f4420e8d951b396ee892d8"

INPUT_DIR = Path(r"Z:\\Data_alexander\\data\\raw_data\\Spotify\\1980_2000_songs_artists\\parts_charted_songs\\more_normalized\\third_fetch\\fourth_fetch")
OUTPUT_DIR = INPUT_DIR / "spotify_track_info"
OUTPUT_DIR.mkdir(exist_ok=True)

CHUNK_SIZE = 500
PRINT_EVERY = 50

MARKETS = ['US', 'GB', 'DE', 'FR', 'IT']

# ---------------------------------------------------------------
# NORMALIZATION
# ---------------------------------------------------------------
def normalize(text):
    if not isinstance(text, str):
        return ""
    text = text.lower().strip()
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("utf-8")
    text = re.sub(r"\s+", " ", text)
    return text

# ---------------------------------------------------------------
# SPOTIFY SETUP
# ---------------------------------------------------------------
client_credentials_manager = SpotifyClientCredentials(
    client_id=SPOTIFY_CLIENT_ID,
    client_secret=SPOTIFY_CLIENT_SECRET
)
sp = Spotify(client_credentials_manager=client_credentials_manager, requests_timeout=30)

# ---------------------------------------------------------------
# STRICT SEARCH FUNCTION, MULTI-MARKET
# ---------------------------------------------------------------
def search_track_all_markets(artist_name, track_name, max_retries=5):
    query_strict = f'track:"{track_name}" artist:"{artist_name}"'
    wait_seconds = 5

    for attempt in range(max_retries):
        try:
            for market in MARKETS:
                result = sp.search(q=query_strict, type="track", limit=1, market=market)
                items = result.get("tracks", {}).get("items", [])
                if items:
                    info = extract_track_info(items[0])
                    info['spotify_market_found'] = market
                    return info
            return None
        except requests.exceptions.RequestException as e:
            print(f"[Network Error] {e}. Retrying in {wait_seconds}s...")
            time.sleep(wait_seconds)
            wait_seconds *= 2
        except Exception as e:
            print(f"[Spotify Error] {e}")
            return None
    return None

def extract_track_info(track):
    return {
        "spotify_track_id": track.get("id"),
        "spotify_track_title": track.get("name"),
        "spotify_artist_name": ", ".join(a.get("name") for a in track.get("artists", [])),
        "spotify_album_name": track.get("album", {}).get("name"),
        "release_date": track.get("album", {}).get("release_date"),
        "spotify_url": track.get("external_urls", {}).get("spotify"),
        "spotify_isrc": track.get("external_ids", {}).get("isrc")
    }

# ---------------------------------------------------------------
# MAIN PROCESSING
# ---------------------------------------------------------------
def process_csv_file(file_path):
    part_label = file_path.stem
    print(f"\n🚀 Starting processing of {part_label}")

    df_part = pd.read_csv(file_path)
    print(f"▶ Loaded {len(df_part)} rows from {file_path.name}")

    cache_path = OUTPUT_DIR / f"{part_label}_track_cache.pkl"
    sample_path = OUTPUT_DIR / f"{part_label}_sample_500.csv"
    log_path = OUTPUT_DIR / f"{part_label}_track_log.txt"
    output_path = OUTPUT_DIR / f"{part_label}_chunked_output"
    os.makedirs(output_path, exist_ok=True)

    cache = {}
    if os.path.exists(cache_path):
        with open(cache_path, "rb") as f:
            cache = pickle.load(f)

    sample_rows = []
    processed = 0
    found_total = 0
    not_found_total = 0
    chunk_id = 0
    sample_written = False

    for start in range(0, len(df_part), CHUNK_SIZE):
        df_chunk = df_part.iloc[start:start + CHUNK_SIZE].copy()
        rows = []

        for i, row in df_chunk.iterrows():
            artist = str(row["name_artist_credit"]).strip()
            track = str(row["name_recording"]).strip()

            if not artist or not track:
                continue

            key = (track, artist)
            if key in cache:
                track_info = cache[key]
            else:
                track_info = search_track_all_markets(artist, track)
                cache[key] = track_info

            if track_info:
                enriched_row = {**row.to_dict(), **track_info}
                rows.append(enriched_row)
                found_total += 1

                if len(sample_rows) < 500:
                    sample_rows.append(enriched_row)
                    if len(sample_rows) == 500 and not sample_written:
                        pd.DataFrame(sample_rows).to_csv(sample_path, index=False)
                        print(f"✅ Sample CSV saved early at {sample_path.name}")
                        sample_written = True
            else:
                not_found_total += 1

            processed += 1
            if processed % PRINT_EVERY == 0:
                print(f"[{part_label}, Row {processed}] Found: {found_total}, Not found: {not_found_total}")

        if rows:
            df_out = pd.DataFrame(rows)
            df_out.to_parquet(output_path / f"{part_label}_chunk_{chunk_id}.parquet", index=False)

        with open(cache_path, "wb") as f:
            pickle.dump(cache, f)

        with open(log_path, "a") as log:
            log.write(f"{part_label} chunk {chunk_id}: Found {found_total}, Not Found {not_found_total}\n")

        chunk_id += 1

    print(f"✅ Finished processing {part_label}")
    if not sample_written and sample_rows:
        pd.DataFrame(sample_rows).to_csv(sample_path, index=False)
        print(f"✅ Final sample CSV written to {sample_path.name}")

# ---------------------------------------------------------------
# RUN SCRIPT FOR A GIVEN FILE
# ---------------------------------------------------------------
if __name__ == "__main__":
    file = INPUT_DIR / "missing_after_third_fetch_part4.csv"  # Change for each part/VM!
    process_csv_file(file)
