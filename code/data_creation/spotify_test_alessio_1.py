# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 12:47:04 2023

@author: User
"""
#(client_id="c103dcb8fe024630b197517568df9d40", client_secret="0a456e9ecb73410c8a6317068f9515b6")

import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import time
import pandas as pd
import os
import json

# Open the file and set up Spotify API client
with open(r'C:\Users\User\Desktop\songs_ISRCs\unique_isrcs.json', 'r') as f:
    unique_isrcs = json.load(f)

#print(unique_isrcs)

client_credentials_manager = SpotifyClientCredentials(client_id="c103dcb8fe024630b197517568df9d40", client_secret="0a456e9ecb73410c8a6317068f9515b6")
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

def handle_rate_limit(func, *args, **kwargs):
    while True:
        try:
            return func(*args, **kwargs)
        except spotipy.exceptions.SpotifyException as e:
            if e.http_status == 429:
                wait_time = int(e.headers['Retry-After'])
                print(f"Rate limit exceeded in function, sleeping for {wait_time} seconds")  # Print in the function
                time.sleep(wait_time)
            else:
                raise

def get_spotify_id_from_isrc(isrc):
    results = handle_rate_limit(sp.search, q='isrc:' + isrc, type='track')
    tracks = results.get('tracks', {}).get('items', [])
    return tracks[0]['id'] if tracks else None

def get_track_features_batch(spotify_ids):
    return handle_rate_limit(sp.audio_features, spotify_ids)

spotify_ids = []
data = []

for i, isrc in enumerate(unique_isrcs):
    try:
        spotify_id = get_spotify_id_from_isrc(isrc)
    except spotipy.exceptions.SpotifyException as e:
        if e.http_status == 429:
            wait_time = int(e.headers['Retry-After'])
            print(f"Rate limit exceeded in loop for ISRC {isrc}, sleeping for {wait_time} seconds")  # Print in the loop
            time.sleep(wait_time)
            spotify_id = get_spotify_id_from_isrc(isrc)
        else:
            raise

    if spotify_id:
        spotify_ids.append(spotify_id)
        if len(spotify_ids) % 100 == 0:
            characteristics = get_track_features_batch(spotify_ids)
            for j in range(len(spotify_ids)):
                data.append([isrc, spotify_ids[j]] + list(characteristics[j].values()))
            spotify_ids = []
            time.sleep(0.1)

    if (i + 1) % 100 == 0:  
        print(f"Fetched {i + 1} songs")

    # Save to JSON every 1000 songs retrieved
    if (i + 1) % 1000 == 0:
        df = pd.DataFrame(data, columns=['ISRC', 'Spotify ID'] + list(characteristics[0].keys()))
        df.to_json(os.path.join(r'C:\Users\User\Desktop\songs_ISRCs', f'spotify_info_{i+1}.json'), orient='records')
        data = []

if spotify_ids:
    characteristics = get_track_features_batch(spotify_ids)
    for i in range(len(spotify_ids)):
        data.append([unique_isrcs[-len(spotify_ids)+i], spotify_ids[i]] + list(characteristics[i].values()))

if data:
    df = pd.DataFrame(data, columns=['ISRC', 'Spotify ID'] + list(characteristics[0].keys()))
    df.to_json(os.path.join(r'C:\Users\User\Desktop\songs_ISRCs', f'spotify_info_final.json'), orient='records')
    

print("Data fetching completed and saved to JSON files")
