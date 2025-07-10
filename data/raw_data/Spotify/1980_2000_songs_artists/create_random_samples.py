'''
author: Alexander Staub
date: 2025-07-10
purpose: create random samples of 5000 songs from:
- the final_charted_from_spotify_1980_2000.csv
- missing_after_after_fourth_fetch

and save them to:
/parts_charted_songs/fifth_fetch
'''

#package imports
import pandas as pd
import numpy as np

#load the data in dataframes
found_songs = pd.read_csv('data/raw_data/Spotify/1980_2000_songs_artists/final_charted_from_spotify_1980_2000.csv')
missing_songs = pd.read_csv('data/raw_data/Spotify/1980_2000_songs_artists/missing_after_fourth_fetch.csv')

#sample 5000 songs from the found songs
sampled_found_songs = found_songs.sample(n=5000, random_state=42)
#sample 5000 songs from the missing songs
sampled_missing_songs = missing_songs.sample(n=5000, random_state=42)

#save the sampled dataframes to csv files
sampled_found_songs.to_csv('data/raw_data/Spotify/1980_2000_songs_artists/parts_charted_songs/fifth_fetch/final_charted_from_spotify_1980_2000_sampled.csv', index=False)
sampled_missing_songs.to_csv('data/raw_data/Spotify/1980_2000_songs_artists/parts_charted_songs/fifth_fetch/missing_after_after_fourth_fetch_sampled.csv', index=False)

