
"""
Created on Wed Nov 15 17:40:00 2023

@author: Alexander Staub

This script is used to verify the Spotify IDs that were obtained by ISRCs
Currently, there are multiple (100+) Spotify IDs associated with one ISRC
"""

#library loading

import os
import spotipy # lightweight Python library for the Spotify Web API
import pandas as pd
from spotipy.oauth2 import SpotifyClientCredentials # to access authorised Spotify data
import json # to read json files
import time # to time the code
import requests # to make http requests

# path specification - using os

from path_specification import root_dir

# get the client id and client secret for my account
import sys

# Assuming root_dir contains the path to your project's root directory
#sys.path.append(f"{root_dir}/code/src")

print("Root Directory:", root_dir) # debugging path

credentials_path = os.path.join(root_dir, 'code', 'data_creation', 'spotify_credentials.json')

print("Credentials Path:", credentials_path) # debugging path

# get the client id and client secret for my account

# access the spotify credentials from the json file and store it as variable "f"
with open(credentials_path) as f:

    # transform the json file into a dictionary
    data = json.load(f)
    client_id = data['SPOTIPY_CLIENT_ID']
    client_secret = data['SPOTIPY_CLIENT_SECRET']

if not client_id or not client_secret:
    raise ValueError("Spotify API credentials not found in environment variables")

sp = spotipy.Spotify(auth_manager=SpotifyClientCredentials(client_id=client_id, client_secret=client_secret))

# Create the path to the CSV file
csv_file_path = os.path.join(root_dir, 'data', 'incidental', 'spotify_related', 'random_sample_ISRC_URI.csv')

print("CSV File Path:" , csv_file_path) # debugging path

#detect the encoding of the csv

import chardet    

rawdata = open(csv_file_path, "rb").read()
result = chardet.detect(rawdata)
charenc = result['encoding']

# Now read the CSV into a DataFrame
sample_isrc_spotID = pd.read_csv(csv_file_path, encoding=charenc)

# Define the function to handle rate limiting and get track details
def get_track_details(spotify_id):
    try:
        track_info = sp.track(spotify_id)
        # Extract artist name and track name
        artist_name = track_info['artists'][0]['name']
        track_name = track_info['name']
        return artist_name, track_name
    except spotipy.SpotifyException as e:
        if e.http_status == 429:
            wait_time = int(e.headers['Retry-After'])
            print(f"Rate limit exceeded, sleeping for {wait_time} seconds")
            time.sleep(wait_time)
            return get_track_details(spotify_id)  # retry fetching track details after waiting
        else:
            raise e

# Loop over the Spotify IDs in the dataframe and get the artist and song title
artists = []
track_titles = []
for spotify_id in sample_isrc_spotID['spotify_ID']:
    artist, track_title = get_track_details(spotify_id)
    artists.append(artist)
    track_titles.append(track_title)

# Append the artist and track title to the original dataframe
sample_isrc_spotID['artist'] = artists
sample_isrc_spotID['song_title'] = track_titles

# Specify the output file path
output_file_name = "sample_isrc_spotID_artist_songtitle.csv"
output_file_path = os.path.join(root_dir, 'data', 'incidental', 'spotify_related', output_file_name)

# Save the dataframe to a CSV file
sample_isrc_spotID.to_csv(output_file_path, index=False)

print(f"Updated dataframe saved to {output_file_path}")