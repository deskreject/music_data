"""
the python code to access python credentials that are located in a seperate folder
"""

#packages to load the credentials
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import json
import os

# Assuming root_dir contains the path to your project's root directory
json_path = "../data_creation/spotify_credentials.json"


# access the spotify credentials from the json file and store it as variable "f"
with open(json_path) as f:

    # transform the json file into a dictionary
    data = json.load(f)
    client_id = data['SPOTIPY_CLIENT_ID']
    client_secret = data['SPOTIPY_CLIENT_SECRET']

if not client_id or not client_secret:
    raise ValueError("Spotify API credentials not found in environment variables")

sp = spotipy.Spotify(auth_manager=SpotifyClientCredentials(client_id=client_id, client_secret=client_secret))