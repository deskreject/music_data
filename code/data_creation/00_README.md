
# Data_preparation_sql_export

_2024: the code to combine release with songs based on sql exports of the musicbrainz data into panel data
_2024_scratch_book: code for sanity checks from the panel creation - exploratory

#Data_preparation_Mb_spotify_aom_2024 (11.12.2023):

- the prep script meant to combine my label level sql data + ADs spotify and mb exports for US and EU countries to a dataset ready for the AOM 2024 working paper
-- DiD on remix intesity
-- DiD on acoustic characteristics on label level


# Data_preparation_MB_vn scripts:

- Scripts that were created to look into all the MB exports from Alessio over time
- _v6 is one which includes ISRCs based on ALessios attempt to get US based releases during time from 1998-2005 

# chartmetric_api
_access_spotify_id_vm: using songs for which we have spotify characteristics, take their isrc code to request the chartmetric id - WU VM version
_access_spotify_id: same as above, only locally tested and developed. Different file paths
_access_trial: testing whether possible to get chartmetric ID from song names
_song_metadata_spotify_comp: using the chartmetric IDs gathered from isrcs to collect song metadata

## additional

chartmetric_api.log: log file for the chartmetric api gathering script

# data_preparation_sql_labels_v6:
- a script related to exporting the release_IDs from the label export by Alessio (including more than US countries), and getting label information relate dto the release IDs

# spotify_access:

- my attempts at getting a spotify API access script. Issue stopped at was that I get a hard request limit at 5000 requests
- Alessio managed to batch his requests in his script, which I haven't implemented in mine 


# path_speficiation.py:
- a helper script for my spotify access. Doesn't work well, might delete

# .py files:
- from Alessio; not up to date, i.e. ones where ISRCs were mismatched to spotify IDs due to loop issue

