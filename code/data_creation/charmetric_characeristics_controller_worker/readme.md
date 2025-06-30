# Guide on how this characteristic access works

This folder contains scripts that are meant to hit the chartmetrics api simultaneously, then merge the separate "checkpoint" files

## STEP 1

I need to run the 00_chartmetric controller script once
- CAREFUL: check that I am loading the correct dataset (sample vs full)

## STEP 2

Open up 3 anaconda terminals
In each:
- set to the "music_data_chartmetric" environment
- set the working directory to this directory by "cd C:\Users\User\Documents\R_Work\research\music_data\code\data_creation\charmetric_characeristics_controller_worker"

in the separate anaconda prompts
- run "jupyter execute worker_1_chartmetric_api_song_metadata.ipynb"
- run "jupyter execute worker_2_chartmetric_api_song_metadata.ipynb"
- run "jupyter execute worker_3_chartmetric_api_song_metadata.ipynb"

The log files are saved in the working directory by worker
- I can delete the log files after I am done with one run to reset it

## STEP 3 

run the script "99_" to merge the data
